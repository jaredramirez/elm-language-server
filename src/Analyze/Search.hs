{-# LANGUAGE OverloadedStrings #-}

module Analyze.Search
  ( hover
  , HoverResult(..)
  , getInfo
  , Location(..)
  , Value(..)
  , Type(..)
  ) where


import AST.Canonical (Module)
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified LSP.Misc
import Misc (andThen, (|>), (<|))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Render.Type as RenderType
import Task (Task)
import qualified Task


-- Hover


data HoverResult
  = HoverType Text
  | HoverReference ModuleName.Canonical N.Name
  | HoverDebug String


hover :: Module -> L.Localizer -> Int -> Int -> Task HoverResult
hover modul localizer line column =
  let
      location =
        Location line column

      maybeInfo =
        getInfo modul location
  in
  case maybeInfo of
    Nothing ->
      Task.throw "Nothing Found"

    Just info ->
      case info of
        Raw name ->
          -- HoverType (N.toText name)
          HoverDebug "Not quite right"
            |> return

        InferredArg functionName argNumber variableName ->
          do
            annotations <- LSP.Misc.getAnnotations modul
            let maybeArgType =
                  annotations
                    |> Map.lookup functionName
                    |> andThen
                        (\(Can.Forall _ tipe) ->
                          getArgAtPosition argNumber tipe
                        )
            case maybeArgType of
              Nothing ->
                Task.throw "Arg not found"

              Just result ->
                HoverType (typeToText result)
                  |> return

        Arg variableName tipe ->
          HoverType (typeToText tipe)
            |> return

        Reference canonical name ->
          HoverReference canonical name
            |> return

        Type (Primitive name) ->
          HoverType (N.toText name)
            |> return

        Type (Definition tipe) ->
          RenderType.canToDoc localizer RenderType.None tipe
            |> D.toString
            |> Text.pack
            |> HoverType
            |> return

        Debug message ->
          HoverDebug message
            |> return


-- Base search


data Value
  = Raw N.Name
  | InferredArg N.Name Int N.Name
  | Arg N.Name Can.Type
  | Reference ModuleName.Canonical N.Name
  | Type Type
  | Debug String


data Type
  = Primitive N.Name
  | Definition Can.Type


data Location
  = Location {_row :: Int, _col :: Int}


getInfo :: Module -> Location -> Maybe Value
getInfo modul location =
  modul
    |> Can._decls
    |> foldlDelcs
        (\declaration maybeFound ->
          maybeFound
            |> ifNothingThen (searchDefinition location declaration)
        )
        (\_recDeclarations maybeFound -> maybeFound)
        Nothing


foldlDelcs :: (Can.Def -> result -> result) -> ([Can.Def] -> result -> result) -> result -> Can.Decls -> result
foldlDelcs declareFunc recDeclareFunc acc decls =
  case decls of
    Can.Declare def subDecls ->
      foldlDelcs declareFunc recDeclareFunc (declareFunc def acc) subDecls

    Can.DeclareRec defs subDecls ->
      foldlDelcs declareFunc recDeclareFunc (recDeclareFunc defs acc) subDecls

    Can.SaveTheEnvironment  ->
      acc


searchDefinitions :: Location -> [Can.Def] -> Maybe Value
searchDefinitions location definitions =
  List.foldl
    (\maybeFound definition ->
      maybeFound |> ifNothingThen (searchDefinition location definition)
    )
    Nothing
    definitions


searchDefinition :: Location -> Can.Def ->  Maybe Value
searchDefinition location definition =
  case definition of
    Can.Def locatedDefinition patterns expression ->
      let
          thisDefinition =
            locatedDefinition
              |> isWithin location
              |> fmap Raw

          thisPatterns =
            searchPatterns location patterns

          thisExpression =
            searchExpression location expression
              |> andThen
                  -- If we find a raw value, then check the function arguements to
                  -- see if the raw value matches any
                  (\found ->
                    case found of
                      Raw name ->
                        patterns
                          |> List.zip [1..]
                          |> List.foldr
                              (\(index, curPattern) acc ->
                                case acc of
                                  Nothing ->
                                    case unboxLocated curPattern of
                                      Can.PVar argName ->
                                        if argName == name then
                                          Just (InferredArg (unboxLocated locatedDefinition) index name)

                                        else
                                          acc

                                      -- TODO: Search inside other patterns for matching
                                      -- names

                                      _ ->
                                        acc

                                  Just _ ->
                                    acc
                              )
                              Nothing
                          |> ifNothingThen (Just found)

                      _ ->
                        Just found
                  )
      in
      thisDefinition
        |> ifNothingThen thisPatterns
        |> ifNothingThen thisExpression

    Can.TypedDef locatedDefinition _typeVariables typedPatterns expression resultType ->
      let
          thisDefinition =
            locatedDefinition
              |> isWithin location
              |> fmap Raw

          thisPatterns =
            typedPatterns
              |> List.map fst
              |> searchPatterns location

          thisExpression =
            searchExpression location expression
              |> andThen
                  (\found ->
                    case found of
                      Raw name ->
                        typedPatterns
                          |> List.foldr
                              (\(curPattern, curType) acc ->
                                case acc of
                                  Nothing ->
                                    case unboxLocated curPattern of
                                      Can.PVar argName ->
                                        if argName == name then
                                          Just (Arg name curType)

                                        else
                                          acc

                                      -- TODO: Search inside other patterns for matching
                                      -- names

                                      _ ->
                                        acc

                                  Just _ ->
                                    acc
                              )
                              Nothing
                          |> ifNothingThen (Just found)

                      _ ->
                        Just found
                  )
      in
      thisDefinition
        |> ifNothingThen thisPatterns
        |> ifNothingThen thisExpression


searchPatterns :: Location -> [Can.Pattern] -> Maybe Value
searchPatterns location patterns =
  List.foldl
    (\maybeFound pattern ->
      maybeFound |> ifNothingThen (searchPattern location pattern)
    )
    Nothing
    patterns


searchPattern :: Location -> Can.Pattern -> Maybe Value
searchPattern location locatedPattern =
  locatedPattern
    |> isWithin location
    |> andThen
        (\pattern ->
          case pattern of
            Can.PAlias locatedSubPattern name ->
              searchPattern location locatedSubPattern
                |> ifNothingThen (Just <| Raw name)

            Can.PTuple locatedPatternA locatedPatternB maybeLocatedWrapper ->
              searchPattern location locatedPatternA
                |> ifNothingThen (searchPattern location locatedPatternB)
                |> ifNothingThen (maybeLocatedWrapper |> andThen (searchPattern location))

            Can.PList locatedPatterns ->
              searchPatterns location locatedPatterns

            Can.PCons locatedPatternA locatedPatternB ->
              searchPattern location locatedPatternA
                |> ifNothingThen (searchPattern location locatedPatternB)

            Can.PCtor {Can._p_home = canoncial, Can._p_type = typeName} ->
              Just (Reference canoncial typeName)

            _ ->
              Nothing
        )


searchExpressions :: Location -> [Can.Expr] -> Maybe Value
searchExpressions location expressions =
  List.foldl
    (\maybeFound expression ->
      maybeFound |> ifNothingThen (searchExpression location expression)
    )
    Nothing
    expressions


searchExpression :: Location -> Can.Expr -> Maybe Value
searchExpression location locatedExpression =
  let
      searchSubExpression =
        searchExpression location

      searchSubExpressions =
        searchExpressions location
  in
  locatedExpression
    |> isWithin location
    |> andThen
        (\expression ->
          case expression of
            Can.VarLocal name ->
              Just <| Raw name

            Can.VarTopLevel canoncial name ->
              Just <| Reference canoncial name

            Can.VarForeign canoncial name _ ->
              Just <| Reference canoncial name

            Can.VarCtor _ canoncial name _ _ ->
              Just <| Reference canoncial name

            Can.VarDebug canoncial name _ ->
              Just <| Reference canoncial name

            Can.VarOperator name canoncial _ _ ->
              Just <| Reference canoncial name

            Can.Chr _ ->
              N.fromText "Char"
                |> Primitive
                |> Type
                |> Just

            Can.Str _ ->
              N.fromText "String"
                |> Primitive
                |> Type
                |> Just

            Can.Float _ ->
              N.fromText "Float"
                |> Primitive
                |> Type
                |> Just

            Can.List locatedExpresions ->
              searchSubExpressions locatedExpresions

            Can.Negate subExpression ->
              searchSubExpression subExpression

            Can.Binop binopName canoncial _ _ subExpressionA subExpressionB ->
              searchSubExpression subExpressionA
                |> ifNothingThen (searchSubExpression subExpressionB)
                |> ifNothingThen (Just <| Reference canoncial binopName)

            Can.Lambda subPatterns expression ->
              searchSubExpression expression
                |> ifNothingThen (searchPatterns location subPatterns)

            Can.Call function args ->
              searchSubExpression function
                |> ifNothingThen (searchSubExpressions args)

            Can.If expressions result ->
              searchSubExpression result
                |> ifNothingThen
                    (List.foldl
                      (\maybeFound (expressionA, expressionB) ->
                        maybeFound
                          |> ifNothingThen (searchSubExpression expressionA)
                          |> ifNothingThen (searchSubExpression expressionB)
                      )
                      Nothing
                      expressions
                    )

            Can.Let definition subExpression ->
              searchSubExpression subExpression
                |> ifNothingThen (searchDefinition location definition)

            Can.LetRec definitions subExpression ->
              searchSubExpression subExpression
                |> ifNothingThen (searchDefinitions location definitions)

            Can.LetDestruct pattern subExpressionA subExpressionB ->
              searchPattern location pattern
                |> ifNothingThen (searchSubExpression subExpressionA)
                |> ifNothingThen (searchSubExpression subExpressionB)

            Can.Case subExpression caseBranches ->
              searchSubExpression subExpression
                |> ifNothingThen (searchCaseBranches location caseBranches)

            Can.Accessor name ->
              Just (Raw name)

            Can.Access expresion locatedName ->
              locatedName
                |> isWithin location
                |> fmap Raw
                |> ifNothingThen (searchSubExpression expresion)

            Can.Update name expression fieldUpdates ->
              searchSubExpression expression
                |> ifNothingThen
                    (fieldUpdates
                      |> Map.toList
                      |> List.foldl
                          (\maybeFound (_ , Can.FieldUpdate region expression) ->
                            maybeFound
                              |> ifNothingThen
                                (if location |> isWithinRegion region then
                                  searchSubExpression expression

                                 else
                                  Nothing
                                )
                          )
                          Nothing

                    )
                |> ifNothingThen (Just <| Raw name)

            Can.Record map ->
              map
                |> Map.toList
                |> List.foldl
                    (\maybeFound (_ , expression) ->
                      maybeFound
                        |> ifNothingThen (searchExpression location expression)
                    )
                    Nothing

            Can.Unit ->
              N.fromText "()"
                |> Primitive
                |> Type
                |> Just

            Can.Tuple subExpressionA subExpressionB wrapperExpression->
              searchSubExpression subExpressionA
                |> ifNothingThen (searchSubExpression subExpressionB)
                |> ifNothingThen (wrapperExpression |> andThen searchSubExpression)

            _ ->
              Nothing
        )


searchCaseBranches :: Location -> [Can.CaseBranch] -> Maybe Value
searchCaseBranches location caseBranches =
  List.foldl
    (\maybeFound (Can.CaseBranch pattern expression) ->
      maybeFound
        |> ifNothingThen (searchPattern location pattern)
        |> ifNothingThen (searchExpression location expression)
    )
    Nothing
    caseBranches


-- Arguements flipped for pipeline operator
ifNothingThen ::  Maybe value -> Maybe value -> Maybe value
ifNothingThen nextMaybe curMaybe =
  case curMaybe of
    Nothing ->
      nextMaybe

    Just _ ->
      curMaybe


isWithin :: Location -> A.Located value -> Maybe value
isWithin location (A.At region value) =
  if location |> isWithinRegion region then
    Just value

  else
    Nothing


isWithinRegion :: R.Region -> Location -> Bool
isWithinRegion (R.Region (R.Position startLine startCol) (R.Position endLine endCol)) (Location searchLine searchCol) =
  (searchLine >= startLine && searchCol >= startCol)
    && (searchLine <= endLine && searchCol <= endCol)


unboxLocated :: A.Located value -> value
unboxLocated (A.At _ value) =
  value


-- Can.Type helpers


getArgAtPosition :: Int -> Can.Type -> Maybe Can.Type
getArgAtPosition argNumber tipe =
  case tipe of
    Can.TLambda type1 type2 ->
      let
          nextArgNumber =
            argNumber - 1
      in
      if nextArgNumber == 0 then
        Just type1

      else
      getArgAtPosition nextArgNumber type2

    _ ->
      Nothing


typeToText :: Can.Type -> Text
typeToText tipe =
  typeToTextHelper tipe ""


typeToTextHelper :: Can.Type -> Text -> Text
typeToTextHelper tipe current =
  case tipe of
    Can.TLambda type1 type2 ->
      typeToTextHelper type2 (typeToTextHelper type1 current <> " -> ")

    Can.TVar name ->
      current <> N.toText name

    Can.TType canonical name args ->
      current <>  N.toText name

    Can.TUnit  ->
      current <>  "()"

    Can.TTuple leftType midType maybeRightType ->
      current <>
        "( "
          <> typeToTextHelper leftType ""
          <> ", "
          <> typeToTextHelper midType ""
          <> (maybeRightType
              |> fmap
                  (\rightType ->
                    typeToTextHelper rightType "" <> ", "
                  )
              |> Maybe.fromMaybe ""
             )
          <> " )"

    Can.TAlias canonical name map type_ ->
      current <> N.toText name


-- Debug Helpers


locationToString ::  Location -> String
locationToString (Location searchLine searchCol) =
    "(" ++ (show searchLine) ++ ", " ++ (show searchCol) ++ ")"


locatedToString :: A.Located value -> String
locatedToString (A.At (R.Region (R.Position startLine startCol) (R.Position endLine endCol)) value) =
    "Start: ("
      ++ (show startLine)
      ++ ", "
      ++ (show startCol)
      ++ ") End: ("
      ++ (show endLine)
      ++ ", "
      ++ (show endCol)
      ++ ")"
