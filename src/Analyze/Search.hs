{-# LANGUAGE OverloadedStrings #-}

module Analyze.Search
  ( getInfo
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Render.Type as RenderType
import Misc (andThen, (|>), (<|))


-- Hover


data HoverResult
  = HoverType Text
  | HoverReference ModuleName.Canonical N.Name
  | HoverDebug String


hover :: Module -> L.Localizer -> Int -> Int -> Maybe HoverResult
hover modul localizer line column =
  let
      location =
        Location line column

      maybeInfo =
        getInfo modul location
  in
  maybeInfo
    |> andThen
        (\info ->
          case info of
            RawName _ ->
              Nothing

            Reference canonical name ->
              HoverReference canonical name
                |> Just

            Type (Primitive name) ->
              HoverType (N.toText name)
                |> Just

            Type (Definition tipe) ->
              RenderType.canToDoc localizer RenderType.None tipe
                |> D.toString
                |> Text.pack
                |> HoverType
                |> Just

            Debug message ->
              HoverDebug message
                |> Just
        )


-- Base search


data Value
  = RawName N.Name
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
              |> fmap RawName

          thisPatterns =
            searchPatterns location patterns

          thisExpression =
            searchExpression location expression
      in
      thisDefinition
        |> ifNothingThen thisExpression
        |> ifNothingThen thisExpression

    Can.TypedDef locatedDefinition _typeVariables typedPatterns expression resultType ->
      let
          thisDefinition =
            locatedDefinition
              |> isWithin location
              |> fmap RawName

          thisPatterns =
            typedPatterns
              |> List.map fst
              |> searchPatterns location

          thisExpression =
            searchExpression location expression
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
                |> ifNothingThen (Just <| RawName name)

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
  locatedExpression
    |> isWithin location
    |> andThen
        (\expression ->
          case expression of
            Can.VarLocal name ->
              Just <| RawName name

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
              searchExpressions location locatedExpresions

            Can.Negate subExpression ->
              searchExpression location subExpression

            Can.Binop binopName canoncial _ _ subExpressionA subExpressionB ->
              searchExpression location subExpressionA
                |> ifNothingThen (searchExpression location subExpressionB)
                |> ifNothingThen (Just <| Reference canoncial binopName)

            Can.Lambda subPatterns expression ->
              searchExpression location expression
                |> ifNothingThen (searchPatterns location subPatterns)

            Can.Call function args ->
              searchExpression location function
                |> ifNothingThen (searchExpressions location args)

            Can.If expressions result ->
              searchExpression location result
                |> ifNothingThen
                    (List.foldl
                      (\maybeFound (expressionA, expressionB) ->
                        maybeFound
                          |> ifNothingThen (searchExpression location expressionA)
                          |> ifNothingThen (searchExpression location expressionB)
                      )
                      Nothing
                      expressions
                    )

            Can.Let definition subExpression ->
              searchExpression location subExpression
                |> ifNothingThen (searchDefinition location definition)

            Can.LetRec definitions subExpression ->
              searchExpression location subExpression
                |> ifNothingThen (searchDefinitions location definitions)

            Can.LetDestruct pattern subExpressionA subExpressionB ->
              searchPattern location pattern
                |> ifNothingThen (searchExpression location subExpressionA)
                |> ifNothingThen (searchExpression location subExpressionB)

            Can.Case subExpression caseBranches ->
              searchExpression location subExpression
                |> ifNothingThen (searchCaseBranches location caseBranches)

            Can.Accessor name ->
              Just (RawName name)

            Can.Access expresion locatedName ->
              locatedName
                |> isWithin location
                |> fmap RawName
                |> ifNothingThen (searchExpression location expresion)

            Can.Update name expression fieldUpdates ->
              searchExpression location expression
                |> ifNothingThen
                    (fieldUpdates
                      |> Map.toList
                      |> List.foldl
                          (\maybeFound (_ , Can.FieldUpdate region expression) ->
                            maybeFound
                              |> ifNothingThen
                                (if location |> isWithinRegion region then
                                  searchExpression location expression

                                 else
                                  Nothing
                                )
                          )
                          Nothing

                    )
                |> ifNothingThen (Just <| RawName name)

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
              searchExpression location subExpressionA
                |> ifNothingThen (searchExpression location subExpressionB)
                |> ifNothingThen (wrapperExpression |> andThen (searchExpression location))

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
