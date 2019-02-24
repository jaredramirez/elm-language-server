{-# LANGUAGE OverloadedStrings #-}

module Analyze.Search
  ( hover
  , HoverResult(..)
  , getInfo
  , Location(..)
  , Value(..)
  , canTypeToText
  ) where


import AST.Canonical (Module)
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Data.List as List
import qualified Data.Foldable as Foldable
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
import qualified Type.Constrain.Module as Constrain
import qualified Type.Type as Type
import qualified Type.Solve as TypeSolver
import qualified Type.UnionFind as UF
import qualified Task


-- Hover


data HoverResult
  = HoverType Can.Type
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
      Task.throw "Not found"

    Just info ->
      case info of
        Raw name ->
          do
            annotations <- LSP.Misc.getAnnotations modul
            let maybeArgType =
                  annotations
                    |> Map.lookup name
                    |> andThen
                        (\(Can.Forall freeVars tipe) ->
                          searchCanType name tipe
                            |> ifNothingThen
                                (freeVars
                                  |> Map.lookup name
                                  |> fmap (\_ -> Can.TVar name)
                                )
                            |> ifNothingThen (Just tipe)
                        )
            case maybeArgType of
              Nothing ->
                HoverDebug (N.toString name)
                  |> return
                -- Task.throw "Not found 1"

              Just canType ->
                HoverType canType
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
                Task.throw "Inferred type not found"

              Just canType ->
                HoverType canType
                  |> return


        Type variableName canType ->
          HoverType canType
            |> return

        Reference canonical name ->
          HoverReference canonical name
            |> return

        Debug message ->
          HoverDebug message
            |> return


-- Base search


data Value
  = Raw N.Name
  | InferredArg N.Name Int N.Name
  | Type (Maybe N.Name) Can.Type
  | Reference ModuleName.Canonical N.Name
  | Debug String


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

                                      Can. argName ->
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
                                          Just (Type (Just name) curType)

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

            Can.PCtor {Can._p_home=canoncial, Can._p_type=typeName, Can._p_args=args} ->
              args
                |> List.foldr
                    (\(Can.PatternCtorArg {Can._arg=arg, Can._type=tipe}) acc ->
                      case acc of
                        Nothing ->
                          let
                              thisPattern =
                                searchPattern location arg

                              thisPatternType =
                                arg
                                  |> isWithin location
                                  |> fmap (\_ -> Type Nothing tipe)
                          in
                          thisPattern
                            |> ifNothingThen thisPatternType

                        Just _ ->
                          acc
                    )
                    Nothing
                |> ifNothingThen (Just (Reference canoncial typeName))

            Can.PUnit ->
              Just unit

            Can.PBool _ _ ->
              Just bool

            Can.PChr _ ->
              Just char

            Can.PStr _ ->
              Just string

            Can.PInt _ ->
              Just int

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
              Just char

            Can.Str _ ->
              Just string

            Can.Int _ ->
              Just int

            Can.Float _ ->
              Just float

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
              Just unit

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


-- Can Type helpers


bool :: Value
bool =
  Type Nothing (Can.TType ModuleName.basics N.bool [])


char :: Value
char =
  Type Nothing (Can.TType ModuleName.char N.char [])


string :: Value
string =
  Type Nothing (Can.TType ModuleName.string N.string [])


int :: Value
int =
  Type Nothing (Can.TType ModuleName.basics N.int [])


float :: Value
float =
  Type Nothing (Can.TType ModuleName.basics N.float [])


unit :: Value
unit =
  Type Nothing Can.TUnit


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


searchCanType ::  N.Name -> Can.Type -> Maybe Can.Type
searchCanType nameToSearchFor tipe =
  case tipe of
    Can.TLambda type1 type2 ->
      searchCanType nameToSearchFor type1
        |> ifNothingThen (searchCanType nameToSearchFor type2)

    Can.TVar name ->
      if nameToSearchFor == name then
        Just tipe

      else
        Nothing

    Can.TType _canonical name args ->
      if nameToSearchFor == name then
        Just tipe

      else
        args
          |> List.foldr
              (\cur acc ->
                case acc of
                  Nothing ->
                    searchCanType nameToSearchFor cur

                  Just _ ->
                    acc
              )
              Nothing

    Can.TUnit  ->
      Nothing

    Can.TTuple leftType midType maybeRightType ->
      searchCanType nameToSearchFor leftType
        |> ifNothingThen (searchCanType nameToSearchFor midType)
        |> ifNothingThen (maybeRightType |> andThen (searchCanType nameToSearchFor))

    Can.TAlias canonical name typeMap _type ->
      if nameToSearchFor == name then
        Just tipe

      else
        typeMap
          |> List.foldr
              (\(subName, subType) acc ->
                if nameToSearchFor == subName then
                  Just subType

                else
                  searchCanType nameToSearchFor subType
              )
              Nothing


canTypeToText :: Can.Type -> Text
canTypeToText tipe =
  canTypeToTextHelper False tipe ""


canTypeToTextHelper :: Bool -> Can.Type -> Text -> Text
canTypeToTextHelper wrapLambda tipe current =
  case tipe of
    Can.TLambda type1 type2 ->
      let
          firstPart =
            canTypeToTextHelper True type1 ""

          secondPart =
            canTypeToTextHelper False type2 ""
      in
      if wrapLambda then
        current <> "(" <> firstPart <> " -> " <> secondPart <> ")"

      else
        current <> firstPart <> " -> " <> secondPart

    Can.TVar name ->
      current <> N.toText name

    Can.TType canonical name args ->
      current <> N.toText name

    Can.TUnit  ->
      current <>  "()"

    Can.TTuple leftType midType maybeRightType ->
      current <>
        "( "
          <> canTypeToTextHelper True leftType ""
          <> ", "
          <> canTypeToTextHelper True midType ""
          <> (maybeRightType
              |> fmap
                  (\rightType ->
                    canTypeToTextHelper True rightType "" <> ", "
                  )
              |> Maybe.fromMaybe ""
             )
          <> " )"

    Can.TAlias canonical name map type_ ->
      current <> N.toText name


-- Constraint Type helpers


searchConstraint :: [N.Name] -> Type.Constraint -> Task (Maybe N.Name)
searchConstraint names constraint =
  case names of
    [] ->
      Nothing |> return

    nameToSearchFor : restOfNames ->
      case constraint of
        Type.CLocal _region localName _exptectedType ->
          if nameToSearchFor == localName then
            Just localName |> return

          else
            Nothing |> return

        Type.CLet {Type._header=header, Type._headerCon=headerCon, Type._bodyCon=bodyCon} ->
          case Map.lookup nameToSearchFor header of
            Nothing ->
              searchConstraint names bodyCon

            Just locatedTipe ->
              if List.null restOfNames then
                contraintTypeToName (unboxLocated locatedTipe)

              else
                searchConstraint restOfNames headerCon

        Type.CAnd subConstraints ->
          subConstraints
            |> Foldable.foldrM
                (\cur acc ->
                  case acc of
                    Nothing ->
                      searchConstraint names cur

                    Just _ ->
                      acc |> return
                )
                Nothing

        _ ->
          Nothing |> return


contraintTypeToName :: Type.Type -> Task (Maybe N.Name)
contraintTypeToName tipe =
  case tipe of
    Type.PlaceHolder name ->
      Just name
        |> return

    Type.VarN variable ->
      do
        (Type.Descriptor content _ _ _) <- Task.lift <| UF.get variable
        case content of
          Type.FlexVar name ->
            name
              |> Maybe.fromMaybe (N.fromText "a")
              |> Just
              |> return

          Type.FlexSuper _ name ->
            return name

          Type.RigidVar name ->
            return (Just name)

          Type.RigidSuper _ name ->
            return (Just name)

          _ ->
            return Nothing

    _ ->
      return Nothing


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
