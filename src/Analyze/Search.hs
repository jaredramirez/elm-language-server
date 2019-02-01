module Analyze.Search
  ( findValueInModule
  ) where


import AST.Canonical (Module)
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Misc (andThen, (|>), (<|))


data Value
  = Raw N.Name
  | Qualified ModuleName.Canonical N.Name


data Location
  = Location Int Int


findValueInModule :: Module -> Int -> Int -> Maybe Value
findValueInModule modul line column =
  let
      location =
        Location line column
  in
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
    Can.Def locatedDefinitionName patterns expression ->
      let
          thisDefinition =
            locatedDefinitionName
              |> isWithin location
              |> fmap Raw

          thisPatterns =
            searchPatterns location patterns

          thisExpression =
            searchExpression location expression
      in
      thisDefinition
        |> ifNothingThen thisPatterns
        |> ifNothingThen thisExpression

    Can.TypedDef locatedDefinitionName typeVariables typedPatterns expression resultType ->
      let
          isThisDefinition =
            locatedDefinitionName
              |> isWithin location
              |> fmap Raw
      in
      isThisDefinition


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
              Just (Qualified canoncial typeName)

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
searchExpression location locationExpression =
  locationExpression
    |> isWithin location
    |> andThen
        (\expression ->
          case expression of
            Can.VarLocal name ->
              Just <| Raw name

            Can.VarTopLevel canoncial name ->
              Just <| Qualified canoncial name

            Can.VarForeign canoncial name _ ->
              Just <| Qualified canoncial name

            Can.VarCtor _ canoncial name _ _ ->
              Just <| Qualified canoncial name

            Can.VarDebug canoncial name _ ->
              Just <| Qualified canoncial name

            Can.VarOperator name canoncial _ _ ->
              Just <| Qualified canoncial name

            Can.List locatedExpresions ->
              searchExpressions location locatedExpresions

            Can.Negate subExpression ->
              searchExpression location subExpression

            Can.Binop _ canoncial name _ subExpressionA subExpressionB ->
              searchExpression location subExpressionA
                |> ifNothingThen (searchExpression location subExpressionB)
                |> ifNothingThen (Just <| Qualified canoncial name)

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
              Just (Raw name)

            Can.Access expresion locatedName ->
              locatedName
                |> isWithin location
                |> fmap Raw
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
                                (if isWithinRegion location region then
                                  searchExpression location expression

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
isWithin (Location searchLine searchCol) (A.At region value) =
  let
      (R.Region (R.Position startLine startCol) (R.Position endLine endCol)) =
        region
  in
  if
    (searchLine >= startLine && searchCol >= startCol)
      && (searchLine <= endLine && searchCol >= endCol)
  then
    Just value

  else
    Nothing


isWithinRegion :: Location -> R.Region -> Bool
isWithinRegion (Location searchLine searchCol) (R.Region (R.Position startLine startCol) (R.Position endLine endCol)) =
  (searchLine >= startLine && searchCol >= startCol)
    && (searchLine <= endLine && searchCol >= endCol)
