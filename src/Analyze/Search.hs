module Analayze.Search
  (
  ) where


import qualified AST.Source as Src
import AST.Valid (Module)
import qualified AST.Valid as V
import qualified Reporting.Annotation as A
import Misc (andThen)



getImports :: Module -> [Src.Import]
getImports (Src.Module _maybeHeader imports _decls) =
  imports


-- Get value from AST

getValueFromModule :: Module -> Int -> Int -> Maybe ()
getValueFromModule module_ line column =
  module_
    |> V._decls
    |> List.foldr
        (\locatedDeclaration maybeFound ->
          case maybeFound of
            Nothing ->
              locatedDeclaration
                |> isWithinLocated line column
                |> fmap searchDeclaration

            Just _ ->
              maybeFound
        )
        Nothing


searchDeclarations :: [A.Located V.Decl] -> Maybe ()
searchDeclarations  =
  case declaration of
    Src.Union name lowercaseVariables constructors ->
      isWithinLocated name
        |> ifNothingThen isWithinLocated


searchDeclaration :: A.Located V.Decl -> Maybe ()
searchDeclaration locatedDecl =
  locatedDecl
    |> isWithinLocated
    |> andThen
        (\(V.Decl locatedName patterns expression maybeType) ->
          isWithinLocated locatedName
            |> ifNothingThen 
        )



ifNothingThen :: Eq value => Maybe value -> Maybe value -> Maybe value
ifNothingThen nextMaybe curMaybe =
  if curMaybe == Nothing then
    nextMaybe

  else
    curMaybe


isWithinLocated :: Int -> Int -> A.Located value -> Maybe value
isWithinLocated searchLine searchCol (A.A region value) =
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
