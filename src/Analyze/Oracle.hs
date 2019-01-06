{-# LANGUAGE OverloadedStrings #-}

module Analyze.Oracle
    ( search
    ) where

import Analyze.Data.Documentation (Documentation, ModuleName)
import qualified Analyze.Data.Documentation as Documentation
import Analyze.Data.Reference (Reference)
import qualified Analyze.Data.Reference as Ref
import qualified AST.Declaration as Dec
import AST.Module (Module)
import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.V0_16 as Base
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Misc ((|>), andThen)
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


search :: Module -> Int -> Int -> HashMap ModuleName Documentation ->  Text -> Either Text Reference
search module_ line column docMap rawSearch =
  let
      eitherSplit =
        splitSearchText rawSearch

      (moduleToDocMap, aliasToModuleMap) =
        getImportsFromModule module_
  in
  eitherSplit
    |> andThen
      (\(value, maybeValueIdentifier) ->
        let
            modulesToSearch =
              getModulesToSearch
                moduleToDocMap
                aliasToModuleMap
                value
                maybeValueIdentifier

            fullValue =
              case maybeValueIdentifier of
                Nothing ->
                  value

                Just moduleIdentifier ->
                  moduleIdentifier <> "." <> value
        in
        if List.null modulesToSearch then
          Left
            ( "\"" <> fullValue <> "\" was not found in any imported modules." )

        else
          modulesToSearch
            |> List.foldr
                (\moduleName maybeFound ->
                  if maybeFound == Nothing then
                    searchModuleDocumentation value docMap moduleName

                  else
                    maybeFound
                )
                Nothing
            |> maybeToEither
                ( "\""
                  <> fullValue
                  <> "\" was found in an import, but it's definition was not found"
                )
      )


-- Maybe/Either helpers

maybeToEither :: error -> Maybe value -> Either error value
maybeToEither error maybeValue =
  case maybeValue of
    Nothing ->
      Left error

    Just value ->
      Right value

-- Read and search module


searchModuleDocumentation :: Text -> HashMap ModuleName Documentation -> ModuleName -> Maybe Reference
searchModuleDocumentation value docMap moduleName =
  let
      maybeDoc =
        HM.lookup moduleName docMap

      (Documentation.ModuleName moduleNameText) =
        moduleName
  in
  maybeDoc
    |> andThen
        (\doc ->
          searchDocUnions moduleNameText value doc
            |> ifNothingThen (searchDocAliases moduleNameText value doc)
            |> ifNothingThen (searchDocValues moduleNameText value doc)
            |> ifNothingThen (searchDocBinops moduleNameText value doc)
        )


searchDocUnions :: Text -> Text -> Documentation -> Maybe Reference
searchDocUnions moduleNameText value doc =
    doc
      |> Documentation._unions
      |> List.foldr
        (\unionDoc acc ->
          if acc == Nothing then
            let
                name =
                  Documentation._unionName unionDoc

                constructors =
                  unionDoc
                    |> Documentation._cases
                    |> List.map
                      (\(Documentation.UnionCase constructor _args) ->
                        constructor
                      )
            in
            if value == name || List.elem value constructors then
              Just
                (Ref.External
                  { Ref.name = name
                  , Ref.fullyQualifiedName = moduleNameText <> "." <> name
                  , Ref.typeSignature = name
                  , Ref.comment = Documentation._unionComment unionDoc
                  }
                )

            else
              Nothing

          else
            acc
        )
        Nothing


searchDocAliases :: Text -> Text -> Documentation -> Maybe Reference
searchDocAliases moduleNameText value doc =
    doc
      |> Documentation._aliases
      |> List.foldr
        (\aliasDoc acc ->
          if acc == Nothing then
            let
                name =
                  Documentation._aliasName aliasDoc
            in
            if value == name then
              Just
                (Ref.External
                  { Ref.name = name
                  , Ref.fullyQualifiedName = moduleNameText <> "." <> name
                  , Ref.typeSignature = Documentation._aliasType aliasDoc
                  , Ref.comment = Documentation._aliasComment aliasDoc
                  }
                )

            else
              Nothing

          else
            acc
        )
        Nothing


searchDocValues :: Text -> Text -> Documentation -> Maybe Reference
searchDocValues moduleNameText value doc =
    doc
      |> Documentation._values
      |> List.foldr
        (\valueDoc acc ->
          if acc == Nothing then
            let
                name =
                  Documentation._valueName valueDoc
            in
            if value == name then
              Just
                (Ref.External
                  { Ref.name = name
                  , Ref.fullyQualifiedName = moduleNameText <> "." <> name
                  , Ref.typeSignature = Documentation._valueType valueDoc
                  , Ref.comment = Documentation._valueComment valueDoc
                  }
                )

            else
              Nothing

          else
            acc
        )
        Nothing


searchDocBinops :: Text -> Text -> Documentation -> Maybe Reference
searchDocBinops moduleNameText value doc =
    doc
      |> Documentation._binops
      |> List.foldr
        (\binopDoc acc ->
          if acc == Nothing then
            let
                name =
                  Documentation._binopName binopDoc
            in
            if value == name then
              Just
                (Ref.External
                  { Ref.name = name
                  , Ref.fullyQualifiedName = moduleNameText <> ".(" <> name <> ")"
                  , Ref.typeSignature = Documentation._binopType binopDoc
                  , Ref.comment = Documentation._binopComment binopDoc
                  }
                )

            else
              Nothing

          else
            acc
        )
        Nothing


-- Maybe helper


ifNothingThen :: Eq value => Maybe value -> Maybe value -> Maybe value
ifNothingThen nextMaybe curMaybe =
  if curMaybe == Nothing then
    nextMaybe

  else
    curMaybe


-- Find modules to search


getModulesToSearch ::
  Map ModuleName Exports
  -> Map Text ModuleName
  -> Text
  -> Maybe Text
  -> [ModuleName]
getModulesToSearch moduleToDocMap aliasToModuleMap value maybeValueIdentifier =
    let
        -- Check if idenfier matches any module name
        maybeModuleIdentifierModuleName =
          maybeValueIdentifier
            |> andThen
              (\valueIdentifier ->
                let
                    moduleName =
                      Documentation.ModuleName valueIdentifier
                in
                moduleToDocMap
                  |> Map.lookup moduleName
                  |> fmap (\_ -> moduleName)
              )

        -- Check if idenfier matches any module alias
        maybeAliasIdentifiderModuleName =
          maybeValueIdentifier
            |> andThen
              (\valueIdentifier ->
                Map.lookup valueIdentifier aliasToModuleMap
              )

        -- Check if value matches any module alias
        maybeValueModuleName =
          Map.lookup value aliasToModuleMap

        -- Check if value matches any exported value, type, or constructor
        maybeTypeOrConstructorModuleNames =
          moduleToDocMap
            |> Map.foldrWithKey
              (\moduleName exports acc ->
                case exports of
                  DotDot ->
                    moduleName : acc

                  Exports valuesMap typesAndConstructorsMap ->
                    let
                        -- Check if value is in module's exported
                        -- varaibles
                        isExportedAsValue =
                          valuesMap
                            |> Map.lookup value
                            |> Maybe.maybe False (\_ -> True)

                        -- Check if value is in module's exported
                        -- types, or if any type exports it
                        isExportedAsTypeOrConstructor =
                          Map.foldrWithKey
                            (\typeName typeExports acc ->
                              if acc == True then
                                acc

                              else
                                value == typeName
                                  || (case typeExports of
                                        TypeDotDot ->
                                          True

                                        TypeExports exportMap ->
                                          exportMap
                                            |> Map.lookup value
                                            |> Maybe.maybe False (\_ -> True)

                                        TypeNone ->
                                          False
                                     )
                            )
                            False
                            typesAndConstructorsMap
                    in
                    if isExportedAsValue || isExportedAsTypeOrConstructor then
                      moduleName : acc

                    else
                      acc

                  None ->
                    acc
              )
              []
            |> fmap Just

        possibleModules =
          [ maybeModuleIdentifierModuleName
          , maybeAliasIdentifiderModuleName
          , maybeValueModuleName
          ]
            ++ maybeTypeOrConstructorModuleNames

    in
    Maybe.catMaybes possibleModules


-- Gather export data form each module


data Exports
  = DotDot
  | Exports (Map Text ()) (Map Text TypeExports)
  | None


data TypeExports
  = TypeDotDot
  | TypeExports (Map Text ())
  | TypeNone


getImportsFromModule :: Module -> (Map ModuleName Exports, Map Text ModuleName)
getImportsFromModule module_ =
  let
      (_comments, imports) =
        Module.imports module_

      localImports =
        imports
          |> mapKeys uppercaseIdentifiersToModuleName
          |> Map.map
            (\(_importComments, importMethod) ->
              ( importMethod
                  |> Module.alias
                  |> fmap
                    (\(_comments, (_precomment, aliasIdenfitier)) ->
                      uppercaseIdentifierToText aliasIdenfitier
                    )
              , importMethod
                  |> Module.exposedVars
                  |>(\(_comments, (_precomment, listing)) ->
                      case listing of
                        Var.ExplicitListing detailedListing _ ->
                          let
                              valuesMap =
                                detailedListing
                                  |> Module.values
                                  |> mapKeys lowercaseIdentifierToText
                                  |> Map.map (\_ -> ())

                              operatorsMap =
                                detailedListing
                                  |> Module.operators
                                  |> mapKeys symbolIdentifierToText
                                  |> Map.map (\_ -> ())

                              typesAndConstructorsMap =
                                detailedListing
                                  |> Module.types
                                  |> Map.toList
                                  |> fmap
                                    (\(typeIdenfier, Base.Commented _ (_comments, typeListing) _) ->
                                      (uppercaseIdentifierToText typeIdenfier
                                      , case typeListing of
                                        Var.ExplicitListing map _ ->
                                          (map :: Var.CommentedMap Base.UppercaseIdentifier ())
                                            |> mapKeys uppercaseIdentifierToText
                                            |> Map.map (\_ -> ())
                                            |> TypeExports

                                        Var.OpenListing _ ->
                                          TypeDotDot

                                        Var.ClosedListing ->
                                          TypeNone
                                      )
                                    )
                                  |> Map.fromList

                          in
                          Exports (Map.union valuesMap operatorsMap) typesAndConstructorsMap

                        Var.OpenListing _ ->
                          DotDot

                        Var.ClosedListing ->
                          None
                  )
              )
            )

      allImports =
        Map.union
          localImports
          implicitImports
  in
  Map.foldrWithKey
    (\moduleName (maybeAlias, exports) (moduleToDocMap, aliasToModuleMap) ->
      case maybeAlias of
        Nothing ->
          ( Map.insert moduleName exports moduleToDocMap
          , aliasToModuleMap
          )

        Just alias ->
          ( Map.insert moduleName exports moduleToDocMap
          , Map.insert alias moduleName aliasToModuleMap
          )
    )
    (Map.empty, Map.empty)
    allImports


uppercaseIdentifiersToModuleName :: [Base.UppercaseIdentifier] -> ModuleName
uppercaseIdentifiersToModuleName identifiers =
  identifiers
    |> List.map uppercaseIdentifierToText
    |> List.intersperse "."
    |> Text.concat
    |> Documentation.ModuleName


uppercaseIdentifierToText :: Base.UppercaseIdentifier -> Text
uppercaseIdentifierToText (Base.UppercaseIdentifier identifier) =
  Text.pack identifier


lowercaseIdentifierToText :: Base.LowercaseIdentifier -> Text
lowercaseIdentifierToText (Base.LowercaseIdentifier identifier) =
  Text.pack identifier


symbolIdentifierToText :: Base.SymbolIdentifier -> Text
symbolIdentifierToText (Base.SymbolIdentifier identifier) =
  Text.pack identifier


mapKeys :: (Ord key, Ord nextKey) => (key -> nextKey) -> Map key value -> Map nextKey value
mapKeys toNextKey map =
  map
    |> Map.toList
    |> fmap (\(key, value) -> (toNextKey key, value))
    |> Map.fromList


implicitImports :: Map ModuleName (Maybe Text, Exports)
implicitImports =
  Map.empty
    |> Map.insert (Documentation.ModuleName "Basics") (Nothing, DotDot)
    |> Map.insert (Documentation.ModuleName "List")
      ( Nothing
      , Exports
        (Map.insert "::" () Map.empty)
        (Map.insert "List" TypeNone Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "Maybe")
      ( Nothing
      , Exports Map.empty (Map.insert "Maybe" TypeDotDot Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "Result")
      ( Nothing
      , Exports Map.empty (Map.insert "Result" TypeDotDot Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "String")
      ( Nothing
      , Exports Map.empty (Map.insert "String" TypeNone Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "Char")
      ( Nothing
      , Exports Map.empty (Map.insert "Char" TypeNone Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "Tuple")
      ( Nothing
      , Exports Map.empty Map.empty
      )
    |> Map.insert (Documentation.ModuleName "Debug")
      ( Nothing
      , Exports Map.empty Map.empty
      )
    |> Map.insert (Documentation.ModuleName "Platform")
      ( Nothing
      , Exports Map.empty (Map.insert "Platform" TypeDotDot Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "Platform.Cmd")
      ( Just "Cmd"
      , Exports Map.empty (Map.insert "Cmd" TypeDotDot Map.empty)
      )
    |> Map.insert (Documentation.ModuleName "Platform.Sub")
      ( Just "Sub"
      , Exports Map.empty (Map.insert "Sub" TypeDotDot Map.empty)
      )


-- Split search in to module and varaibles


splitSearchText :: Text -> Either Text (Text, Maybe Text)
splitSearchText search =
  let
      parts =
        search
          |> Text.split (== '.')
          |> List.reverse
  in
  case parts of
    [] ->
      Left "Invalid search"

    [head] ->
      Right (head, Nothing)

    head : rest ->
      Right
        ( head
        , rest
          |> List.reverse
          |> Text.intercalate "."
          |> Just
        )

-- Get value from AST

getValueFromModule :: Module -> Int -> Int -> Maybe ()
getValueFromModule module_ line column =
  module_
    |> Module.body
    |> List.foldr
        (\topLevelDeclaration maybeFound ->
          if maybeFound == Nothing then
            case topLevelDeclaration of
              Dec.DocComment _ ->
                Nothing

              Dec.BodyComment _ ->
                Nothing

              Dec.Entry locatedDeclaration ->
                let
                    within =
                      isWithinLocated line column locatedDeclaration
                in
                -- TODO: continue searching AST to find word cursor is on
                Nothing

          else
            maybeFound
        )
        Nothing


data Within value
  = Is value
  | IsNot


isWithinLocated :: Int -> Int -> A.Located value -> Within value
isWithinLocated searchLine searchCol (A.A region value) =
  let
      (R.Region (R.Position startLine startCol) (R.Position endLine endCol)) =
        region
  in
  if
    (searchLine >= startLine && searchCol >= startCol)
      && (searchLine <= endLine && searchCol >= endCol)
  then
    Is value

  else
    IsNot
