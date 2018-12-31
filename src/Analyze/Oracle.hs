{-# LANGUAGE OverloadedStrings #-}

module Analyze.Oracle
    ( search
    ) where

import Analyze.Data.Documentation (Documentation, ModuleName)
import qualified Analyze.Data.Documentation as Documentation
import Analyze.Data.ElmConfig (ElmConfig)
import qualified Analyze.Data.ElmConfig as ElmConfig
import AST.Module (Module)
import qualified AST.Module as Module
import qualified AST.V0_16 as Base
import qualified AST.Variable as Var
import qualified Data.List as List
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import LSP.Model (Package)
import qualified LSP.Model as M
import Misc ((|>), andThen)


-- TODO: Support local modules search
-- Thoughts on this are to pretty much do everything the same
-- as searching external modules, then for whatever modules are
-- not found in ~/.elm/0.19.0/package/*, look in the local file path.
-- We can convert Module.Name to Module/Name.elm for local lookups.
-- This can get tricky for two reason:
-- 1. May be expenseive to spend extra time checking if lots of files exist
-- since we'd probably check external modules first, then look locally.
-- 2. `package` type projects can have lots of possible source nesting since
-- they do not have to specify source directories, making the lookup harder.
-- Plus `applicaton` type projects can have multiple source dirs
--
-- Additionally, we'd need to add in AST parsing for any modules that
-- do not already exist in the `package`. This isn't to bad, we'd just
-- need to make sure that we return the newly parsed ASTs to be added to
-- the model to avoid having to re-parse them later. This should be fine
-- since we re-parse and update all AST on file open and change, so we
-- should generally avoid inconsistent data.
--
-- The search function for variables in ASTs can be used for the currently
-- open module, but this will also get tricky for any non-globally defined
-- varaibles or types. For searching for function level declarations, we'll
-- need to deterime what scope we're in before proceeding


search :: Package -> Module -> Text -> ()
search package module_ rawSearch =
  let
      eitherSplit =
        splitSearchText rawSearch

      (moduleMap, aliasMap) =
        getImportsFromModule module_

      documentation =
        M._documentation package
  in
  case eitherSplit of
    Left problem ->
      ()

    Right (value, maybeValueIdentifier) ->
      let
          -- Check if idenfier matches any module name
          maybeModuleIdentifierModuleName =
            maybeValueIdentifier
              |> andThen
                (\moduleName ->
                  moduleMap
                    |> Map.lookup moduleName
                    |> fmap (\_ -> moduleName)
                )

          -- Check if idenfier matches any module alias
          maybeAliasIdentifiderModuleName =
            maybeValueIdentifier
              |> andThen
                (\valueIdentifier ->
                  Map.lookup valueIdentifier aliasMap
                )

          -- Check if value matches any module alias
          maybeValueModuleName =
            Map.lookup value aliasMap

          -- Check if value matches any exported value, type, or constructor
          maybeTypeOrConstructorModuleNames =
            moduleMap
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
                      if isExportedAsValue || isExportedAsValue then
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

          modulesToSearch =
            Maybe.catMaybes possibleModules
      in
      if List.null modulesToSearch then
        Left ( "\"" <> value <> "\" was not found in any imported modules.")

      else
        -- TODO: Search modules for reference to search value


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
    (\moduleName (maybeAlias, exports) (moduleMap, aliasMap) ->
      case maybeAlias of
        Nothing ->
          ( Map.insert moduleName exports moduleMap
          , aliasMap
          )

        Just alias ->
          ( Map.insert moduleName exports moduleMap
          , Map.insert alias moduleName aliasMap
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


splitSearchText :: Text -> Either Text (Text, Maybe ModuleName)
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
          |> Documentation.ModuleName
          |> Just
        )
