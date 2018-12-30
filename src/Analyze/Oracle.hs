{-# LANGUAGE OverloadedStrings #-}

module Analyze.Oracle
    ( ask
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
import Data.Text (Text)
import qualified Data.Text as Text
import LSP.Model (Package)
import qualified LSP.Model as M
import Misc ((|>))

-- Much thanks to https://github.com/Krzysztof-Cieslak/vscode-elm/blob/master/src/elmDelphi.ts
-- the interworkings of this module


ask :: Package -> Module -> Text -> ()
ask package module_ rawSearch =
  let
      eitherSplit =
        splitSearchText rawSearch

      modulesToSearch =
        getImportsFromModule module_
      -- (searchValue, maybeSearchModule)
  in
  ()


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


data TypeExports
  = TypeDotDot
  | TypeExports (Map Text ())
  | TypeNone


data Exports
  = DotDot
  | Exports (Map Text ()) (Map Text TypeExports)
  | None


getImportsFromModule :: Module -> Map ModuleName (Maybe Text, Exports)
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
  in
  Map.union
    localImports
    implicitImports


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
