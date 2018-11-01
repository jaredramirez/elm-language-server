{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.ElmConfig
    ( ElmConfig(..)
    , parseFromFile
    , getElmSourceDirectories
    ) where

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Data.Aeson (FromJSON, Value, (.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Misc ((<|), (|>))
import qualified Misc

data ElmConfig
  = Application
    { appName :: Text
    , appSourceDirectories :: [Text]
    , appElmVersion :: Text
    , appDirectDependencies :: Maybe (HashMap Text Text)
    , appIndirectDependencies :: Maybe (HashMap Text Text)
    , appDirectTestDependencies :: Maybe (HashMap Text Text)
    , appIndirectTestDependencies :: Maybe (HashMap Text Text)
    }
  | Package
    { pkgName :: Text
    , pkgSummary :: Text
    , pkgLicense :: Text
    , pkgVersion :: Text
    , pkgExposedModules :: [Text]
    , pkgElmVersion :: Text
    , pkgDependencies :: Maybe (HashMap Text Text)
    , pkgTestDependencies :: Maybe (HashMap Text Text)
    }
    deriving (Show)

data DependencyType
  = Dependencies
  | TestDependencies

dependencyTypeToText :: DependencyType -> Text
dependencyTypeToText dependencyType =
  case dependencyType of
    Dependencies ->
      "dependencies"

    TestDependencies ->
      "test-dependencies"


parseDependencies :: DependencyType -> HashMap Text Value -> Parser (Maybe (HashMap Text Text), Maybe (HashMap Text Text))
parseDependencies dependencyType v =
  let key = dependencyTypeToText dependencyType
      dependencies = HM.lookup key v
  in
  case dependencies of
    Nothing ->
      return (Nothing, Nothing)

    Just depMap ->
      return (,)
        <*> v .:? "direct"
        <*> v .:? "indirect"


parseApplication :: HashMap Text Value -> Parser ElmConfig
parseApplication v =
  let make name sourceDirectories elmVersion (directDeps, indirectDeps) (directTestDeps, indirectTestDeps) =
        Application name sourceDirectories elmVersion directDeps indirectDeps directTestDeps indirectTestDeps
  in
  return make
    <*> v .: "name"
    <*> v .: "source-directories"
    <*> v .: "elm-version"
    <*> parseDependencies Dependencies v
    <*> parseDependencies TestDependencies v


parseExposedModules :: HashMap Text Value -> Parser [Text]
parseExposedModules v =
  let
      key =
        "exposed-modules"

      list :: Parser [Text]
      list =
        v .: key

      map :: Parser [Text]
      map =
        (v .: key :: Parser (HashMap Text [Text]))
          |> fmap (List.foldl (++) [] . HM.elems)
  in list <|> map


parsePackage :: HashMap Text Value -> Parser ElmConfig
parsePackage v =
  return Package
    <*> v .: "name"
    <*> v .: "summary"
    <*> v .: "license"
    <*> v .: "version"
    <*> parseExposedModules v
    <*> v .: "elm-version"
    <*> v .:? "dependencies"
    <*> v .:? "testDependencies"


instance FromJSON ElmConfig where
  parseJSON =
    A.withObject "ElmConfig" $ \v ->
      (v .: "type" :: Parser Text) >>= \type_ ->
        case type_ of
          "application" ->
            parseApplication v

          "package" ->
            parsePackage v

          _ ->
            fail "Invalid elm.json type"

getElmSourceDirectories :: ElmConfig -> [Text]
getElmSourceDirectories config =
  case config of
    Application _ appSourceDirs _ _ _ _ _ ->
      appSourceDirs

    Package _ _ _ _ _ _ _ _ ->
      []


parseFromFile :: FilePath -> IO (Either Text ElmConfig)
parseFromFile filePath =
  BS.readFile filePath
    |> fmap
      (\text ->
        text
          |> A.eitherDecode'
          |> Misc.mapLeft T.pack
      )
