{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.ElmConfig
    ( ElmConfig(..)
    , parseFromFile
    ) where

import Data.Aeson (FromJSON, Value, (.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Misc ((<|), (|>))

data ElmConfig
  = Application
    { appName :: Text
    , appSourceDirectories :: Text
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
    , pkgDirectDependencies :: Maybe (HashMap Text Text)
    , pkgIndirectDependencies :: Maybe (HashMap Text Text)
    , pkgDirectTestDependencies :: Maybe (HashMap Text Text)
    , pkgIndirectTestDependencies :: Maybe (HashMap Text Text)
    }
    deriving (Show)

parseDependencies :: HashMap Text Value -> Parser (Maybe (HashMap Text Text), Maybe (HashMap Text Text))
parseDependencies v =
  let dependencies = HM.lookup "dependencies" v
  in
  case dependencies of
    Nothing ->
      fail "Failed to find \"dependencies\" object"

    Just depMap ->
      return (,)
        <*> v .: "direct"
        <*> v .: "indirect"


parseTestDependencies :: HashMap Text Value -> Parser (Maybe (HashMap Text Text), Maybe (HashMap Text Text))
parseTestDependencies v =
  let dependencies = HM.lookup "test-dependencies" v
  in
  case dependencies of
    Nothing ->
      fail "Failed to find \"test-dependencies\" object"

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
    <*> parseDependencies v
    <*> parseTestDependencies v


parsePackage :: HashMap Text Value -> Parser ElmConfig
parsePackage v =
  let make name summary license version exposedModules elmVersion (directDeps, indirectDeps) (directTestDeps, indirectTestDeps) =
        Package name summary license version exposedModules elmVersion directDeps indirectDeps directTestDeps indirectTestDeps
  in
  return make
    <*> v .: "name"
    <*> v .: "summary"
    <*> v .: "license"
    <*> v .: "version"
    <*> v .: "exposed-modules"
    <*> v .: "elm-version"
    <*> parseDependencies v
    <*> parseTestDependencies v


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

parseFromFile :: FilePath -> IO (Either String ElmConfig)
parseFromFile filePath =
  BS.readFile filePath
    |> fmap A.eitherDecode'
