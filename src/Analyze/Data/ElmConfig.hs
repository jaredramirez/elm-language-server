{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze.Data.ElmConfig
  ( ElmVersion(..)
  , ElmConfig(..)
  , DependencyName(..)
  , ExactVersion(..)
  , RangeVersion(..)
  , parseFromFile
  , getElmSourceDirectories
  , getElmDependencies
  ) where


import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, FromJSONKey, Value, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import Data.Hashable as H
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Misc ((|>))
import qualified Misc
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PChar
import qualified Text.Parsec.Error as PError
import qualified Text.Parsec.Text as PText


-- Version


data ElmVersion
  = InvalidVersion
  | V0_19
  deriving (Show)


newtype ExactVersion =
  Exact Text
  deriving (Show)


semverParser :: PText.Parser ExactVersion
semverParser = do
    major <- P.many1 PChar.digit
    _ <- PChar.char '.'
    minor <- P.many1 PChar.digit
    _ <- PChar.char '.'
    patch <- P.many1 PChar.digit
    (major ++ "." ++ minor ++ "." ++ patch)
        |> T.pack
        |> Exact
        |> return


instance FromJSON ExactVersion where
  parseJSON =
    A.withText "ExactVersion" $ \text ->
      case P.parse semverParser "" text of
        Left error ->
          fail
            ( error
              |> PError.errorMessages
              |> List.foldr (\cur acc -> acc ++ PError.messageString cur) ""
            )

        Right validSemver ->
          return validSemver


data RangeVersion =
  Range ExactVersion ExactVersion
  deriving (Show)


semverRangeParser :: PText.Parser RangeVersion
semverRangeParser = do
    _ <- PChar.spaces
    lowerVersion <- semverParser
    _ <- PChar.spaces
    _ <- PChar.string "<= v <"
    _ <- PChar.spaces
    upperVersion <- semverParser
    _ <- PChar.spaces
    return (Range lowerVersion upperVersion)


instance FromJSON RangeVersion where
  parseJSON =
    A.withText "RangeVersion" $ \text ->
      case P.parse semverRangeParser "" text of
        Left error ->
          fail
            ( error
              |> PError.errorMessages
              |> List.foldr (\cur acc -> acc ++ PError.messageString cur) ""
            )

        Right validSemverRange ->
          return validSemverRange


-- Dependency


newtype DependencyName =
  DependencyName { getDependcyName :: Text }
  deriving (Show, Eq, Ord, H.Hashable, FromJSONKey)


instance FromJSON DependencyName where
  parseJSON =
    A.withText "DependencyName" $ \text -> return (DependencyName text)

 -- Elm configs


data ElmConfig
  = Application
    { appSourceDirectories :: [Text]
    , appElmVersion :: Text
    , appDirectDependencies :: HashMap DependencyName ExactVersion
    , appIndirectDependencies :: HashMap DependencyName ExactVersion
    , appDirectTestDependencies :: HashMap DependencyName ExactVersion
    , appIndirectTestDependencies :: HashMap DependencyName ExactVersion
    }
  | Package
    { pkgName :: Text
    , pkgSummary :: Text
    , pkgLicense :: Text
    , pkgVersion :: Text
    , pkgExposedModules :: [Text]
    , pkgElmVersion :: Text
    , pkgDependencies :: HashMap DependencyName RangeVersion
    , pkgTestDependencies :: HashMap DependencyName RangeVersion
    }
    deriving (Show)


parseApplication :: HashMap Text Value -> Parser ElmConfig
parseApplication v =
  let make sourceDirectories elmVersion directDeps indirectDeps directTestDeps indirectTestDeps =
        Application sourceDirectories elmVersion directDeps indirectDeps directTestDeps indirectTestDeps
  in
  return make
    <*> v .: "source-directories"
    <*> v .: "elm-version"
    <*> (v .: "dependencies" >>= \subV -> subV .: "direct")
    <*> (v .: "dependencies" >>= \subV -> subV  .: "indirect")
    <*> (v .: "test-dependencies" >>= \subV -> subV .: "direct")
    <*> (v .: "test-dependencies" >>= \subV -> subV .: "indirect")


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
    <*> v .: "dependencies"
    <*> v .: "test-dependencies"


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


-- Helpers


getElmSourceDirectories :: ElmConfig -> [Text]
getElmSourceDirectories config =
  case config of
    Application {appSourceDirectories = appSourceDirs} ->
      appSourceDirs

    Package {} ->
      -- If the project is a package, then the current
      -- directory is the source dir
      ["."]


getElmDependencies :: ElmConfig -> HashMap DependencyName ExactVersion
getElmDependencies config =
  case config of
    Application {appDirectDependencies = deps} ->
      HM.map (\version -> version) deps

    Package {pkgDependencies = deps} ->
      HM.map (\(Range lowerVersion _) -> lowerVersion) deps


parseFromFile :: Text -> IO (Either Text ElmConfig)
parseFromFile filePath =
  filePath
    |> T.unpack
    |> BS.readFile
    |> fmap
      (\text ->
        text
          |> A.eitherDecode'
          |> Misc.mapLeft T.pack
      )
