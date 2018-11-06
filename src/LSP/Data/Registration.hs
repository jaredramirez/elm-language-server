{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Registration
  ( Registration(..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import LSP.Data.FileSystemWatcher (FileSystemWatcher)
import Misc ((<|))


data Registration
  = DidChangeWatchedFiles Text [FileSystemWatcher]
  deriving (Show)

didChangeWatchedFiles :: Text
didChangeWatchedFiles = "workspace/didChangeWatchedFiles"

parseRegistration :: HM.HashMap Text Value -> Text -> Parser Registration
parseRegistration v method
  | method == didChangeWatchedFiles =
    return DidChangeWatchedFiles
      <*> v .: "id"
      <*> v .: "watchers"

instance FromJSON Registration where
  parseJSON =
    A.withObject "Registration" <| \v -> v .: "method" >>= parseRegistration v

instance ToJSON Registration where
  toJSON registrationMethod =
    case registrationMethod of
      DidChangeWatchedFiles id fileSystemWatchers ->
        A.object
          [ "id" .= id
          , "method" .= (didChangeWatchedFiles :: Text)
          , "registerOptions" .=
              A.object [ "watchers" .= fileSystemWatchers ]
          ]
