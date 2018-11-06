{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.FileSystemWatcher
  ( FileSystemWatcher(..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Aeson.Types (Parser, Pair)
import LSP.Data.URI (URI)
import LSP.Data.FileChangeType (FileChangeType)
import Misc ((<|))

data FileSystemWatcher =
  FileSystemWatcher Text (Maybe FileChangeType)
  deriving (Show)

instance FromJSON FileSystemWatcher where
  parseJSON =
    A.withObject "FileSystemWatcher" <| \v ->
      return FileSystemWatcher
        <*> v .: "globPattern"
        <*> v .:? "kind"

instance ToJSON FileSystemWatcher where
  toJSON (FileSystemWatcher glob kind) =
    A.object <|
      (("globPattern" .= glob)
        : (case kind of
              Nothing ->
                []

              Just kind ->
                [ "kind" .= kind ]
          )
      )

-- CLIENT CAPABILITIES --
didChangeWatchedFiles :: Text
didChangeWatchedFiles = "workspace/didChangeWatchedFiles"
