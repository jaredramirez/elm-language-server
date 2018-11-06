{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.FileEvent
  ( FileEvent(..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Aeson.Types (Parser, Pair)
import LSP.Data.URI (URI)
import LSP.Data.FileChangeType (FileChangeType)
import Misc ((<|))

data FileEvent =
  FileEvent URI FileChangeType
  deriving (Show)

instance FromJSON FileEvent where
  parseJSON =
    A.withObject "FileEvent" <| \v ->
      return FileEvent
        <*> v .: "uri"
        <*> v .: "type"

instance ToJSON FileEvent where
  toJSON (FileEvent uri fileTypeChange) =
    A.object
      [ "uri" .= uri
      , "type" .= fileTypeChange
      ]

-- CLIENT CAPABILITIES --
didChangeWatchedFiles :: Text
didChangeWatchedFiles = "workspace/didChangeWatchedFiles"
