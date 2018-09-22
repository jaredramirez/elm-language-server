{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.RequestMethod
  ( InitializeParams(..)
  , TextDocumentHoverParams(..)
  , RequestMethod(..)
  ) where

import           Data.Aeson                      (ToJSON, FromJSON, Value, (.:))
import qualified Data.Aeson                      as A
import           Data.Aeson.Types                (Parser)
import qualified Data.ByteString.Lazy            as BS
import qualified Data.HashMap.Strict             as HM
import           Data.Text                       (Text)
import           LSP.Data.Position               (Position)
import           LSP.Data.TextDocumentIdentifier (TextDocumentIdentifier)
import           LSP.Data.URI                    (URI)
import           Misc                            ((<|))

-- INITIALIZE --
initialize :: Text
initialize = "initialize"

newtype InitializeParams =
  InitializeParams URI
  deriving (Show)

instance FromJSON InitializeParams where
  parseJSON =
    A.withObject "InitializeParams" <| \v -> InitializeParams <$> v .: "rootUri"

-- TEXT DOCUMENT HOVER --
textDocumentHover :: Text
textDocumentHover = "textDocument/hover"

newtype TextDocumentHoverParams =
  TextDocumentHoverParams (TextDocumentIdentifier, Position)
  deriving (Show)

instance FromJSON TextDocumentHoverParams where
  parseJSON =
    A.withObject "TextDocumentHoverParams" <| \v ->
      curry TextDocumentHoverParams <$> v .: "textDocument" <*> v .: "position"

-- SHUTDOWN --
shutdown :: Text
shutdown = "shutdown"

-- METHODS --
data RequestMethod
  = Initialize Value
  | TextDocumentHover Value
  | Shutdown
  deriving (Show)

decoder :: HM.HashMap Text Value -> Text -> Parser RequestMethod
decoder v key
  | key == initialize = Initialize <$> v .: "params"
  | key == shutdown = return Shutdown
  | otherwise = fail "Unknown request method"

instance FromJSON RequestMethod where
  parseJSON = A.withObject "RequestMethod" <| \v -> v .: "method" >>= decoder v

instance ToJSON RequestMethod where
  toJSON message =
    case message of
      Initialize _ ->
        A.toJSON initialize

      TextDocumentHover _ ->
        A.toJSON textDocumentHover

      Shutdown ->
        A.toJSON shutdown
