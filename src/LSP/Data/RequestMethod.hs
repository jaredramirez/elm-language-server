{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.RequestMethod
  ( InitializeParams(..)
  , TextDocumentHoverParams(..)
  , RequestMethod(..)
  , toPairs
  ) where

import           Data.Aeson                      (FromJSON, Value, (.:), (.=))
import qualified Data.Aeson                      as A
import           Data.Aeson.Types                (Parser, Pair)
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
  TextDocumentHoverParams (URI, Position)
  deriving (Show)

instance FromJSON TextDocumentHoverParams where
  parseJSON =
    A.withObject "TextDocumentHoverParams" <| \v ->
      return (curry TextDocumentHoverParams)
        <*> (v .: "textDocument" >>= \subV -> subV .: "uri")
        <*> v .: "position"

-- SHUTDOWN --
shutdown :: Text
shutdown = "shutdown"

-- METHODS --
data RequestMethod
  = Initialize InitializeParams
  | TextDocumentHover TextDocumentHoverParams
  | Shutdown
  deriving (Show)

decoder :: HM.HashMap Text Value -> Text -> Parser RequestMethod
decoder v key
  | key == initialize = Initialize <$> v .: "params"
  | key == shutdown = return Shutdown
  | otherwise = fail "Unknown request method"

instance FromJSON RequestMethod where
  parseJSON = A.withObject "RequestMethod" <| \v -> v .: "method" >>= decoder v

toPairs :: RequestMethod -> [Pair]
toPairs message =
  case message of
    Initialize _ ->
      [ "method" .= initialize ]

    TextDocumentHover _ ->
      [ "method" .= textDocumentHover ]

    Shutdown ->
      [ "method" .= shutdown ]
