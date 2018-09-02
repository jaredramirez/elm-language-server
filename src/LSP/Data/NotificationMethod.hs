{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.NotificationMethod
  ( NotificationMethod(..)
  , TextDocumentDidOpenParams(..)
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import           Misc                 ((<|))
import qualified Misc

-- TEXT DOCUMENT DID OPEN --
textDocumentDidOpen :: Text
textDocumentDidOpen = "textDocument/didOpen"

newtype TextDocumentDidOpenParams =
  TextDocumentDidOpenParams (Text, Int, Text)
  deriving (Show)

instance FromJSON TextDocumentDidOpenParams where
  parseJSON =
    A.withObject "TextDocumentDidOpenParams" <| \v ->
      v .: "textDocument" >>= \subV ->
        Misc.curryTriple TextDocumentDidOpenParams <$> subV .: "uri" <*>
        subV .: "version" <*>
        subV .: "text"

-- EXIT --
exit :: Text
exit = "exit"

-- METHODS --
data NotificationMethod
  = TextDocumentDidOpen Value
  | Exit
  deriving (Show)

decoder :: HM.HashMap Text Value -> Text -> Parser NotificationMethod
decoder v key
  | key == textDocumentDidOpen = TextDocumentDidOpen <$> v .: "params"
  | key == exit = return Exit
  | otherwise = fail "Unknown notificaiton method"

instance FromJSON NotificationMethod where
  parseJSON =
    A.withObject "NotificationMethod" <| \v -> v .: "method" >>= decoder v
