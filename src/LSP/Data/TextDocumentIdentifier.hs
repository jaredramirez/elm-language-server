{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.TextDocumentIdentifier
  ( TextDocumentIdentifier(..)
  , decode
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           Misc                 ((<|))

newtype TextDocumentIdentifier =
  TextDocumentIdentifier Text

instance FromJSON TextDocumentIdentifier where
  parseJSON =
    A.withObject "TextDocumentIdentifier" <| \v ->
      TextDocumentIdentifier <$> v .: "uri"

decode :: BS.ByteString -> Either String TextDocumentIdentifier
decode = A.eitherDecode'
