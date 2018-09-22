{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.URI
  ( URI(..)
  , decodePath
  ) where

import qualified Data.Aeson as A
import Data.Aeson.Types (ToJSON, FromJSON, Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Hashable as H
import Misc ((<|), (|>))

newtype URI =
  URI Text
  deriving (Show, Eq)

prefix :: Text
prefix = "file://"

prefixLength :: Int
prefixLength = Text.length prefix

decodePath :: Text -> Either String Text
decodePath uri =
  let (head, rest) = Text.splitAt prefixLength uri
  in if head == prefix
       then Right rest
       else Left "Invalid URI"

instance FromJSON URI where
  parseJSON =
    A.withText "URI" <| \uri ->
      case decodePath uri of
        Left message ->
          fail message

        Right decoded ->
          return (URI decoded)

instance ToJSON URI where
  toJSON (URI uri) =
    A.toJSON uri

-- So we can use URI as a key in a HashMap --
instance H.Hashable URI where
    hashWithSalt salt (URI text) =
      H.hashWithSalt salt text
