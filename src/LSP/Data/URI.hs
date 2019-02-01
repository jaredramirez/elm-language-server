{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LSP.Data.URI
  ( URI(..)
  , decodePath
  ) where


import Data.Semigroup ((<>))
import qualified Data.Aeson as A
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Hashable as H
import Misc ((<|))


newtype URI =
  URI Text
  deriving (Show, Eq, Ord, H.Hashable)


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
    A.toJSON (prefix <> uri)
