{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Location
  ( Location(..)
  , decode
  ) where

import           Data.Aeson           (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           LSP.Data.Range       (Range)

newtype Location =
  Location (Text, Range)

instance FromJSON Location where
  parseJSON =
    A.withObject "Location" $ \v ->
      curry Location <$> v .: "uri" <*> v .: "range"

decode :: BS.ByteString -> Either String Location
decode = A.eitherDecode'

instance ToJSON Location where
  toJSON (Location (uri, range)) = A.object ["uri" .= uri, "range" .= range]

encode :: Location -> BS.ByteString
encode = A.encode
