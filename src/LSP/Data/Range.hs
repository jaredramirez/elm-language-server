{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Range
  ( Range(..)
  , decode
  ) where

import           Data.Aeson           (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Data.Position    (Position)

newtype Range =
  Range (Position, Position)

instance FromJSON Range where
  parseJSON =
    A.withObject "Range" $ \v -> curry Range <$> v .: "start" <*> v .: "end"

decode :: BS.ByteString -> Either String Range
decode = A.eitherDecode'

instance ToJSON Range where
  toJSON (Range (start, end)) = A.object ["start" .= start, "end" .= end]

encode :: Range -> BS.ByteString
encode = A.encode
