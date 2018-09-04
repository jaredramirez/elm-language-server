{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Position
  ( Position(..)
  , decode
  , encode
  ) where

import           Data.Aeson           (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS

newtype Position =
  Position (Int, Int)
  deriving (Show)

instance FromJSON Position where
  parseJSON =
    A.withObject "Position" $ \v ->
      curry Position <$> v .: "line" <*> v .: "character"

decode :: BS.ByteString -> Either String Position
decode = A.eitherDecode'

instance ToJSON Position where
  toJSON (Position (line, character)) =
    A.object ["line" .= line, "character" .= character]

encode :: Position -> BS.ByteString
encode = A.encode
