{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Position
  ( Position(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS

newtype Position =
  Position (Int, Int)
  deriving (Show)

instance FromJSON Position where
  parseJSON =
    A.withObject "Position" $ \v ->
        (curry Position <$> v .: "line" <*> v .: "character")
      <|> (curry Position <$> v .: "line" <*> v .: "column")

instance ToJSON Position where
  toJSON (Position (line, character)) =
    A.object ["line" .= line, "character" .= character]
