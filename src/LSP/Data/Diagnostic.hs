{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Diagnostic
  ( Diagnostic(..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, (.:), (.=), (.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import LSP.Data.Range (Range)

data Diagnostic = Diagnostic Range Text
  deriving (Show)

instance FromJSON Diagnostic where
  parseJSON =
    A.withObject "Diagnostic" $ \v ->
      return Diagnostic
      <*> v .: "range"
      <*> v .: "message"

instance ToJSON Diagnostic where
  toJSON (Diagnostic range message) =
    A.object
      [ "range" .= range
      , "message" .= message
      , "source" .= ("elm make" :: Text)
      ]
