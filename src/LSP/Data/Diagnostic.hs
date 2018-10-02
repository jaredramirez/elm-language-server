{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Diagnostic
  ( Diagnostic(..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, (.:), (.=), (.=), (.!=), (.:?))
import qualified Data.Aeson as A
import Data.Text (Text)
import LSP.Data.Range (Range)
import LSP.Data.URI (URI)

data Diagnostic = Diagnostic Range Text Int
 deriving (Show)

instance FromJSON Diagnostic where
  parseJSON =
    A.withObject "Diagnostic" $ \v ->
      return Diagnostic
      <*> v .: "range"
      <*> v .: "message"
      <*> (v .:? "severity" .!= 1)

instance ToJSON Diagnostic where
  toJSON (Diagnostic range message severity) =
    A.object
      [ "range" .= range
      , "message" .= message
      , "source" .= ("elm" :: Text)
      , "severity" .= severity
      ]
