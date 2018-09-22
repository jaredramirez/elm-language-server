{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.MessageError
  ( MessageError(..)
  ) where

import           Data.Aeson           (Value, (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           LSP.Data.Error       (Error)

data MessageError =
  MessageError Error Text
  deriving (Show)

instance A.ToJSON MessageError where
  toJSON (MessageError err message) =
    A.object ["code" .= err, "message" .= message]

instance A.FromJSON MessageError where
  parseJSON =
    A.withObject "Message Error" $ \v ->
      MessageError <$> v .: "code" <*> v .: "message"
