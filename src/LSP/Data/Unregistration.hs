{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Unregistration
  ( Unregistration(..)
  ) where

import           Data.Aeson           (FromJSON, ToJSON, Value, (.:), (.:?))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           LSP.Data.Method      (Method)
import qualified LSP.Data.Misc        as Misc

newtype Unregistration =
  Unregistration (Text, Method)

instance FromJSON Unregistration where
  parseJSON =
    A.withObject "Unregistration" $ \v ->
      curry Unregistration <$> v .: "id" <*> v .: "method"

decode :: BS.ByteString -> Either String Unregistration
decode = A.eitherDecode'
