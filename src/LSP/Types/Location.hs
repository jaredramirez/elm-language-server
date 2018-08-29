{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.Location
  ( decode
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Types            (Location)
import qualified LSP.Types            as Types
import qualified LSP.Types.Range

instance FromJSON Location where
  parseJSON =
    A.withObject "Location" $ \v ->
      curry Types.Location <$> v .: "uri" <*> v .: "range"

decode :: BS.ByteString -> Either String Location
decode = A.eitherDecode'
