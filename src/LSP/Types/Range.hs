{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.Range
  ( decode
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Types            (Range)
import qualified LSP.Types            as Types
import qualified LSP.Types.Position

instance FromJSON Range where
  parseJSON =
    A.withObject "Range" $ \v ->
      curry Types.Range <$> v .: "start" <*> v .: "end"

decode :: BS.ByteString -> Either String Range
decode = A.eitherDecode'
