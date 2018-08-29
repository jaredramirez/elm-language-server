{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.Position
  ( decode
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Types            (Position)
import qualified LSP.Types            as Types

instance FromJSON Position where
  parseJSON =
    A.withObject "Position" $ \v ->
      curry Types.Position <$> v .: "line" <*> v .: "character"

decode :: BS.ByteString -> Either String Position
decode = A.eitherDecode'
