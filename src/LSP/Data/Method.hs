{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Method
  ( Method(..)
  , decode
  ) where

import           Data.Aeson           (FromJSON, Value)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Misc                 ((<|))

data Method =
  CancelRequest
  deriving (Show)

instance FromJSON Method where
  parseJSON =
    A.withText "Method" <| \case
      "$/cancelRequest" -> return CancelRequest
      _ -> fail "Did not have \"jsonrpc\" of \"2.0\""

decode :: BS.ByteString -> Either String Method
decode = A.eitherDecode'
