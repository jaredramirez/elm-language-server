{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.Method
  ( decode
  ) where

import           Data.Aeson           (FromJSON, Value)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Types            (Method)
import qualified LSP.Types            as Types
import           Misc                 ((<|))

instance FromJSON Method where
  parseJSON =
    A.withText "Incoming Message" <| \case
      "$/cancelRequest" -> return Types.CancelRequest
      _ -> fail "Did not have \"jsonrpc\" of \"2.0\""

decode :: BS.ByteString -> Either String Method
decode = A.eitherDecode'
