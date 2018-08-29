{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.OutgoingError
  (
  ) where

import           Data.Aeson           (Value, (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Types            (OutgoingError)
import qualified LSP.Types            as Types
import qualified LSP.Types.Error

instance A.ToJSON OutgoingError where
  toJSON (Types.ResponseError (err, message)) =
    A.object ["code" .= err, "message" .= message]

encode :: OutgoingError -> BS.ByteString
encode = A.encode
