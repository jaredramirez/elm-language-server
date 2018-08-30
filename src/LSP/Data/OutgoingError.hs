{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.OutgoingError
  ( OutgoingError(..)
  , encode
  ) where

import           Data.Aeson           (Value, (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           LSP.Data.Error       (Error)

newtype OutgoingError =
  ResponseError (Error, Text)

instance A.ToJSON OutgoingError where
  toJSON (ResponseError (err, message)) =
    A.object ["code" .= err, "message" .= message]

encode :: OutgoingError -> BS.ByteString
encode = A.encode
