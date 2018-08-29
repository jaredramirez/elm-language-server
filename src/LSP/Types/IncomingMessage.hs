{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.IncomingMessage
  ( decode
  ) where

import           Control.Applicative  ((<|>))
import           Data.Aeson           (Value, (.:))
import qualified Data.Aeson           as A
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           LSP.Types            (IncomingMessage)
import qualified LSP.Types            as Types
import qualified LSP.Types.Method
import           Misc                 ((<|), (|>))

jsonrpcDecoder :: HM.HashMap Text Value -> Parser Text
jsonrpcDecoder v = v .: "jsonrpc"

requestMessageDecoder :: HM.HashMap Text Value -> Parser IncomingMessage
requestMessageDecoder v = Types.RequestMessage <$> v .: "id" <*> v .: "method"

notificationMessageDecoder :: HM.HashMap Text Value -> Parser IncomingMessage
notificationMessageDecoder v = Types.NotificationMessage <$> v .: "method"

instance A.FromJSON IncomingMessage where
  parseJSON =
    A.withObject "Incoming Message" $ \v ->
      jsonrpcDecoder v >>= \case
        "2.0" -> requestMessageDecoder v <|> notificationMessageDecoder v
        _ -> fail "\"jsonrpc\" must be \"2.0\""

decode :: BS.ByteString -> Either String IncomingMessage
decode = A.eitherDecode'
