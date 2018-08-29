{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.IncomingMessage
  ( decode
  ) where

import           Data.Aeson           (Value)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           LSP.Types            (IncomingMessage)
import qualified LSP.Types            as Types
import qualified LSP.Types.Method
import           Misc                 ((<|), (|>))

jsonrpcKey :: Text
jsonrpcKey = "jsonrpc"

idKey :: Text
idKey = "id"

methodKey :: Text
methodKey = "method"

string :: Maybe A.Value -> Maybe Text
string value =
  case value of
    Just (A.String str) -> Just str
    _                   -> Nothing

stringOrNum :: Maybe A.Value -> Maybe Text
stringOrNum value =
  case value of
    Just (A.String str) -> Just str
    Just (A.Number num) -> num |> show |> T.pack |> Just
    _                   -> Nothing

instance A.FromJSON IncomingMessage where
  parseJSON =
    A.withObject "Incoming Message" $ \jsonObject ->
      let maybeJsonrpc = jsonObject |> HM.lookup jsonrpcKey |> string
          maybeId = jsonObject |> HM.lookup idKey |> stringOrNum
          maybeMethod = jsonObject |> HM.lookup jsonrpcKey
      in case (maybeJsonrpc, maybeId, maybeMethod) of
           (Just "2.0", Just id, Just methodValue) ->
             Types.RequestMessage id <$> A.parseJSON methodValue
           (Just "2.0", Nothing, Just methodValue) ->
             Types.NotificationMessage <$> A.parseJSON methodValue
           (_, _, _) -> fail "Did not have \"jsonrpc\" of \"2.0\""

decode :: BS.ByteString -> Either String IncomingMessage
decode = A.eitherDecode'
