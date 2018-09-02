{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LSP.Data.IncomingMessage
  ( IncomingMessage(..)
  , decode
  ) where

import           Control.Applicative     ((<|>))
import           Data.Aeson              (FromJSON, Value, (.:), (.:?))
import qualified Data.Aeson              as A
import           Data.Aeson.Types        (Parser)
import qualified Data.Aeson.Utils        as AUtils
import qualified Data.ByteString         as BSStrict
import qualified Data.ByteString.Lazy    as BS
import qualified Data.HashMap.Strict     as HM
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified LSP.Data.Header         as Header
import           LSP.Data.IncomingMethod (IncomingMethod)
import           LSP.Data.Params         (Params)
import           LSP.Log                 (LogState)
import qualified LSP.Log                 as Log
import           Misc                    ((<|), (|>))
import           System.IO               (Handle)
import qualified System.IO               as IO

data IncomingMessage
  = RequestMessage Text
                   IncomingMethod
  | NotificationMessage IncomingMethod
  deriving (Show)

jsonrpcDecoder :: HM.HashMap Text Value -> Parser Text
jsonrpcDecoder v = v .: "jsonrpc"

toInt :: (RealFloat r, Integral i) => Either r i -> Int
toInt num =
  case num of
    Left float     -> fromIntegral (round float)
    Right integral -> fromIntegral integral

requestMessageDecoder :: HM.HashMap Text Value -> Parser IncomingMessage
requestMessageDecoder v =
  let id = HM.lookup "id" v
  in case id of
       Just (A.String text) -> RequestMessage text <$> A.parseJSON (A.Object v)
       Just (A.Number num) ->
         RequestMessage
           (num |> AUtils.floatingOrInteger |> toInt |> show |> T.pack) <$>
         A.parseJSON (A.Object v)
       Just _ -> fail "\"id\" must be string or number"
       Nothing -> fail "\"id\" is required for a request message"

notificationMessageDecoder :: HM.HashMap Text Value -> Parser IncomingMessage
notificationMessageDecoder v = NotificationMessage <$> A.parseJSON (A.Object v)

instance FromJSON IncomingMessage where
  parseJSON =
    A.withObject "Incoming Message" $ \v ->
      jsonrpcDecoder v >>= \case
        "2.0" -> requestMessageDecoder v <|> notificationMessageDecoder v
        _ -> fail "\"jsonrpc\" must be \"2.0\""

decode :: Handle -> LogState -> IO (Either String IncomingMessage, LogState)
decode handle logState =
  getLineBSLazy >>= \header ->
    getLineBSLazy >>= \endLine ->
      let eitherContentLength =
            Header.decode header >>= \contentLength ->
              Header.decodeEndLine endLine >> return contentLength
      in case eitherContentLength of
           Left error -> (Left error, ) <$> Log.log (T.pack error) logState
           Right contentLength ->
             BS.hGet IO.stdin contentLength >>= \json ->
               (A.eitherDecode' json, ) <$> Log.log (Log.toText json) logState

getLineBSLazy :: IO BS.ByteString
getLineBSLazy = BS.fromStrict <$> BSStrict.getLine
