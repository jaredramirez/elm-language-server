{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Message
  ( Message(..)
  , decode
  , encode
  ) where

import           Control.Applicative         ((<|>))
import           Data.Aeson                  (FromJSON, ToJSON, Value, (.:), (.:!), (.=))
import qualified Data.Aeson                  as A
import           Data.Aeson.Types            (Parser)
import qualified Data.Aeson.Utils            as AUtils
import qualified Data.ByteString             as BSStrict
import qualified Data.ByteString.Lazy        as BS
import qualified Data.HashMap.Strict         as HM
import qualified Data.Maybe                  as Maybe
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified LSP.Data.Header             as Header
import           LSP.Data.MessageError       (MessageError)
import           LSP.Data.NotificationMethod (NotificationMethod)
import qualified LSP.Data.NotificationMethod as NotificationMethod
import           LSP.Data.RequestMethod      (RequestMethod)
import qualified LSP.Data.RequestMethod      as RequestMethod
import           Misc                        ((|>))
import qualified Misc
import           System.IO                   (Handle)
import qualified System.IO                   as IO

data Message result
  = RequestMessage Text RequestMethod
  | NotificationMessage NotificationMethod
  | ResponseMessage (Maybe Text) (Maybe result) (Maybe MessageError)
  deriving (Show)

-- DECODING --

jsonrpcDecoder :: HM.HashMap Text Value -> Parser Text
jsonrpcDecoder v = v .: "jsonrpc"

withJsonRcp :: HM.HashMap Text Value -> (HM.HashMap Text Value -> Parser (Message result)) -> Parser (Message result)
withJsonRcp v finish =
  jsonrpcDecoder v >>= \case
    "2.0" ->
      finish v

    _ ->
      fail "\"jsonrpc\" must be \"2.0\""

requestMessageDecoder :: FromJSON result => HM.HashMap Text Value -> Parser (Message result)
requestMessageDecoder v =
  let id = HM.lookup "id" v
  in case id of
      Just (A.String text) ->
        RequestMessage text <$> A.parseJSON (A.Object v)

      Just (A.Number num) ->
        RequestMessage
          (num |> AUtils.floatingOrInteger |> Misc.toInt |> show |> T.pack) <$>
        A.parseJSON (A.Object v)

      Just _ ->
        fail "\"id\" must be string or number"

      Nothing ->
        fail "\"id\" is required for a request message"

notificationMessageDecoder :: FromJSON result => HM.HashMap Text Value -> Parser (Message result)
notificationMessageDecoder v =
  NotificationMessage <$> A.parseJSON (A.Object v)

responseMessageDecoder :: FromJSON result => HM.HashMap Text Value -> Parser (Message result)
responseMessageDecoder v =
  let id = HM.lookup "id" v
      finish maybeId =
        ResponseMessage maybeId <$> v .:! "result" <*> v .:! "error"
  in case id of
      Just (A.String text) ->
        finish (Just text)

      Just (A.Number num) ->
        finish (Just (num |> AUtils.floatingOrInteger |> Misc.toInt |> show |> T.pack))

      Just A.Null ->
        finish Nothing

      _ ->
        fail "\"id\" must be string, number or null"


instance FromJSON result => FromJSON (Message result) where
  parseJSON =
    A.withObject "Incoming Message" $ \v ->
          withJsonRcp v requestMessageDecoder
      <|> withJsonRcp v notificationMessageDecoder
      <|> responseMessageDecoder v

decode :: Handle ->  IO (Either String (Message Value))
decode handle =
  getLineBSLazy >>= \header ->
    getLineBSLazy >>= \endLine ->
      let eitherContentLength =
            Header.decode header >>= \contentLength ->
              Header.decodeEndLine endLine >> return contentLength
      in case eitherContentLength of
          Left error ->
            return (Left error)

          Right contentLength ->
            BS.hGet IO.stdin contentLength >>= \json ->
              return (A.eitherDecode' json)

getLineBSLazy :: IO BS.ByteString
getLineBSLazy = BS.fromStrict <$> BSStrict.getLine

-- ENCODING --
--
encodeJsonrpc :: (Text, Value)
encodeJsonrpc = "jsonrpc" .= ("2.0" :: Text)

encodeId :: Maybe Text -> (Text, Value)
encodeId id =
  case id of
    Just value -> "id" .= id
    Nothing    -> ("id", A.Null)

encodeResult :: ToJSON result => result -> [(Text, Value)]
encodeResult result = ["result" .= result]

encodeError :: MessageError -> [(Text, Value)]
encodeError error = ["error" .= error]

encodeRequestMessage :: Text -> RequestMethod -> Value
encodeRequestMessage id method =
  let pairs = RequestMethod.toPairs method
  in A.object ([ "id" .= id , encodeJsonrpc ] ++ pairs)

encodeNotificationMessage :: NotificationMethod -> Value
encodeNotificationMessage method =
  let pairs = NotificationMethod.toPairs method
  in A.object (encodeJsonrpc : pairs)

encodeResponseMessage :: ToJSON result => Maybe Text -> Maybe result -> Maybe MessageError -> Value
encodeResponseMessage maybeId maybeResult maybeError =
    let id = maybeId |> encodeId
        result = maybeResult |> fmap encodeResult |> Maybe.fromMaybe []
        error = maybeError |> fmap encodeError |> Maybe.fromMaybe []
    in A.object ([ id, encodeJsonrpc ] ++ result ++ error)

instance ToJSON result => ToJSON (Message result) where
  toJSON message =
    case message of
      RequestMessage id method ->
        encodeRequestMessage id method

      NotificationMessage method ->
        encodeNotificationMessage method

      ResponseMessage maybeId maybeResult maybeError ->
        encodeResponseMessage maybeId maybeResult maybeError


encode :: ToJSON result => Message result -> BS.ByteString
encode outgoingMessage =
  let content = A.encode outgoingMessage
  in Header.encode content <> content
