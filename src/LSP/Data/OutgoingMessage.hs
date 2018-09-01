{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.OutgoingMessage
  (
  ) where

import           Data.Aeson             (ToJSON, Value, (.=))
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Maybe             as Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           LSP.Data.OutgoingError (OutgoingError)
import           Misc                   ((<|), (|>))

newtype OutgoingMessage result =
  ResponseMessage (Maybe Text, Maybe result, Maybe OutgoingError)

encodeId :: Maybe Text -> [(Text, Value)]
encodeId id =
  case id of
    Just value -> ["id" .= id]
    Nothing    -> [("id", A.Null)]

encodeResult :: ToJSON result => result -> [(Text, Value)]
encodeResult result = ["result" .= result]

encodeError :: OutgoingError -> [(Text, Value)]
encodeError error = ["error" .= error]

instance ToJSON result => ToJSON (OutgoingMessage result) where
  toJSON (ResponseMessage (maybeId, maybeResult, maybeError)) =
    let id = maybeId |> encodeId
        result = maybeResult |> encodeResult
        error = maybeError |> fmap encodeError |> Maybe.fromMaybe []
    in A.object (id ++ result ++ error)

encode :: ToJSON result => OutgoingMessage result -> BS.ByteString
encode = A.encode
