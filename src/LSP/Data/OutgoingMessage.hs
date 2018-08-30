{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.OutgoingMessage
  (
  ) where

import           Data.Aeson             (Value, (.=))
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Maybe             as Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           LSP.Data.OutgoingError (OutgoingError)
import           Misc                   ((<|), (|>))

newtype OutgoingMessage =
  ResponseMessage (Maybe Text, Maybe OutgoingError)

encodeId :: Maybe Text -> [(Text, Value)]
encodeId id =
  case id of
    Just value -> ["id" .= id]
    Nothing    -> [("id", A.Null)]

encodeError :: OutgoingError -> [(Text, Value)]
encodeError error = ["error" .= error]

instance A.ToJSON OutgoingMessage where
  toJSON (ResponseMessage (maybeId, maybeError)) =
    let id = maybeId |> encodeId
        error = maybeError |> fmap encodeError |> Maybe.fromMaybe []
    in A.object (id ++ error)

encode :: OutgoingMessage -> BS.ByteString
encode = A.encode
