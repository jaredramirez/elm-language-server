{-# LANGUAGE OverloadedStrings #-}

module LSP.Types.OutgoingMessage
  (
  ) where

import           Data.Aeson              (Value, (.=))
import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Maybe              as Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import           LSP.Types               (OutgoingError, OutgoingMessage)
import qualified LSP.Types               as Types
import qualified LSP.Types.OutgoingError
import           Misc                    ((<|), (|>))

encodeId :: Maybe Text -> [(Text, Value)]
encodeId id =
  case id of
    Just value -> ["id" .= id]
    Nothing    -> [("id", A.Null)]

encodeError :: OutgoingError -> [(Text, Value)]
encodeError error = ["error" .= error]

instance A.ToJSON OutgoingMessage where
  toJSON (Types.ResponseMessage (maybeId, maybeError)) =
    let id = maybeId |> encodeId
        error = maybeError |> fmap encodeError |> Maybe.fromMaybe []
    in A.object (id ++ error)

encode :: OutgoingMessage -> BS.ByteString
encode = A.encode
