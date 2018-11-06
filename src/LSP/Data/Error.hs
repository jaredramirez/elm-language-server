module LSP.Data.Error
  ( Error(..)
  ) where

import           Data.Aeson           (Value)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Utils     as AUtils
import qualified Data.ByteString.Lazy as BS
import           Misc                 ((<|), (|>))
import qualified Misc

data Error
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerErrorStart
  | ServerErrorEnd
  | ServerNotInitialized
  | UnknownErrorCode
  | RequestCancelled deriving (Show)

fromCode :: Int -> Maybe Error
fromCode err =
  case err of
    -32700 -> Just <| ParseError
    -32600 -> Just <| InvalidRequest
    -32601 -> Just <| MethodNotFound
    -32602 -> Just <| InvalidParams
    -32603 -> Just <| InternalError
    -32099 -> Just <| ServerErrorStart
    -32000 -> Just <| ServerErrorEnd
    -32002 -> Just <| ServerNotInitialized
    -32001 -> Just <| UnknownErrorCode
    -32800 -> Just <| RequestCancelled
    _ ->
      Nothing


instance A.FromJSON Error where
  parseJSON =
    A.withScientific "Error" $ \num ->
      let int_ = num |> AUtils.floatingOrInteger |> Misc.toInt
      in case fromCode int_ of
          Nothing ->
            fail "invalid error"

          Just error ->
            return error

toCode :: Error -> Int
toCode err =
  case err of
    ParseError           -> -32700
    InvalidRequest       -> -32600
    MethodNotFound       -> -32601
    InvalidParams        -> -32602
    InternalError        -> -32603
    ServerErrorStart     -> -32099
    ServerErrorEnd       -> -32000
    ServerNotInitialized -> -32002
    UnknownErrorCode     -> -32001
    RequestCancelled     -> -32800

instance A.ToJSON Error where
  toJSON err = A.toJSON (toCode err)
