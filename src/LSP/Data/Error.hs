module LSP.Data.Error
  ( Error(..)
  , encode
  ) where

import           Data.Aeson           (Value)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS

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
  | RequestCancelled
  deriving (Show)

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

encode :: Error -> BS.ByteString
encode = A.encode
