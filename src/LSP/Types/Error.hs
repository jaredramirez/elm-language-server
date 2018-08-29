module LSP.Types.Error
  ( encode
  ) where

import           Data.Aeson           (Value)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Types            (Error)
import qualified LSP.Types            as Types

toCode :: Error -> Int
toCode err =
  case err of
    Types.ParseError           -> -32700
    Types.InvalidRequest       -> -32600
    Types.MethodNotFound       -> -32601
    Types.InvalidParams        -> -32602
    Types.InternalError        -> -32603
    Types.ServerErrorStart     -> -32099
    Types.ServerErrorEnd       -> -32000
    Types.ServerNotInitialized -> -32002
    Types.UnknownErrorCode     -> -32001
    Types.RequestCancelled     -> -32800

instance A.ToJSON Error where
  toJSON err = A.toJSON (toCode err)

encode :: Error -> BS.ByteString
encode = A.encode
