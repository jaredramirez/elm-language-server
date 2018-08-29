module LSP.Misc
  ( errorToInt
  ) where

import           LSP.Types (Error)
import qualified LSP.Types as T

errorToInt :: Error -> Int
errorToInt method =
  case method of
    T.ParseError           -> -32700
    T.InvalidRequest       -> -32600
    T.MethodNotFound       -> -32601
    T.InvalidParams        -> -32602
    T.InternalError        -> -32603
    T.ServerErrorStart     -> -32099
    T.ServerErrorEnd       -> -32000
    T.ServerNotInitialized -> -32002
    T.UnknownErrorCode     -> -32001
    T.RequestCancelled     -> -32800
