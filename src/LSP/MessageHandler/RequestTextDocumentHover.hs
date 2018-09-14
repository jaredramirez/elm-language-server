module LSP.MessageHandler.RequestTextDocumentHover
  ( handler
  ) where

import           Data.Aeson              (Value)
import qualified Data.Aeson              as A
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified LSP.Data.Error          as Error
import           LSP.Data.RequestMethod  (TextDocumentHoverParams)
import qualified LSP.Data.RequestMethod  as RequestMethod
import           LSP.Data.State          (State)
import qualified LSP.Data.State          as State
import           LSP.Log                 (LogState)
import qualified LSP.Log                 as Log
import           LSP.MessageHandler.Misc (HandlerResult, makeRequestError)
import           Misc                    ((<|), (|>))

handler :: State -> LogState -> Text -> Value -> IO HandlerResult
handler state logState id paramsJson =
  let toRequestError error message =
        return (makeRequestError Nothing logState id error message)
      params :: A.Result TextDocumentHoverParams
      params = A.fromJSON paramsJson
  in case params of
       A.Error error -> toRequestError Error.InvalidParams (Text.pack error)
       A.Success (RequestMethod.TextDocumentHoverParams (textDocumentIdentifier, position)) ->
         return (Just state, logState, Nothing)
