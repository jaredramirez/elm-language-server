module LSP.MessageHandler.RequestInitialize
  ( handler
  ) where

import           Data.Aeson               (Value)
import qualified Data.Aeson               as A
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Data.Capabilities    as Capabilities
import qualified LSP.Data.Error           as Error
import qualified LSP.Data.OutgoingMessage as OutgoingMessage
import           LSP.Data.RequestMethod   (InitializeParams)
import qualified LSP.Data.RequestMethod   as RequestMethod
import           LSP.Data.State           (State)
import qualified LSP.Data.State           as State
import qualified LSP.Data.URI             as URI
import           LSP.Log                  (LogState)
import qualified LSP.Log                  as Log
import           LSP.MessageHandler.Misc  (HandlerResult, makeRequestError)
import           Misc                     ((<|), (|>))

handler :: LogState -> Text -> Value -> IO HandlerResult
handler logState id paramsJson =
  let toRequestError error message =
        return (makeRequestError Nothing logState id error message)
      params :: A.Result InitializeParams
      params = A.fromJSON paramsJson
  in case params of
       A.Error error -> toRequestError Error.InvalidParams (Text.pack error)
       A.Success (RequestMethod.InitializeParams rootUri) ->
         case URI.decodePath rootUri of
           Left message ->
             toRequestError Error.InvalidParams (Text.pack message)
           Right rootUriDecoded ->
             let nextLogState = logState |> Log.setDirPath rootUriDecoded
                 outgoingMessage =
                   OutgoingMessage.ResponseMessage
                     (Just id, Just Capabilities.capabilities, Nothing)
             in return
                  ( Just (State.init rootUriDecoded)
                  , nextLogState
                  , Just (OutgoingMessage.encode outgoingMessage))
