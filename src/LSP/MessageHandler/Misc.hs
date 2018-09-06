module LSP.MessageHandler.Misc
  ( HandlerResult
  , makeNotificationError
  , makeRequestError
  ) where

import qualified Data.ByteString.Lazy     as BS
import           Data.Text                (Text)
import           LSP.Data.Error           (Error)
import           LSP.Data.OutgoingError   (OutgoingError)
import qualified LSP.Data.OutgoingError   as OutgoingError
import           LSP.Data.OutgoingMessage (OutgoingMessage)
import qualified LSP.Data.OutgoingMessage as OutgoingMessage
import           LSP.Data.State           (State)
import qualified LSP.Data.State           as State
import           LSP.Log                  (LogState)
import qualified LSP.Log                  as Log

type HandlerResult = (Maybe State, LogState, Maybe BS.ByteString)

makeNotificationError ::
     Maybe State -> LogState -> Error -> Text -> HandlerResult
makeNotificationError maybeState logState error text =
  let outgoingError = OutgoingError.ResponseError (error, text)
      outgoingMessage :: OutgoingMessage ()
      outgoingMessage =
        OutgoingMessage.ResponseMessage (Nothing, Nothing, Just outgoingError)
  in ( maybeState
     , Log.logDeferred text logState
     , Just (OutgoingMessage.encode outgoingMessage))

makeRequestError ::
     Maybe State -> LogState -> Text -> Error -> Text -> HandlerResult
makeRequestError maybeState logState id error text =
  let outgoingError = OutgoingError.ResponseError (error, text)
      outgoingMessage :: OutgoingMessage ()
      outgoingMessage =
        OutgoingMessage.ResponseMessage (Just id, Nothing, Just outgoingError)
  in ( maybeState
     , Log.logDeferred text logState
     , Just (OutgoingMessage.encode outgoingMessage))
