{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import           Control.Exception                                  (SomeException,
                                                                     catch)
import qualified Data.ByteString.Lazy                               as BS
import           Data.Semigroup                                     ((<>))
import           Data.Text                                          (Text)
import qualified Data.Text                                          as Text
import qualified LSP.Data.Error                                     as Error
import           LSP.Data.IncomingMessage                           (IncomingMessage)
import qualified LSP.Data.IncomingMessage                           as IncomingMessage
import qualified LSP.Data.NotificationMethod                        as NotificationMethod
import qualified LSP.Data.RequestMethod                             as RequestMethod
import           LSP.Data.State                                     (State)
import           LSP.Log                                            (LogState)
import qualified LSP.Log                                            as Log
import           LSP.MessageHandler.Misc                            (makeNotificationError,
                                                                     makeRequestError)
import qualified LSP.MessageHandler.NotificationTextDocumentDidOpen as NotifTextDocumentDidOpen
import qualified LSP.MessageHandler.RequestInitialize               as RequestInitialize
import qualified LSP.MessageHandler.RequestTextDocumentHover        as RequestTextDocumentHover
import           Misc                                               ((<|), (|>))

handler ::
     Maybe State
  -> LogState
  -> IncomingMessage
  -> IO (Maybe State, LogState, Maybe BS.ByteString)
handler maybeState logState message =
  let toNotificationError error message =
        return (makeNotificationError maybeState logState error message)
      toRequestError id error message =
        return (makeRequestError maybeState logState id error message)
  in case (maybeState, message) of
       (Nothing, IncomingMessage.RequestMessage id (RequestMethod.Initialize paramsJson)) ->
         RequestInitialize.handler logState id paramsJson
       (Just state, IncomingMessage.RequestMessage id (RequestMethod.TextDocumentHover paramsJson)) ->
         RequestTextDocumentHover.handler state logState id paramsJson
       (Just state, IncomingMessage.NotificationMessage (NotificationMethod.TextDocumentDidOpen paramsJson)) ->
         NotifTextDocumentDidOpen.handler state logState paramsJson
       (Just _, IncomingMessage.NotificationMessage NotificationMethod.Initialized) ->
         return (maybeState, logState, Nothing)
       (Nothing, _) ->
         toNotificationError Error.ServerNotInitialized "Server Not Initialized"
       (Just _, message) ->
         return
           ( maybeState
           , Log.logDeferred "Method not implemented" logState
           , Nothing)
