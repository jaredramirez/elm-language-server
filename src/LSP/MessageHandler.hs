{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import           Data.Aeson                  (ToJSON)
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Lazy        as BS
import qualified Data.HashMap.Strict         as HM
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified LSP.Data.Capabilities       as Capabilities
import           LSP.Data.Error              (Error)
import qualified LSP.Data.Error              as Error
import           LSP.Data.IncomingMessage    (IncomingMessage)
import qualified LSP.Data.IncomingMessage    as IncomingMessage
import           LSP.Data.NotificationMethod (NotificationMethod,
                                              TextDocumentDidOpenParams)
import qualified LSP.Data.NotificationMethod as NotificationMethod
import           LSP.Data.OutgoingError      (OutgoingError)
import qualified LSP.Data.OutgoingError      as OutgoingError
import           LSP.Data.OutgoingMessage    (OutgoingMessage)
import qualified LSP.Data.OutgoingMessage    as OutgoingMessage
import           LSP.Data.RequestMethod      (InitializeParams)
import qualified LSP.Data.RequestMethod      as RequestMethod
import           LSP.Data.State              (State)
import qualified LSP.Data.State              as State
import qualified LSP.Data.URI                as URI
import           LSP.Log                     (LogState)
import qualified LSP.Log                     as Log
import           Misc                        ((<|), (|>))
import           Prelude                     hiding (log)

handler ::
     Maybe State
  -> LogState
  -> IncomingMessage
  -> (Maybe State, LogState, Maybe BS.ByteString)
handler maybeState initialLogState message =
  let logState =
        Log.logDeferred ("Got message: " <> Log.toText message) initialLogState
      toNotificationError = makeNotificationError maybeState logState
      toRequestError = makeRequestError maybeState logState
  in case (maybeState, message) of
       (Nothing, IncomingMessage.RequestMessage id (RequestMethod.Initialize paramsJson)) ->
         let params :: A.Result InitializeParams
             params = A.fromJSON paramsJson
         in case params of
              A.Error error ->
                toRequestError id Error.InvalidParams (Text.pack error)
              A.Success (RequestMethod.InitializeParams rootUri) ->
                case URI.decodePath rootUri of
                  Left message ->
                    toRequestError id Error.InvalidParams (Text.pack message)
                  Right rootUriDecoded ->
                    let nextLogState = logState |> Log.setDirPath rootUriDecoded
                        outgoingMessage =
                          OutgoingMessage.ResponseMessage
                            (Just id, Just Capabilities.capabilities, Nothing)
                    in ( Just (State.init rootUriDecoded)
                       , nextLogState
                       , Just (OutgoingMessage.encode outgoingMessage))
       (Just state, IncomingMessage.NotificationMessage (NotificationMethod.TextDocumentDidOpen paramsJson)) ->
         let params :: A.Result TextDocumentDidOpenParams
             params = A.fromJSON paramsJson
         in case params of
              A.Error error ->
                toNotificationError Error.InvalidParams (Text.pack error)
              A.Success (NotificationMethod.TextDocumentDidOpenParams (uri, version, text)) ->
                let documentText = State.DocumentText (version, text)
                    nextState =
                      State.updateDocumentText
                        (HM.alter
                           (const <| Just documentText)
                           uri
                           (State.documentText state))
                        state
                in (Just nextState, logState, Nothing)
       (Nothing, _) ->
         toNotificationError Error.ServerNotInitialized "Server Not Initialized"
       (Just _, message) ->
         ( maybeState
         , Log.logDeferred "Method not implemented" logState
         , Nothing)

makeNotificationError ::
     Maybe State
  -> LogState
  -> Error
  -> Text
  -> (Maybe State, LogState, Maybe BS.ByteString)
makeNotificationError maybeState logState error text =
  let outgoingError = OutgoingError.ResponseError (error, text)
      outgoingMessage :: OutgoingMessage ()
      outgoingMessage =
        OutgoingMessage.ResponseMessage (Nothing, Nothing, Just outgoingError)
  in ( maybeState
     , Log.logDeferred text logState
     , Just (OutgoingMessage.encode outgoingMessage))

makeRequestError ::
     Maybe State
  -> LogState
  -> Text
  -> Error
  -> Text
  -> (Maybe State, LogState, Maybe BS.ByteString)
makeRequestError maybeState logState id error text =
  let outgoingError = OutgoingError.ResponseError (error, text)
      outgoingMessage :: OutgoingMessage ()
      outgoingMessage =
        OutgoingMessage.ResponseMessage ((Just id), Nothing, Just outgoingError)
  in ( maybeState
     , Log.logDeferred text logState
     , Just (OutgoingMessage.encode outgoingMessage))
