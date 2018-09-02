{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import qualified Data.Aeson               as A
import qualified Data.ByteString.Lazy     as BS
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Data.Capabilities    as Capabilities
import           LSP.Data.Error           (Error)
import qualified LSP.Data.Error           as Error
import           LSP.Data.IncomingMessage (IncomingMessage)
import qualified LSP.Data.IncomingMessage as IncomingMessage
import           LSP.Data.IncomingMethod  (InitializeParams)
import qualified LSP.Data.IncomingMethod  as IncomingMethod
import           LSP.Data.OutgoingError   (OutgoingError)
import qualified LSP.Data.OutgoingError   as OutgoingError
import           LSP.Data.OutgoingMessage (OutgoingMessage)
import qualified LSP.Data.OutgoingMessage as OutgoingMessage
import           LSP.Data.State           (State)
import qualified LSP.Data.State           as State
import qualified LSP.Data.URI             as URI
import           LSP.Log                  (LogState)
import qualified LSP.Log                  as Log
import           Misc                     ((<|), (|>))
import           Prelude                  hiding (log)

handler ::
     Maybe State
  -> LogState
  -> IncomingMessage
  -> (Maybe State, LogState, Maybe BS.ByteString)
handler maybeState initialLogState message =
  let logState =
        Log.logDeferred ("Got message: " <> Log.toText message) initialLogState
      toError = makeError maybeState logState Nothing
  in case (maybeState, message) of
       (Nothing, IncomingMessage.RequestMessage id (IncomingMethod.Initialize paramsJson)) ->
         let params :: A.Result InitializeParams
             params = A.fromJSON paramsJson
         in case params of
              A.Error error ->
                toError Error.InvalidParams "Invalid Initialized Params"
              A.Success (IncomingMethod.InitializeParams rootUri) ->
                case URI.decodePath rootUri of
                  Left message ->
                    toError Error.InvalidParams (Text.pack message)
                  Right rootUriDecoded ->
                    let nextLogState =
                          logState |> Log.setDirPath rootUriDecoded |>
                          Log.logDeferred ("rootUri: " <> rootUriDecoded)
                        outgoingMessage =
                          OutgoingMessage.ResponseMessage
                            (Just id, Just Capabilities.capabilities, Nothing)
                    in ( Just (State.init rootUriDecoded)
                       , nextLogState
                       , Just (A.encode outgoingMessage))
       (Nothing, _) ->
         toError Error.ServerNotInitialized "Server Not Initialized"
       (Just _, message) ->
         ( maybeState
         , Log.logDeferred "Method not implemented" logState
         , Nothing)

makeError ::
     Maybe State
  -> LogState
  -> Maybe Text
  -> Error
  -> Text
  -> (Maybe State, LogState, Maybe BS.ByteString)
makeError maybeState logState maybeId error text =
  let outgoingError :: OutgoingError
      outgoingError = OutgoingError.ResponseError (error, text)
      outgoingMessage :: OutgoingMessage ()
      outgoingMessage =
        OutgoingMessage.ResponseMessage (maybeId, Nothing, Just outgoingError)
  in ( maybeState
     , Log.logDeferred text logState
     , Just (A.encode outgoingMessage))
