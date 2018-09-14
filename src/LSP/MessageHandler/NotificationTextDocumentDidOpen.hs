{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler.NotificationTextDocumentDidOpen
  ( handler
  ) where

import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as A
import qualified Data.HashMap.Strict         as HM
import qualified Data.List                   as List
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified LSP.Data.Error              as Error
import           LSP.Data.NotificationMethod (TextDocumentDidOpenParams)
import qualified LSP.Data.NotificationMethod as NotificationMethod
import qualified LSP.Data.OutgoingMessage    as OutgoingMessage
import           LSP.Data.State              (Document, DocumentText, State)
import qualified LSP.Data.State              as State
import qualified LSP.Data.URI                as URI
import           LSP.Log                     (LogState)
import qualified LSP.Log                     as Log
import           LSP.MessageHandler.Misc     (HandlerResult)
import qualified LSP.MessageHandler.Misc     as Misc
import           Misc                        ((<|), (|>))
import qualified System.Directory            as Dir

handler :: State -> LogState -> Value -> IO HandlerResult
handler state logState paramsJson =
  let toNotificationError error message =
        return (Misc.makeNotificationError (Just state) logState error message)
      params :: A.Result TextDocumentDidOpenParams
      params = A.fromJSON paramsJson
  in case params of
       A.Error error ->
         toNotificationError Error.InvalidParams (Text.pack error)
       A.Success (NotificationMethod.TextDocumentDidOpenParams (uri, version, text)) ->
         case URI.decodePath uri of
           Left message ->
             toNotificationError Error.InvalidParams (Text.pack message)
           Right filePathText ->
             let documentText = State.DocumentText (version, text)
                 filePath = Text.unpack filePathText
                 exectuable = Misc.findElmExectuable filePath
             in exectuable >>= \case
                  Left message ->
                    return
                      (Misc.makeNotificationError
                         (Just state)
                         logState
                         Error.InternalError
                         message)
                  Right exectuableValue ->
                    let nextState =
                          upsertDocument
                            uri
                            documentText
                            (Text.pack exectuableValue)
                            state
                    in return (Just nextState, logState, Nothing)

upsertDocument :: Text -> DocumentText -> Text -> State -> State
upsertDocument uri documentText exectuable state =
  let document = State.Document documentText exectuable
  in State.updateDocuments
       (HM.alter (const (Just document)) uri (State._documents state))
       state
