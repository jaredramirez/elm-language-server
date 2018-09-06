module LSP.MessageHandler.NotificationTextDocumentDidOpen
  ( handler
  ) where

import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as A
import qualified Data.HashMap.Strict         as HM
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified LSP.Data.Error              as Error
import           LSP.Data.NotificationMethod (TextDocumentDidOpenParams)
import qualified LSP.Data.NotificationMethod as NotificationMethod
import qualified LSP.Data.OutgoingMessage    as OutgoingMessage
import           LSP.Data.State              (State)
import qualified LSP.Data.State              as State
import           LSP.Log                     (LogState)
import qualified LSP.Log                     as Log
import           LSP.MessageHandler.Misc     (HandlerResult,
                                              makeNotificationError)
import           Misc                        ((<|), (|>))

handler :: State -> LogState -> Value -> IO HandlerResult
handler state logState paramsJson =
  let toNotificationError error message =
        return (makeNotificationError (Just state) logState error message)
      params :: A.Result TextDocumentDidOpenParams
      params = A.fromJSON paramsJson
  in case params of
       A.Error error ->
         toNotificationError Error.InvalidParams (Text.pack error)
       A.Success (NotificationMethod.TextDocumentDidOpenParams (uri, version, text)) ->
         let documentText = State.DocumentText (version, text)
             nextState =
               state |>
               State.updateDocumentText
                 (HM.alter
                    (const <| Just documentText)
                    uri
                    (State._documentText state)) |>
               id
         in return (Just nextState, logState, Nothing)
