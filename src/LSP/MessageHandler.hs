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
import           LSP.Data.Message                                   (Message)
import qualified LSP.Data.Message                                   as Message
import qualified LSP.Data.NotificationMethod                        as NotificationMethod
import qualified LSP.Data.RequestMethod                             as RequestMethod
import qualified LSP.MessageHandler.NotificationTextDocumentDidOpen as NotifTextDocumentDidOpen
import qualified LSP.MessageHandler.RequestInitialize               as RequestInitialize
import           LSP.Model                                          (Model)
import qualified LSP.Model                                          as M
import           LSP.Update                                         (Msg)
import qualified LSP.Update                                         as U
import           Misc                                               ((<|), (|>))

handler :: Model -> Message result -> IO Msg
handler model incomingMessage =
  case (M._initialized model, incomingMessage) of
    (False, Message.RequestMessage id (RequestMethod.Initialize paramsJson)) ->
      RequestInitialize.handler id paramsJson

    (False, _) ->
      U.SendNotifError Error.ServerNotInitialized "Server Not Initialized"
        |> return

    (True, Message.NotificationMessage NotificationMethod.Initialized) ->
      U.NoOp
        |> return

    (True, Message.RequestMessage _ RequestMethod.Shutdown) ->
      U.RequestShutDown
        |> return

    (True, Message.NotificationMessage NotificationMethod.Exit) ->
      U.Exit
        |> return

    (True, _) ->
      U.SendNotifError Error.MethodNotFound "Method not implemented"
        |> return
