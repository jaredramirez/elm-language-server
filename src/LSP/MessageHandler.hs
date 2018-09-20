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
import qualified LSP.MessageHandler.NotificationTextDocumentDidOpen as NotifTextDocumentDidOpen
import qualified LSP.MessageHandler.RequestInitialize               as RequestInitialize
import           LSP.Model                                          (Model)
import qualified LSP.Model                                          as M
import           LSP.Update                                         (Msg)
import qualified LSP.Update                                         as U
import           Misc                                               ((<|), (|>))

handler :: Model -> IncomingMessage -> IO Msg
handler model incomingMessage =
  case (M._initialized model, incomingMessage) of
    (False, IncomingMessage.RequestMessage id (RequestMethod.Initialize paramsJson)) ->
      RequestInitialize.handler id paramsJson

    (True, IncomingMessage.RequestMessage _ RequestMethod.Shutdown) ->
      U.RequestShutDown
        |> return

    (True, IncomingMessage.NotificationMessage NotificationMethod.Exit) ->
      U.Exit
        |> return

    (False, _) ->
      U.SendNotifError Error.ServerNotInitialized "Server Not Initialized"
        |> return

    (True, _) ->
      U.SendNotifError Error.MethodNotFound "Method not implemented"
        |> return
