{-# LANGUAGE OverloadedStrings #-}

module LSP.Server
  ( run
  ) where

import qualified Data.ByteString.Lazy        as BS
import qualified Data.Text                   as Text
import qualified LSP.Data.IncomingMessage    as IncomingMessage
import qualified LSP.Data.NotificationMethod as NotificationMethod
import qualified LSP.Data.RequestMethod      as RequestMethod
import           LSP.Data.State              (State)
import qualified LSP.Data.State              as State
import           LSP.Log                     (LogState)
import qualified LSP.Log                     as Log
import qualified LSP.MessageHandler          as MessageHandler
import           Misc                        ((|>))
import qualified System.Directory            as Dir
import qualified System.IO                   as IO

run :: IO Int
run = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEncoding IO.stdin IO.utf8
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetEncoding IO.stdout IO.utf8
  let logState = Log.init |> Log.log "Booting up"
  logState >>= loop False Nothing

loop :: Bool -> Maybe State -> LogState -> IO Int
loop isShuttingDown maybeState initialLogState =
  IncomingMessage.decode IO.stdin initialLogState >>= \(decoded, logState) ->
    case decoded of
      Left error ->
        Log.log (Text.pack error) logState >>= loop isShuttingDown maybeState
      Right message ->
        case message of
          (IncomingMessage.RequestMessage id RequestMethod.Shutdown) ->
            loop True maybeState logState
          (IncomingMessage.NotificationMessage NotificationMethod.Exit) ->
            if isShuttingDown
              then Log.log "Exiting." logState >>= Log.log "---" >> return 0
              else Log.log "Exiting without shutdown." logState >>=
                   Log.log "---" >>
                   return 1
          _ ->
            let (nextState, nextLogState, maybeResponseByteString) =
                  MessageHandler.handler maybeState logState message
                response = mapM_ BS.putStr maybeResponseByteString
            in response >> Log.flush nextLogState >>= loop False nextState
