{-# LANGUAGE OverloadedStrings #-}

module LSP.Server
  ( run
  ) where

import qualified AST.Valid                   as Valid
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Map.Strict             as Map
import           Data.Semigroup              ((<>))
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
import qualified Misc
import qualified System.Directory            as Dir
import qualified System.IO                   as IO

simpleModuleAst :: Valid.Module
simpleModuleAst = Valid.defaultModule Map.empty [] [] [] [] []

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
  IncomingMessage.decode IO.stdin initialLogState >>= \(decoded, decodeLogState) ->
    case decoded of
      Left error ->
        Log.log (Text.pack error) decodeLogState >>=
        loop isShuttingDown maybeState
      Right message ->
        Log.log ("Got message: " <> Log.toText message) decodeLogState >>= \logState ->
          case message of
            (IncomingMessage.RequestMessage _ RequestMethod.Shutdown) ->
              loop True maybeState logState
            (IncomingMessage.NotificationMessage NotificationMethod.Exit) ->
              if isShuttingDown
                then Log.log "Exiting." logState >>= Log.log "---" >> return 0
                else Log.log "Exiting without shutdown." logState >>=
                     Log.log "---" >>
                     return 1
            _ ->
              MessageHandler.handler maybeState logState message >>= \(nextState, nextLogState, maybeResponseByteString) ->
                Log.flush nextLogState >>=
                (\flushedLogState ->
                   case maybeResponseByteString of
                     Nothing -> return flushedLogState
                     Just response ->
                       BS.putStr response >>
                       Log.log
                         ("Sending " <> Misc.byteStringToText response)
                         flushedLogState) >>=
                loop False nextState
