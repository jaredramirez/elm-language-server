{-# LANGUAGE OverloadedStrings #-}

module LSP.Log
  ( LogState
  , init
  , setDirPath
  , log
  , logDeferred
  , flush
  , toText
  ) where

import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Prelude          hiding (init, log)
import qualified System.Directory as Dir
import           System.IO        (Handle)
import qualified System.IO        as IO

newtype LogState =
  LogState (Maybe Text, [Text])
  deriving (Show)

init :: LogState
init = LogState (Nothing, [])

setDirPath :: Text -> LogState -> LogState
setDirPath dirPath (LogState (_, existingMessages)) =
  LogState (Just dirPath, existingMessages)

log :: Text -> LogState -> IO LogState
log message (LogState (maybeDirPath, existingMessages)) =
  case maybeDirPath of
    Nothing -> return (LogState (Nothing, message : existingMessages))
    Just dirPath ->
      logInternal dirPath message >>
      return (LogState (maybeDirPath, existingMessages))

logDeferred :: Text -> LogState -> LogState
logDeferred message (LogState (maybeDirPath, existingMessages)) =
  LogState (maybeDirPath, message : existingMessages)

flush :: LogState -> IO LogState
flush (LogState (maybeDirPath, messages)) =
  if null messages
    then return (LogState (maybeDirPath, messages))
    else case maybeDirPath of
           Nothing -> return (LogState (maybeDirPath, messages))
           Just dirPath ->
             let messagesChronological = reverse messages
             in mapM_ (logInternal dirPath) messagesChronological >>
                return (LogState (maybeDirPath, []))

toText :: Show showable => showable -> Text
toText showable = Text.pack (show showable)

logInternal :: Text -> Text -> IO ()
logInternal dirPath message =
  let dirPathModified = Text.append dirPath "/elm-stuff/.lsp"
      filePath = Text.append dirPathModified "/debug.log"
  in Dir.createDirectoryIfMissing True (Text.unpack dirPathModified) >>
     IO.openFile (Text.unpack filePath) IO.AppendMode >>= \handle ->
       IO.hPrint handle message >> IO.hClose handle
