{-# LANGUAGE OverloadedStrings #-}

module LSP.Server
  ( run
  ) where

import qualified Data.ByteString          as BSStrict
import qualified Data.ByteString.Lazy     as BS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Data.Header          as Header
import qualified LSP.Data.IncomingMessage as IncomingMessage
import qualified LSP.Data.OutgoingMessage as OutgoingMessage
import           LSP.Data.State           (State)
import qualified LSP.Data.State           as State
import           Misc                     ((<|))
import           Prelude                  hiding (getLine, log)
import qualified System.Directory         as Dir
import           System.IO                (Handle)
import qualified System.IO                as IO

run :: IO Int
run = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEncoding IO.stdin IO.utf8
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetEncoding IO.stdout IO.utf8
  log "Booting up"
  loop Nothing
  return 1

loop :: Maybe State -> IO ()
loop maybeState =
  getLine >>= \header ->
    getLine >>= \endLine ->
      let eitherContentLength =
            Header.decode header >>= \contentLength ->
              Header.decodeEndLine endLine >> return contentLength
      in case eitherContentLength of
           Left error -> log error >> loop maybeState
           Right contentLength ->
             BS.hGet IO.stdin contentLength >>= \jsonBytestring ->
               log jsonBytestring >>
               case IncomingMessage.decode jsonBytestring of
                 Left error    -> log error >> loop maybeState
                 Right message -> log message >> loop maybeState

getLine :: IO BS.ByteString
getLine = BS.fromStrict <$> BSStrict.getLine

log :: Show message => message -> IO ()
log message =
  let dirPath = "/Users/jaredramirez/elm-stuff/.lsp"
      filePath = Text.append dirPath "/debug.log"
  in Dir.createDirectoryIfMissing True (Text.unpack dirPath) >>
     IO.openFile (Text.unpack filePath) IO.AppendMode >>= \handle ->
       IO.hPrint handle message >> IO.hClose handle
