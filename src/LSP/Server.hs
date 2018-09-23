{-# LANGUAGE OverloadedStrings #-}

module LSP.Server
  ( run
  ) where

import qualified AST.Valid                   as Valid
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Map.Strict             as Map
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as Text
import qualified LSP.Data.Message            as Message
import qualified LSP.Data.NotificationMethod as NotificationMethod
import qualified LSP.Data.RequestMethod      as RequestMethod
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import qualified LSP.Update                  as U
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
  loop U.init

loop :: Model -> IO Int
loop model =
  Message.decode IO.stdin >>= \decoded ->
    case decoded of
      Left error ->
        loop model

      Right message ->
        logger ("Got Message: " ++ show decoded) >>
        MessageHandler.handler model message >>= \msg ->
          let (nextModel, response, termination) = U.update msg model
              responseIO =
                case response of
                  U.None ->
                    return ()

                  U.Send byteString ->
                    BS.putStr byteString
          in
            logger ("Response: " ++ show response) >>
              logger ("Termination: " ++ show termination) >>
                case termination of
                  U.ShouldTerminate ->
                    return 1

                  U.ShouldNotTerminate ->
                    responseIO >> loop nextModel

logger ::  Show a => a -> IO ()
logger  message =
  let dirPath = "/Users/jaredramirez/dev/src/github.com/jaredramirez/elm-field"
      dirPathModified = Text.append dirPath "/elm-stuff/.lsp"
      filePath = Text.append dirPathModified "/debug.log"
  in Dir.createDirectoryIfMissing True (Text.unpack dirPathModified) >>
     IO.openFile (Text.unpack filePath) IO.AppendMode >>= \handle ->
     IO.hPrint handle message >> IO.hClose handle
