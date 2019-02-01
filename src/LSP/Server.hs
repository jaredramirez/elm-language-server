{-# LANGUAGE OverloadedStrings #-}

module LSP.Server
  ( run
  ) where

import           Control.Exception           (SomeException, catch)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.List                   as List
import qualified LSP.Data.Message            as Message
import qualified LSP.Log                     as Log
import           LSP.Model                   (Model)
import qualified LSP.Update                  as U
import qualified LSP.MessageHandler          as MessageHandler
import qualified System.IO                   as IO
import qualified Task

run :: IO Int
run =
  do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEncoding IO.stdin IO.utf8
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetEncoding IO.stdout IO.utf8
    catch (loop U.init) handleException

loop :: Model -> IO Int
loop model =
  do
    decoded <- Message.decode IO.stdin
    case decoded of
      Left _ ->
        loop model

      Right message ->
        do
          -- Log.logger ("Message: " ++ show message)
          let task = MessageHandler.handler model message
          msg <- Task.run task
          let (nextModel, response, termination) = U.update msg model
          let responseIO =
                case response of
                  U.None ->
                    return ()

                  U.Send byteString ->
                    BS.putStr byteString

                  U.SendMany byteStrings ->
                    sequence_ (List.map BS.putStr byteStrings)
          Log.logger ("Msg: " ++ show msg)
          Log.logger ("Response: " ++ show response)
          Log.logger ("Model: " ++ show model ++ "\n")
          case termination of
            U.ShouldTerminate ->
              return 1

            U.ShouldNotTerminate ->
              responseIO >> loop nextModel

handleException :: SomeException -> IO Int
handleException ex =
  print ex >> Log.logger ex >> return 1
