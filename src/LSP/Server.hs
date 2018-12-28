{-# LANGUAGE OverloadedStrings #-}

module LSP.Server
  ( run
  ) where

import           Control.Exception           (SomeException, catch)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Map.Strict             as Map
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as Text
import qualified Data.List                   as List
import qualified LSP.Data.Message            as Message
import qualified LSP.Data.NotificationMethod as NotificationMethod
import qualified LSP.Data.RequestMethod      as RequestMethod
import qualified LSP.Log                     as Log
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import qualified LSP.Update                  as U
import qualified LSP.MessageHandler          as MessageHandler
import           Misc                        ((|>))
import qualified Misc
import qualified System.Directory            as Dir
import qualified System.IO                   as IO
import qualified AST.Json                    as J

run :: IO Int
run =
  IO.hSetBuffering IO.stdin IO.NoBuffering >>
  IO.hSetEncoding IO.stdin IO.utf8 >>
  IO.hSetBuffering IO.stdout IO.NoBuffering >>
  IO.hSetEncoding IO.stdout IO.utf8 >>
  catch (loop U.init) handleException

loop :: Model -> IO Int
loop model =
  Message.decode IO.stdin >>= \decoded ->
    case decoded of
      Left error ->
        loop model

      Right message ->
        Log.logger ("Message: " ++ show message) >>
        MessageHandler.handler model message >>= \msg ->
          let
              (nextModel, response, termination) =
                U.update msg model

              responseIO =
                case response of
                  U.None ->
                    return ()

                  U.Send byteString ->
                    BS.putStr byteString

                  U.SendMany byteStrings ->
                    sequence_ (List.map BS.putStr byteStrings)
          in
          Log.logger ("Model: " ++ show nextModel) >>
          Log.logger ("Msg: " ++ show msg) >>
          Log.logger ("Response: " ++ show response) >>
          Log.logger ("Termination: " ++ show termination) >>
          Log.logger "" >>
            case termination of
              U.ShouldTerminate ->
                return 1

              U.ShouldNotTerminate ->
                responseIO >> loop nextModel

handleException :: SomeException -> IO Int
handleException ex =
  print ex >> Log.logger ex >> return 1
