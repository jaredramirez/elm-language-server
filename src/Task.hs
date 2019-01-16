{-# LANGUAGE OverloadedStrings #-}

module Task
  ( Task
  , try
  , run
  , throw
  , mapError
  , lift
  , liftEither
  , fromElmTask
  , fromElmResult
  ) where

import qualified Control.Monad.Except as E
import Control.Monad.Trans (liftIO)
import Control.Exception (SomeException, tryJust)
import qualified Data.List as List
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Reporting.Task as ElmTask
import qualified Reporting.Progress as ElmProgress
import qualified Reporting.Result as ElmResult
import qualified Reporting.Exit as ElmExit
import Misc ((<|), (|>))


type Task value =
  Task_ Text value


type Task_ error value =
  E.ExceptT error IO value


try :: Task value -> IO (Either Text value)
try task =
  E.runExceptT task


run :: (Text -> value) -> Task value -> IO value
run toValue task =
  do
    either <- E.runExceptT task
    let result =
          case either of
            Left text ->
              toValue text

            Right value ->
              value
    return result


throw :: e -> Task_ e a
throw error =
  E.throwError error


mapError :: (x -> y) -> Task_ x a -> Task_ y a
mapError =
  E.withExceptT


liftEither :: Either error value -> Task_ error value
liftEither e =
  either throw return e


ioToEither :: IO value -> IO (Either Text value)
ioToEither io =
  tryJust exceptionToText io


exceptionToText :: SomeException -> Maybe Text
exceptionToText ex =
  Just (Text.pack (show ex))


lift :: IO value -> Task value
lift io =
  do
    either <- liftIO <| ioToEither io
    liftEither either


fromElmTask :: ElmTask.Task value -> Task value
fromElmTask elmTask =
  do
    result <- liftIO <| ElmTask.tryWithError ElmProgress.silentReporter elmTask
    case result of
      Left exit ->
        exit
          |> ElmExit.toString
          |> Text.pack
          |> throw

      Right value ->
        return value


fromElmResult :: (err -> Text) -> ElmResult.Result () [w] err a -> Task a
fromElmResult errorToText result =
  let
      (_warnings, either) =
        ElmResult.run result
  in
  case either of
    Left errors ->
      errors
        |> List.foldr
            (\curError acc ->
              acc <> "," <> errorToText curError
            )
            ""
        |> Task.throw

    Right value ->
      return value
