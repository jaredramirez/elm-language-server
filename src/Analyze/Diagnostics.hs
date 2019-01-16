{-# LANGUAGE OverloadedStrings #-}

module Analyze.Diagnostics
  ( ElmDiagnostics
  , run
  ) where

import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as ATypes
import Data.Aeson.Types (Parser)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified LSP.Log as Log
import LSP.Data.Diagnostic (Diagnostic)
import qualified LSP.Data.Diagnostic as D
import LSP.Data.Range (Range)
import qualified LSP.Data.Range as Range
import qualified LSP.Data.Position as Position
import Misc ((<|), (|>))
import qualified Misc
import System.Exit as SysE
import System.Process as SysP
import Task (Task)
import qualified Task

-- DIAGNOSTICS MessageMeta --
data MessagePartMeta =
  MessageMeta
    { _bold :: Bool
    , _underLine :: Bool
    , _color :: Maybe Text
    , _string :: Text
    }
    deriving (Show)

instance FromJSON MessagePartMeta where
  parseJSON =
    A.withObject "Diagnostic MessagePartMeta" $ \v ->
      return MessageMeta
        <*> v .: "bold"
        <*> v .: "underline"
        <*> v .:? "color"
        <*> v .: "string"

-- DIAGNOSTICS Message
data MessagePart
  = Str Text
  | Meta MessagePartMeta
  deriving (Show)

instance FromJSON MessagePart where
  parseJSON value =
    case value of
      A.Object _ ->
        return Meta
          <*> A.parseJSON value

      A.String _ ->
        return Str
          <*> A.parseJSON value

      _ ->
        ATypes.typeMismatch "Diagnostic MessagePart" value

messageToTextThrowAwayMeta :: MessagePart -> Text
messageToTextThrowAwayMeta message =
  case message of
    Str text ->
      text

    Meta _meta ->
      ""

messageToText :: MessagePart -> Text
messageToText message =
  case message of
    Str text ->
      text

    Meta meta ->
      _string meta

-- DIAGNOSTICS Problem --
data Problem =
  Problem
    { _title :: Text
    , _range :: Range
    , _message :: [MessagePart]
    }
    deriving (Show)

instance FromJSON Problem where
  parseJSON =
    A.withObject "Diagnostic Problem" $ \v ->
      return Problem
        <*> v .: "title"
        <*> v .: "region" -- parse range as "region"
        <*> v .: "message"

-- DIAGNOSTICS ERROR --
data Error =
  Error
    { _path :: Text
    , _name :: Text
    , _problems :: [Problem]
    }
    deriving (Show)

instance FromJSON Error where
  parseJSON =
    A.withObject "Diagnostic Error" $ \v ->
      return Error
        <*> v .: "path"
        <*> v .: "name"
        <*> v .: "problems"

-- DIAGNOSTICS RESULT --
data ElmDiagnostics
  = CompileError [Error]
  | OtherError Text Text [MessagePart]
  deriving (Show)

instance FromJSON ElmDiagnostics where
  parseJSON =
    A.withObject "Diagnostic Error" $ \v ->
      (v .: "type" :: Parser Text) >>= \type_ ->
        case type_ of
          "compile-errors" ->
            return CompileError
              <*> v .: "errors"

          "error" ->
            return OtherError
              <*> v .: "path"
              <*> v .: "title"
              <*> v .: "message"

          _ ->
            fail "Invalid diagnostics result"

-- CONVERT TO LSP DATA STRUCTURE
squashConsecutiveChars :: Text -> Text
squashConsecutiveChars text =
  text
    |> Text.unpack
    |> List.group
    |> List.foldl
      (\acc cur ->
        case cur of
          h0 : h1 : _ ->
            if (h0 == ' ' && h1 == ' ') || (h0 == '\n' && h1 == '\n') then
              Text.append acc (Text.singleton h0)

            else
              Text.append acc (Text.pack cur)

          _ ->
            Text.append acc (Text.pack cur)
      )
      ""

toDiagnostics :: Text -> ElmDiagnostics -> [(Text, [Diagnostic])]
toDiagnostics filePath diagnostics =
  case diagnostics of
    CompileError errors ->
      List.map
        (\(Error path _name problems) ->
          ( path
          , List.map
            (\(Problem _title range message) ->
              D.Diagnostic
                -- LSP protocol uses 0-index line/column numbers and the Elm
                -- compiler does not. So we decrement each by 1 to get range properly
                (Range.updatePositions (\l -> l - 1) range)
                (List.foldl
                  (\acc cur ->
                    cur
                      |> messageToTextThrowAwayMeta
                      |> squashConsecutiveChars
                      |> Text.append acc
                  )
                  ""
                  message
                )
                1
            )
            problems
          )
        )
        errors

    OtherError path title messageParts ->
      -- If it's OtherError, that means the problem lies in `elm.json` or something
      -- so we show the issue at the top of whatever file is open
      [ ( filePath
        , [ D.Diagnostic
              (Range.Range (Position.Position (0, 0), Position.Position (0, 6)))
              (title <> " in " <> path <> ". " <>
                List.foldl
                  (\acc cur ->
                    cur
                      |> messageToText
                      |> squashConsecutiveChars
                      |> Text.append acc
                  )
                  ""
                  messageParts
              )
              1
          ]
        )
      ]

-- RUN DIAGNOSTICS --
outputToDiagnostics :: Text -> (SysE.ExitCode, String, String) -> Either Text [Diagnostic]
outputToDiagnostics filePath (exitCode, _stdOutString, stdErrString) =
  case exitCode of
    SysE.ExitFailure _ ->
      let
          eitherAllDiagnostics =
            stdErrString
              |> Text.pack
              |> Misc.textToByteString
              |> A.eitherDecode
              |> Misc.mapLeft Text.pack
              |> fmap (toDiagnostics filePath)
      in
      case eitherAllDiagnostics of
        Left error ->
          Left error

        -- elm make returns diagnostics on all files with errors so here
        -- we only take the diagnostics for the relevent file. If there are
        -- no relevent diagnostics, we clear current diagnostics
        Right allDiagnostics ->
          allDiagnostics
            |> List.find
              (\(pathForDiagnostics, _) ->
                filePath == pathForDiagnostics
              )
            |> fmap snd
            |> Maybe.fromMaybe []
            |> Right

    SysE.ExitSuccess ->
      Right []

run :: Text -> Text -> Task [Diagnostic]
run elmExectuablePath filePath =
  do
    liftIO <| Log.logger filePath
    liftIO <| Log.logger elmExectuablePath
    processOutput <- liftIO
        (SysP.readProcessWithExitCode
          (Text.unpack elmExectuablePath)
          ["make", Text.unpack filePath, "--report=json"]
          ""
        )
    Task.liftEither (outputToDiagnostics filePath processOutput)
