{-# LANGUAGE OverloadedStrings #-}

module LSP.Diagnostics
  ( ElmDiagnostics
  , run
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, Value, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as ATypes
import Data.Aeson.Types (Parser)
import qualified Data.List as List
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import LSP.Data.Diagnostic (Diagnostic)
import qualified LSP.Data.Diagnostic as D
import LSP.Data.Range (Range)
import qualified LSP.Data.Range as Range
import Misc ((<|), (|>))
import qualified Misc
import qualified System.IO as IO
import System.Directory as Dir
import System.Exit as SysE
import System.Process as SysP
import System.IO as SysIO

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

messageToText :: MessagePart -> Text
messageToText message =
  case message of
    Str text ->
      text

    Meta meta ->
      ""

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
  deriving (Show)

instance FromJSON ElmDiagnostics where
  parseJSON =
    A.withObject "Diagnostic Error" $ \v ->
      (v .: "type" :: Parser Text) >>= \type_ ->
        case type_ of
          "compile-errors" ->
            return CompileError
              <*> v .: "errors"

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

toDiagnostics :: ElmDiagnostics -> [(Text, [Diagnostic])]
toDiagnostics diagnostics =
  case diagnostics of
    CompileError errors ->
      List.map
        (\(Error path name problems) ->
          ( path
          , List.map
            (\(Problem title range message) ->
              D.Diagnostic
                -- LSP protocol uses 0-index line/column numbers and the Elm
                -- compiler does not. So we decrement each by 1 to get range properly
                (Range.updatePositions (\l -> l - 1) range)
                (List.foldl
                  (\acc cur ->
                    cur
                      |> messageToText
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

-- RUN DIAGNOSTICS --
outputToDiagnostics :: Text -> (SysE.ExitCode, String, String) -> Either Text [Diagnostic]
outputToDiagnostics elmFilePath (exitCode, stdOutString, stdErrString) =
  case exitCode of
    SysE.ExitFailure _ ->
      let eitherAllDiagnostics =
            stdErrString
              |> Text.pack
              |> Misc.textToByteString
              |> A.eitherDecode
              |> Misc.mapLeft Text.pack
              |> fmap toDiagnostics
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
                elmFilePath == pathForDiagnostics
              )
            |> fmap snd
            |> Maybe.fromMaybe []
            |> Right

    SysE.ExitSuccess ->
      Right []

run :: Text -> Text -> IO (Either Text [Diagnostic])
run elmExectuablePath elmFilePath =
  fmap
    (outputToDiagnostics elmFilePath)
    (SysP.readProcessWithExitCode
      (Text.unpack elmExectuablePath)
      ["make", Text.unpack elmFilePath, "--report=json"]
      ""
    )

logger ::  Show a => a -> IO ()
logger  message =
  let dirPath = "/Users/jaredramirez/dev/src/github.com/jaredramirez/elm-field"
      dirPathModified = Text.append dirPath "/elm-stuff/.lsp"
      filePath = Text.append dirPathModified "/debug.log"
  in Dir.createDirectoryIfMissing True (Text.unpack dirPathModified) >>
     IO.openFile (Text.unpack filePath) IO.AppendMode >>= \handle ->
     IO.hPrint handle message >> IO.hClose handle
