{-# LANGUAGE OverloadedStrings #-}

module LSP.Diagnostics
  ( Diagnostics(..)
  , run
  ) where

import System.Exit as SysE
import System.Process as SysP
import System.IO as SysIO
import Data.Aeson (FromJSON, Value, (.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import LSP.Data.Range (Range)
import Misc ((<|), (|>))
import qualified Misc

-- DIAGNOSTICS ERROR --
data Error =
  Error
    { _path :: Text
    , _name :: Text
    , _range :: Range
    , _message :: [Text]
    }
    deriving (Show)

instance FromJSON Error where
  parseJSON =
    A.withObject "Diagnostic Error" $ \v ->
      return Error
      <*> v .: "path"
      <*> v .: "name"
      <*> v .: "region"
      <*> v .: "message"

-- DIAGNOSTICS RESULT --
data Diagnostics
  = CompileError [Error]
  deriving (Show)

instance FromJSON Diagnostics where
  parseJSON =
    A.withObject "Diagnostic Error" $ \v ->
      (v .: "type" :: Parser Text) >>= \type_ ->
        case type_ of
          "compile-errors" ->
            return CompileError
            <*> v .: "errors"

          _ ->
            fail "Invalid diagnostics result"

-- RUN DIAGNOSTICS --
run :: Text -> Text -> IO (Either Text Diagnostics)
run elmExectuablePath elmFilePath =
  SysP.runInteractiveCommand (Text.unpack $ elmExectuablePath <> " " <> elmFilePath <> " --report=json")
    >>= \(_pIn, pOut, pErr, handle) ->
      SysP.waitForProcess handle
        >>= \exitCode ->
          case exitCode of
            SysE.ExitFailure _ ->
              return (Left "Process returned a non-zero exit code")

            SysE.ExitSuccess ->
              fmap
                (
                  Text.pack
                    `compose` Misc.textToByteString
                    `compose` A.eitherDecode
                    `compose` Misc.mapLeft Text.pack
                )
                (SysIO.hGetContents pOut)

compose = flip (.)
