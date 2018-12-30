{-# LANGUAGE OverloadedStrings #-}

module LSP.Log
  ( logger
  ) where

import qualified Data.Text        as Text
import qualified System.Directory as Dir
import qualified System.IO        as IO

logger :: Show a => a -> IO ()
logger  message =
  let
      dirPath =
        "./elm-stuff/.lsp"

      filePath =
        Text.append dirPath "/debug.log"
  in
  Dir.createDirectoryIfMissing True (Text.unpack dirPath) >>
    IO.openFile (Text.unpack filePath) IO.AppendMode >>= \handle ->
    IO.hPrint handle message >> IO.hClose handle
