{-# LANGUAGE OverloadedStrings #-}

module Analyze.Oracle
    (
    ) where

import qualified Analyze.Data.Documentation
import AST.Module (Module)
import qualified AST.Module as Module
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import LSP.Data.ElmConfig (ElmConfig)
import qualified LSP.Data.ElmConfig as ElmConfig
import Misc ((|>))

-- Much thanks to https://github.com/Krzysztof-Cieslak/vscode-elm/blob/master/src/elmDelphi.ts
-- the interworkings of this module

ask :: Module -> ElmConfig -> Text -> ()
ask module_ config rawSearch =
  let
      eitherClassified =
        splitSearchText rawSearch

      -- (searchValue, maybeSearchModule)
  in
  ()


splitSearchText :: Text -> Either Text (Text, Maybe Text)
splitSearchText search =
  let
      parts =
        search
          |> Text.split (== '.')
          |> List.reverse
  in
  case parts of
    [] ->
      Left "Invalid search"

    [head] ->
      Right (head, Nothing)

    head : rest ->
      Right
        ( head
        , rest
          |> List.reverse
          |> Text.intercalate "."
          |> Just
        )
