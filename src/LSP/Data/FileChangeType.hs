{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.FileChangeType
  ( FileChangeType(..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Utils as AUtils
import Misc ((<|), (|>))
import qualified Misc

data FileChangeType
  = Created
  | Changed
  | Deleted
  deriving (Show)

instance FromJSON FileChangeType where
  parseJSON =
    A.withScientific "FileChangeType" $ \num ->
      let
          int_ = num |> AUtils.floatingOrInteger |> Misc.toInt
      in
      case int_ of
        1 ->
          return Created

        2 ->
          return Changed

        3 ->
          return Deleted

        _ ->
          fail "Unrecognized file change type"

instance ToJSON FileChangeType where
  toJSON fileChangeType  =
    case fileChangeType of
      Created ->
        A.toJSON (1 :: Int)

      Changed ->
        A.toJSON (2 :: Int)

      Deleted ->
        A.toJSON (3 :: Int)
