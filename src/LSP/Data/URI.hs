{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.URI
  ( decodePath
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

prefix :: Text
prefix = "file://"

prefixLength :: Int
prefixLength = Text.length prefix

decodePath :: Text -> Either String Text
decodePath uri =
  let (head, rest) = Text.splitAt prefixLength uri
  in if head == prefix
       then Right rest
       else Left "Invalid URI"
