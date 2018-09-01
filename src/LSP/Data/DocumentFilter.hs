{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.DocumentFilter
  ( DocumentFilter(..)
  ) where

import           Data.Aeson           (FromJSON, ToJSON, Value, (.:?))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import           LSP.Data.Method      (Method)
import qualified LSP.Data.Method      as M
import qualified LSP.Data.Misc        as Misc

newtype DocumentFilter =
  DocumentFilter (Maybe Text, Maybe Text, Maybe Text)

instance FromJSON DocumentFilter where
  parseJSON =
    A.withObject "DocumentFilter" $ \v ->
      Misc.curryTriple DocumentFilter <$> v .:? "language" <*> v .:? "scheme" <*>
      v .:? "pattern"

decode :: BS.ByteString -> Either String DocumentFilter
decode = A.eitherDecode'
