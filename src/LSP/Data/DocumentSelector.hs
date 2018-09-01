module LSP.Data.DocumentSelector
  ( DocumentSelector(..)
  ) where

import           Data.Aeson              (FromJSON, ToJSON, Value, (.:?))
import qualified Data.Aeson              as A
import qualified Data.ByteString.Lazy    as BS
import           LSP.Data.DocumentFilter (DocumentFilter)

newtype DocumentSelector =
  DocumentSelector [DocumentFilter]

instance FromJSON DocumentSelector where
  parseJSON v = DocumentSelector <$> A.parseJSON v

decode :: BS.ByteString -> Either String DocumentSelector
decode = A.eitherDecode'
