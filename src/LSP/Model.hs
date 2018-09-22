module LSP.Model
  ( Model(..)
  , Document(..)
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import LSP.Data.URI (URI)
import Prelude hiding (init)

data Model = Model
  { _shouldTerminate :: Bool
  , _initialized :: Bool
  , _projectMeta :: Maybe (Text, Text)
  , _documents :: HashMap URI Document
  } deriving (Show)

data Document = Document
  { _version :: Int
  , _text :: Text
  } deriving (Show)
