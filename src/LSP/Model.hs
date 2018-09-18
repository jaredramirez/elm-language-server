module LSP.Model
  ( State(..)
  , Document(..)
  , DocumentText(..)
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           Prelude             hiding (init)

type UriKey = Text

data Model = Model
  { _projectMeta :: Maybe (Text, Text)
  , _documents   :: HashMap UriKey Document
  } deriving (Show)

data Document = Document
  { _text             :: DocumentText
  , _executableLegacy :: Text
  } deriving (Show)

newtype DocumentText =
  DocumentText (Int, Text)
  deriving (Show)
