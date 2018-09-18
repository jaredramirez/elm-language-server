module LSP.Data.State
  ( State(..)
  , Document(..)
  , init
  , updateDocuments
  , DocumentText(..)
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           Prelude             hiding (init)

type UriKey = Text

data State = State
  { _projectRoot   :: Text
  , _executable :: Text
  , _documents :: HashMap UriKey Document
  } deriving (Show)

data Document = Document
  { _text       :: DocumentText
  , _executableLegacy :: Text
  } deriving (Show)

newtype DocumentText =
  DocumentText (Int, Text)
  deriving (Show)

init :: Text -> Text -> State
init projectRoot executable = State projectRoot executable HM.empty

updateDocuments :: HashMap UriKey Document -> State -> State
updateDocuments documents (State projectRoot executable _) = State projectRoot executable documents
