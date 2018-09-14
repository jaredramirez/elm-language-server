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
  { _rootUri   :: Text
  , _documents :: HashMap UriKey Document
  } deriving (Show)

data Document = Document
  { _text       :: DocumentText
  , _executable :: Text
  } deriving (Show)

newtype DocumentText =
  DocumentText (Int, Text)
  deriving (Show)

init :: Text -> State
init rootUri = State rootUri HM.empty

updateDocuments :: HashMap UriKey Document -> State -> State
updateDocuments documents (State rootUri _) = State rootUri documents
