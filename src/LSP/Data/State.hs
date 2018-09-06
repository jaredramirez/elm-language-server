module LSP.Data.State
  ( State(..)
  , init
  , updateDocumentText
  , DocumentText(..)
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           Prelude             hiding (init)

data State = State
  { _rootUri      :: Text
  , _documentText :: HashMap Text DocumentText
  } deriving (Show)

init :: Text -> State
init rootUri = State rootUri HM.empty

updateDocumentText :: HashMap Text DocumentText -> State -> State
updateDocumentText documentText (State rootUri _) = State rootUri documentText

newtype DocumentText =
  DocumentText (Int, Text)
  deriving (Show)
