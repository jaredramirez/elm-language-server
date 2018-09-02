module LSP.Data.State
  ( State(..)
  , init
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           LSP.Log             (LogState)
import           Prelude             hiding (init)

data State = State
  { rootUri      :: Text
  , documentText :: HashMap Text Text
  } deriving (Show)

init :: Text -> State
init rootUri = State rootUri HM.empty
