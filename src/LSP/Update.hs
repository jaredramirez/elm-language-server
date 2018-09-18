module LSP.Update
  ( init
  , update
  ) where

import qualified Data.ByteString.Lazy as BS
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import           LSP.Model            (Model)
import qualified LSP.Model            as M
import           Misc                 ((|>))
import           Prelude              hiding (init)

init :: Model
init = M.Model Nothing HM.empty

data Response
  = Send BS.ByteString
  | None

data Msg
  = SetProjectMeta Text
                   Text
  | SetDocument Text
                M.Document

update :: Msg -> Model -> (Model, Response)
update msg model =
  case msg of
    SetProjectMeta projectRoot executable ->
      M.Model (Just (projectRoot, executable)) (M._documents model)
    SetDocument uri document ->
      M.Model
        (M._documents model)
        (model |> M._documents |> HM.alter (const (Just document)) uri)
