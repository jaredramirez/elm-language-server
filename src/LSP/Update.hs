module LSP.Update
  ( init
  , Msg(..)
  , update
  , Response(..)
  , ShouldTermiate(..)
  ) where

import qualified Data.ByteString.Lazy     as BS
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Text                (Text)
import qualified LSP.Data.Capabilities    as Capabilities
import           LSP.Data.Error           (Error)
import           LSP.Data.OutgoingError   (OutgoingError)
import qualified LSP.Data.OutgoingError   as OutgoingError
import           LSP.Data.OutgoingMessage (OutgoingMessage)
import qualified LSP.Data.OutgoingMessage as OutgoingMessage
import           LSP.Model                (Model)
import qualified LSP.Model                as M
import           Misc                     ((<|), (|>))
import           Prelude                  hiding (init)

init :: Model
init = M.Model False False Nothing HM.empty

data Response
  = Send BS.ByteString
  | None

data ShouldTermiate
  = ShouldTerminate
  | ShouldNotTerminate

data Msg
  = Initialize Text Text Text
  | SetDocument Text M.Document
  | RequestShutDown
  | Exit
  | SendRequestError Text Error Text
  | SendNotifError Error Text
  | NoOp

update :: Msg -> Model -> (Model, Response, ShouldTermiate)
update msg model =
  case msg of
    Initialize id projectRoot executable ->
      ( model
          { M._initialized = True
          , M._projectMeta = Just (projectRoot, executable)
          }
      , Send
        (OutgoingMessage.encode
          (OutgoingMessage.ResponseMessage
              ( Just id
              , Just Capabilities.capabilities
              , Nothing
              )
          )
        )
      , ShouldNotTerminate
      )

    SetDocument uri document ->
      ( model
          { M._documents =
              model
                |> M._documents
                |> HM.alter (const (Just document)) uri
          }
      , None
      , ShouldNotTerminate
      )

    RequestShutDown ->
      ( model { M._shouldTerminate = True }
      , None
      , ShouldNotTerminate
      )

    Exit ->
      (model, None, ShouldTerminate)

    SendRequestError id error message ->
      let outgoingError = OutgoingError.ResponseError (error, message)
          outgoingMessage :: OutgoingMessage ()
          outgoingMessage =
            OutgoingMessage.ResponseMessage
              (Just id, Nothing, Just outgoingError)
      in
        ( model
        , Send (OutgoingMessage.encode outgoingMessage)
        , ShouldNotTerminate
        )

    SendNotifError error message ->
      let outgoingError = OutgoingError.ResponseError (error, message)
          outgoingMessage :: OutgoingMessage ()
          outgoingMessage =
            OutgoingMessage.ResponseMessage
              (Nothing, Nothing, Just outgoingError)
      in
        ( model
        , Send (OutgoingMessage.encode outgoingMessage)
        , ShouldNotTerminate
        )

    NoOp ->
      ( model
      , None
      , ShouldNotTerminate
      )
