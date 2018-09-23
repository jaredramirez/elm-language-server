module LSP.Update
  ( init
  , Msg(..)
  , update
  , Response(..)
  , ShouldTermiate(..)
  ) where

import qualified Data.ByteString.Lazy  as BS
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)
import qualified LSP.Data.Capabilities as Capabilities
import           LSP.Data.Error        (Error)
import           LSP.Data.Message      (Message)
import qualified LSP.Data.Message      as Message
import           LSP.Data.MessageError (MessageError)
import qualified LSP.Data.MessageError as MessageError
import           LSP.Data.URI          (URI)
import qualified LSP.Data.URI          as URI
import           LSP.Model             (Model)
import qualified LSP.Model             as M
import           Misc                  ((<|), (|>))
import           Prelude               hiding (init)

init :: Model
init = M.Model False False Nothing HM.empty

data Response
  = Send BS.ByteString
  | None
  deriving (Show)

data ShouldTermiate
  = ShouldTerminate
  | ShouldNotTerminate
  deriving (Show)

data Msg
  = Initialize Text Text Text
  | DidOpen URI M.Document
  | RequestShutDown
  | Exit
  | SendRequestError Text Error Text
  | SendNotifError Error Text
  | NoOp
  deriving (Show)

update :: Msg -> Model -> (Model, Response, ShouldTermiate)
update msg model =
  case msg of
    Initialize id projectRoot executable ->
      ( model
          { M._initialized = True
          , M._projectMeta = Just (projectRoot, executable)
          }
      , Send
        (Message.encode
          (Message.ResponseMessage
              (Just id)
              (Just Capabilities.capabilities)
              Nothing
          )
        )
      , ShouldNotTerminate
      )

    DidOpen uri document ->
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

    SendRequestError id error errorMessage ->
      let messageError = MessageError.MessageError error errorMessage
          message :: Message ()
          message =
            Message.ResponseMessage (Just id) Nothing (Just messageError)
      in
        ( model
        , Send (Message.encode message)
        , ShouldNotTerminate
        )

    SendNotifError error errorMessage ->
      let messageError = MessageError.MessageError error errorMessage
          message :: Message ()
          message =
            Message.ResponseMessage Nothing Nothing (Just messageError)
      in
        ( model
        , Send (Message.encode message)
        , ShouldNotTerminate
        )

    NoOp ->
      ( model
      , None
      , ShouldNotTerminate
      )
