{-# LANGUAGE LambdaCase        #-}

module LSP.MessageHandler.RequestInitialize
  ( handler
  ) where

import           Data.Aeson               (Value)
import qualified Data.Aeson               as A
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Data.Error           as Error
import           LSP.Data.RequestMethod   (InitializeParams)
import qualified LSP.Data.RequestMethod   as RequestMethod
import           LSP.Update               (Msg)
import qualified LSP.Update               as U
import qualified LSP.Data.URI             as URI
import qualified LSP.MessageHandler.Misc  as Misc
import           Misc                     ((<|), (|>))

handler :: Text -> Value -> IO Msg
handler id paramsJson =
  let params :: A.Result InitializeParams
      params = A.fromJSON paramsJson
  in case params of
      A.Error error ->
        return (U.SendRequestError id Error.InvalidParams (Text.pack error))

      A.Success (RequestMethod.InitializeParams rootUri) ->
        case URI.decodePath rootUri of
          Left error ->
            return (U.SendRequestError id Error.InvalidParams (Text.pack error))

          Right projectRoot ->
            let exectuable = Misc.findElmExectuable (Text.unpack projectRoot)
            in exectuable >>= \case
                Left error ->
                  return (U.SendRequestError id Error.InternalError error)

                Right executableValue ->
                  return (U.Initialize id projectRoot (Text.pack executableValue))
