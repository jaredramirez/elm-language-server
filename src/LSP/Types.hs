module LSP.Types
  ( IncomingMessage(..)
  , Method(..)
  , OutgoingMessage(..)
  , OutgoingError(..)
  , Error(..)
  ) where

import           Data.Text (Text)

data IncomingMessage
  = RequestMessage Text
                   Method
  | NotificationMessage Method
  deriving (Show)

data Method =
  CancelRequest
  deriving (Show)

data OutgoingMessage =
  ResponseMessage (Maybe Text)
                  (Maybe OutgoingError)
  deriving (Show)

data OutgoingError =
  ResponseError Error
                Text
  deriving (Show)

data Error
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerErrorStart
  | ServerErrorEnd
  | ServerNotInitialized
  | UnknownErrorCode
  | RequestCancelled
  deriving (Show)
