module LSP.Types
  ( IncomingMessage(..)
  , Method(..)
  , Position(..)
  , Range(..)
  , OutgoingMessage(..)
  , OutgoingError(..)
  , Error(..)
  ) where

import           Data.Text (Text)

-- INCOMING DATA --
data IncomingMessage
  = RequestMessage Text
                   Method
  | NotificationMessage Method
  deriving (Show)

data Method =
  CancelRequest
  deriving (Show)

newtype Position =
  Position (Int, Int)

newtype Range =
  Range (Position, Position)

-- OUTGOING DATA --
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
