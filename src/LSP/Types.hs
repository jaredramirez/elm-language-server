module LSP.Types
  ( IncomingMessage(..)
  , Method(..)
  , Position(..)
  , Range(..)
  , Location(..)
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

newtype Location =
  Location (Text, Range)

-- OUTGOING DATA --
newtype OutgoingMessage =
  ResponseMessage (Maybe Text, Maybe OutgoingError)

newtype OutgoingError =
  ResponseError (Error, Text)

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
