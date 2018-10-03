{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Range
  ( Range(..)
  , updatePositions
  ) where

import           Data.Aeson           (FromJSON, ToJSON, Value, (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           LSP.Data.Position    (Position)
import qualified LSP.Data.Position    as P

newtype Range =
  Range (Position, Position)
  deriving (Show)

instance FromJSON Range where
  parseJSON =
    A.withObject "Range" $ \v -> curry Range <$> v .: "start" <*> v .: "end"

instance ToJSON Range where
  toJSON (Range (start, end)) = A.object ["start" .= start, "end" .= end]

updatePositions :: (Int -> Int) -> Range -> Range
updatePositions func (Range (P.Position (l1, c1), P.Position (l2, c2))) =
  Range (P.Position (func l1, func c1), P.Position (func l2, func c2))
