{-# LANGUAGE OverloadedStrings #-}

module Misc
  ( (|>)
  , (<|)
  , curryTriple
  , mapLeft
  , maybeToEither
  , andThen
  , toInt
  , byteStringToText
  ) where

import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as TextEncode

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(<|) :: (a -> b) -> a -> b
(<|) = ($)

curryTriple :: ((a, b, c) -> d) -> a -> b -> c -> d
curryTriple f a b c = f (a, b, c)

mapLeft :: (a -> b) -> Either a e -> Either b e
mapLeft func either =
  case either of
    Left a  -> Left (func a)
    Right e -> Right e

maybeToEither :: error -> Maybe value -> Either error value
maybeToEither error maybe =
  case maybe of
    Nothing    -> Left error
    Just value -> Right value

andThen :: Monad m => (a -> m b) -> m a -> m b
andThen = (=<<)

toInt :: (RealFloat r, Integral i) => Either r i -> Int
toInt num =
  case num of
    Left float     -> fromIntegral (round float)
    Right integral -> fromIntegral integral

byteStringToText :: BS.ByteString -> Text
byteStringToText byteString =
  case TextEncode.decodeUtf8' (BS.toStrict byteString) of
    Left _      -> ""
    Right value -> value
