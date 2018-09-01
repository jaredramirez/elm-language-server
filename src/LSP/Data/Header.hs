{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Header
  ( decodeEndLine
  , decode
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Char            as Char
import           Data.Int             (Int64)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TextEncode
import qualified Data.Text.Read       as TextRead
import           Data.Word            (Word8)
import           Misc                 ((<|), (|>))
import qualified Misc

toWord8 :: Text -> [Word8]
toWord8 = Text.foldr (\c acc -> fromIntegral (Char.ord c) : acc) []

contentLengthHeader :: BS.ByteString
contentLengthHeader = "Content-Length: " |> toWord8 |> BS.pack

contentLengthHeaderLength :: Int64
contentLengthHeaderLength = BS.length contentLengthHeader

lineEnd :: BS.ByteString
lineEnd = "\r" |> toWord8 |> BS.pack

decodeEndLine :: BS.ByteString -> Either String ()
decodeEndLine string =
  if string == lineEnd
    then Right ()
    else Left "Invalid end of line"

toHeader :: BS.ByteString -> Either String Int
toHeader bytestring =
  let stripped = BS.stripSuffix lineEnd bytestring
      either = Misc.maybeToEither "Invalid line ending" stripped
  in either >>=
     (\numString ->
        numString |> BS.toStrict |> TextEncode.decodeUtf8' |>
        Misc.mapLeft (const "Error reading Content-Length value")) >>=
     (\text ->
        fromIntegral . fst <$>
        (TextRead.decimal text :: Either String (Integer, Text)))

decode :: BS.ByteString -> Either String Int
decode string =
  let (header, rest) = BS.splitAt contentLengthHeaderLength string
  in if header == contentLengthHeader
       then toHeader rest
       else Left "Invalid header"
