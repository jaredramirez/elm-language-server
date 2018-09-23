{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Header
  ( decodeEndLine
  , decode
  , encode
  ) where

import qualified Data.ByteString.Lazy as BS
import           Data.Int             (Int64)
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TextEncode
import qualified Data.Text.Read       as TextRead
import           Misc                 ((<|), (|>))
import qualified Misc
import           System.Info          as SysInfo

intToByteString :: Int64 -> BS.ByteString
intToByteString int_ = int_ |> show |> Text.pack |> Misc.textToByteString

contentLengthHeader :: BS.ByteString
contentLengthHeader = Misc.textToByteString "Content-Length: "

contentLengthHeaderLength :: Int64
contentLengthHeaderLength = BS.length contentLengthHeader

incomingEndLine :: BS.ByteString
incomingEndLine =
  let end =
        if SysInfo.os == "mingw32"
          then "\r\n"
          else "\r"
  in Misc.textToByteString end

decodeEndLine :: BS.ByteString -> Either String ()
decodeEndLine string =
  if string == incomingEndLine
    then Right ()
    else Left "Invalid end of line"

toHeader :: BS.ByteString -> Either String Int
toHeader bytestring =
  let stripped = BS.stripSuffix incomingEndLine bytestring
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

outgoingEndLine :: BS.ByteString
outgoingEndLine =
  let end =
        if SysInfo.os == "mingw32"
          then "\n\n"
          else "\r\n\r\n"
  in Misc.textToByteString end

encode :: BS.ByteString -> BS.ByteString
encode content =
  let contentLength = BS.length content
  in contentLengthHeader <> intToByteString contentLength <> outgoingEndLine
