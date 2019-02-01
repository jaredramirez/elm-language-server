{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString        as BS
import qualified Data.Char              as Char
import qualified Data.Map.Strict        as Map
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Word              (Word8)
import           Test.Tasty
import           Test.Tasty.HUnit

-- Internal Modules
import qualified AST.Valid              as Valid
import qualified Elm.Package            as Pkg
import qualified Parse.Parse            as Parse
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result       as Result

main :: IO ()
-- main = defaultMain tests
main = return ()

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- AST Parse Test
toWord8 :: Text -> [Word8]
toWord8 = Text.foldr (\c acc -> fromIntegral (Char.ord c) : acc) []

simpleModuleName :: Pkg.Name
simpleModuleName = Pkg.Name "Jared Ramirez" "Simple"

simpleModule :: BS.ByteString
simpleModule =
  BS.pack
    (toWord8
       ("module Main exposing (..)\n" <> "\n" <> "hello : String\n" <>
        "hello = \"hello, world\"\n"))

unitTests =
  testGroup
    "Parse Tests"
    [ testCase
        "Simple Module"
        (case Result.run $ Parse.program simpleModuleName simpleModule of
           ([], Left _) -> assertFailure "Failed to parse"
           ([], Right parsedModule) -> return ()
           (_, _) -> assertFailure "Failed to parse with warnings")
    ]
