{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString        as BS
import qualified Data.Char              as Char
import qualified Data.List              as List
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Map               as Map
import           Data.Word              (Word8)
import           Test.Tasty
import           Test.Tasty.HUnit

-- Internal Modules
import qualified Analyze.Search         as Search
import qualified AST.Valid              as Valid
import qualified AST.Canonical          as Can
import qualified Canonicalize.Module    as Canonicalize
import qualified Elm.Package            as Pkg
import qualified Elm.Project.Json       as Project
import qualified Elm.Project.Summary    as Summary
import qualified LSP.Misc               as LSPMisc
import           Misc                   ((<|), (|>), andThen)
import qualified Parse.Parse            as Parse
import qualified Reporting.Result       as Result
import qualified Stuff.Verify           as Verify
import qualified Task                   as Task


main :: IO ()
main = defaultMain tests


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


unitTests :: TestTree
unitTests =
  testGroup
    "Parse Tests"
    [ testCase
        "Parse Module"
        (case Result.run $ Parse.program simpleModuleName simpleModule of
           (_, Left _) -> assertFailure "Failed to parse"
           (_, Right _) -> return ()
        )
    , testCase
        "Canoncailize Module"
        (let
            task =
              do
                parsed <-
                  Parse.program simpleModuleName simpleModule
                    |> Task.fromElmResult (\_ -> "Failed to parse")

                project <-
                  Project.read "./test/sample/elm.json"
                    |> Task.fromElmTask

                ifaces <-
                  Verify.verify "./test/safe" project
                    |> Task.fromElmTask
                    |> fmap Summary._ifaces

                let importDict =
                      LSPMisc.getForeignImportDict ifaces

                canonical <-
                  Canonicalize.canonicalize simpleModuleName importDict ifaces parsed
                    |> Task.fromElmResult (\_ -> "Failed to canonicalize")

                return canonical
         in
         Task.try task >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right _ -> return ()
        )
    , testCase
        "Top-level value search"
        (let
            task =
              do
                parsed <-
                  Parse.program simpleModuleName simpleModule
                    |> Task.fromElmResult (\_ -> "Failed to parse")

                project <-
                  Project.read "./test/sample/elm.json"
                    |> Task.fromElmTask

                ifaces <-
                  Verify.verify "./test/safe" project
                    |> Task.fromElmTask
                    |> fmap Summary._ifaces

                let importDict =
                      LSPMisc.getForeignImportDict ifaces

                canonical <-
                  Canonicalize.canonicalize simpleModuleName importDict ifaces parsed
                    |> Task.fromElmResult (\_ -> "Failed to canonicalize")

                let maybeFound =
                      Search.findValueInModule canonical 4 11

                case maybeFound of
                  Nothing ->
                    Task.throw "Not found"

                  Just _ ->
                    return ()
         in
         Task.try task >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right _ -> return ()
        )
    ]
