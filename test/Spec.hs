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
import qualified Elm.Name               as N
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


-- Test Helpers


testPkgName :: Pkg.Name
testPkgName = Pkg.Name "Jared Ramirez" "Test"


parse :: Task.Task Valid.Module
parse =
  do
    raw <-
      BS.readFile "./test/sample/src/Main.elm"
        |> Task.lift

    Parse.program testPkgName raw
      |> Task.fromElmResult (\_ -> "Failed to parse")


parseAndCanonicalize :: Task.Task Can.Module
parseAndCanonicalize =
  do
    parsed <- parse

    project <-
      Project.read "./test/sample/elm.json"
        |> Task.fromElmTask

    ifaces <-
      Verify.verify "./test/safe" project
        |> Task.fromElmTask
        |> fmap Summary._ifaces

    let importDict =
          LSPMisc.getForeignImportDict ifaces

    Canonicalize.canonicalize testPkgName importDict ifaces parsed
      |> Task.fromElmResult (\_ -> "Failed to canonicalize")


searchModule :: Int -> Int -> Task.Task Search.Value
searchModule line column =
  do
    canonical <-
      parseAndCanonicalize

    let maybeFound =
          Search.getInfo canonical (Search.Location line column)

    case maybeFound of
      Nothing ->
        Task.throw "Not found"

      Just found ->
        return found

-- Tests


unitTests :: TestTree
unitTests =
  testGroup
    "Parse Tests"
    [ testCase
        "Parse Module"
        (Task.try parse >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right _ -> return ()
        )
    , testCase
        "Canoncailize Module"
        (Task.try parseAndCanonicalize >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right _ -> return ()
        )
    , testCase
        "Top-level un-typed primitive search"
        (Task.try (searchModule 4 11) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Type (Search.Primitive name) ->
                    case N.toText name of
                        "String" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type primitive but was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected primitive type"
        )
    , testCase
        "Top-level typed primitive search"
        (Task.try (searchModule 8 10) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Type (Search.Primitive name) ->
                    case N.toText name of
                        "()" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type primitive but was \"" ++ Text.unpack invalid ++ "\" instead of \"()\"")

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected primitive type"
        )
    , testCase
        "Top-level type definition search"
        (Task.try (searchModule 12 24) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Reference canonical name ->
                    case N.toText name of
                        "++" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a reference but the name was \"" ++ Text.unpack invalid ++ "\" instead of \"++\"")

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected reference"
        )
    ]
