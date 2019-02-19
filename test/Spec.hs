{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString        as BS
import qualified Data.Char              as Char
import qualified Data.List              as List
import qualified Data.Map               as Map
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Map               as Map
import           Data.Word              (Word8)
import           Test.Tasty
import           Test.Tasty.HUnit


-- Elm
import qualified AST.Valid              as Valid
import qualified AST.Canonical          as Can
import qualified Canonicalize.Module    as Canonicalize
import qualified Elm.Name               as N
import qualified Elm.Package            as Pkg
import qualified Elm.Project.Json       as Project
import qualified Elm.Project.Summary    as Summary
import qualified Parse.Parse            as Parse
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Result       as Result
import qualified Stuff.Verify           as Verify


-- Internal Modules
import qualified Analyze.Search         as Search
import qualified LSP.Misc               as LSPMisc
import           Misc                   ((<|), (|>), andThen)
import qualified Task                   as Task


main :: IO ()
main =
  defaultMain
    (testGroup "Tests"
      [ parseTests
      , searchTests
      , hoverTests
      ]
    )


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


hover :: Int -> Int -> Task.Task Search.HoverResult
hover line column =
  do
    parsed <- parse
    project <-
      Project.read "./test/sample/elm.json"
        |> Task.fromElmTask
    ifaces <-
      Verify.verify "./test/safe" project
        |> Task.fromElmTask
        |> fmap Summary._ifaces
    let importDict = LSPMisc.getForeignImportDict ifaces
    canonical <-
      Canonicalize.canonicalize testPkgName importDict ifaces parsed
        |> Task.fromElmResult (\_ -> "Failed to canonicalize")
    let localizer = L.fromModule parsed
    Search.hover canonical localizer line column

-- Tests


parseTests :: TestTree
parseTests =
  testGroup
    "Parse and Canonicalize Tests"
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
    ]

searchTests :: TestTree
searchTests =
  testGroup
    "Search Tests"
    [ testCase
        "Top-level un-typed primitive value search"
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
        "Top-level typed primitive value search"
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
        (Task.try (searchModule 12 31) >>= \result ->
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
    , testCase
        "Let definition type primitive search"
        (Task.try (searchModule 19 18) >>= \result ->
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
                            ("Got a reference but the name was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type primitive"
        )
       , testCase
        "Let definition variable search"
        (Task.try (searchModule 19 27) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Arg _name tipe ->
                    case tipe of
                      Can.TType _ typeName _ ->
                        case N.toText typeName of
                            "String" ->
                              return ()

                            invalid ->
                              assertFailure
                                ("Got a TType but the name was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")
                      _ ->
                        assertFailure "Got a type but the name was not a TType"

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected Arg"
        )
    ]


hoverTests :: TestTree
hoverTests =
  testGroup
    "Hover Tests"
    [ testCase
        "Top-level un-typed value def"
        (Task.try (hover 4 11) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "String" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed value def"
        (Task.try (hover 8 10) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "()" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"()\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def un-typed arg"
        (Task.try (hover 25 41) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "a" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"a\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def typed arg"
        (Task.try (hover 25 62) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "String" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def typed arg that is a function"
        (Task.try (hover 25 23) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "a -> String" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"a -> String\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def arg that is a bit nested"
        (Task.try (hover 19 27) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "String" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def arg that is pattern matched"
        (Task.try (hover 34 20) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case tipe of
                        "String" ->
                          return ()

                        invalid ->
                          assertFailure
                            ("Got a type but was \"" ++ Text.unpack invalid ++ "\" instead of \"String\"")

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    ]
