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
        (Task.try (searchModule 5 11) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Type _ canType ->
                    case Search.canTypeToText canType of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a reference but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected primitive type"
        )
    , testCase
        "Top-level typed primitive value search"
        (Task.try (searchModule 10 5) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Type _ canType ->
                    case Search.canTypeToText canType of
                        "()" ->
                          return ()

                        invalid ->
                          ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                            |> Text.unpack
                            |> assertFailure

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected primitive type"
        )
    , testCase
        "Top-level type definition search"
        (Task.try (searchModule 15 16) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Reference canonical name ->
                    case N.toText name of
                      "++" ->
                        return ()

                      invalid ->
                        ("Got a reference but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected reference"
        )
    , testCase
        "Let definition type primitive search"
        (Task.try (searchModule 22 17) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Type _ canType ->
                    case Search.canTypeToText canType of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.Debug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type primitive"
        )
       , testCase
        "Let definition variable search"
        (Task.try (searchModule 22 26) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.Type _ canType ->
                    case Search.canTypeToText canType of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

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
        (Task.try (hover 5 11) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def"
        (Task.try (hover 27 3) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "a -> String -> (a -> String) -> String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"a -> String -> (a -> String) -> String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed value def"
        (Task.try (hover 10 5) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "()" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"()\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def"
        (Task.try (hover 36 3) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "Custom -> ( String, Int ) -> Int" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"Custom -> ( String, Int ) -> Int\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def un-typed arg"
        (Task.try (hover 28 34) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "a" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"a\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def typed arg"
        (Task.try (hover 28 59) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level un-typed function def typed arg that is a function"
        (Task.try (hover 28 23) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "a -> String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"a -> String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def arg that is a bit nested"
        (Task.try (hover 22 27) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def custom type arg that is pattern matched"
        (Task.try (hover 36 16) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def custom type arg that is pattern matched reference"
        (Task.try (hover 37 20) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def tuple arg that is pattern matched"
        (
         Task.try (hover 36 23) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ -> assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def tuple arg that is pattern matched reference"
        (
         Task.try (hover 36 29) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ -> assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def tuple arg that is pattern matched"
        (
         Task.try (hover 36 36) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "Int" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"Int\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ ->
                    assertFailure "Expected type"
        )
    , testCase
        "Top-level typed function def tuple arg that is pattern matched reference"
        (
         Task.try (hover 37 43) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "Int" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"Int\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ -> assertFailure "Expected type"
        )
    , testCase
        "let def custom type that is pattern matched"
        (
         Task.try (hover 43 18) >>= \result ->
           case result of
             Left message -> assertFailure (Text.unpack message)
             Right value ->
                case value of
                  Search.HoverType tipe ->
                    case Search.canTypeToText tipe of
                      "String" ->
                        return ()

                      invalid ->
                        ("Got a type but it was \"" <> invalid <> "\" instead of \"String\"")
                          |> Text.unpack
                          |> assertFailure

                  Search.HoverDebug message ->
                    assertFailure ("DEBUG: " ++  message)

                  _ -> assertFailure "Expected type"
        )
    ]
