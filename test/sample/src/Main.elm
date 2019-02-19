module Main exposing (..)


first = "hello, world"


second : ()
second = ()


third : String -> Int -> String
third arg1 arg2 = "appended" ++ arg1


fourth : String -> String
fourth arg =
    let
        appended =
            "apended" ++ arg
    in
    arg


fifth thingToTransform thingToAppend strTransform =
    "appended" ++ (strTransform thingToTransform) ++ thingToAppend


type Custom =
    Custom String


sixth : Custom -> Int
sixth (Custom str) =
    String.length str
