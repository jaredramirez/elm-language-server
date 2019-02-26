module Main exposing (Custom(..), fifth, first, fourth, second, sixth, third)


first =
    "hello, world"


second : ()
second =
    ()


third : String -> Int -> String
third arg1 arg2 =
    "appended" ++ arg1


fourth : String -> String
fourth arg =
    let
        appended =
            "apended" ++ arg
    in
    arg


fifth thingToTransform thingToAppend strTransform =
    "appended" ++ strTransform thingToTransform ++ thingToAppend


type Custom
    = Custom String


sixth : Custom -> ( String, Int ) -> Int
sixth (Custom str) ( anotherStr, anInt ) =
    String.length (str ++ anotherStr) * anInt


seventh : Int -> Int
seventh int =
    let
        (Custom str) =
            Custom ""
    in
    int * (String.length str)
