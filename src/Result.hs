module Result
  ( fromElmResult
  ) where


import qualified Reporting.Result as ElmResult


fromElmResult :: ElmResult.Result () [w] error a -> Maybe a
fromElmResult result =
  let
      (_warnings, either) =
        ElmResult.run result
  in
  case either of
    Left _errors ->
      Nothing

    Right value ->
      Just value
