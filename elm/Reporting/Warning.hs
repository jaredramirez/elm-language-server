{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning
  ( Warning(..)
  , Context(..)
  , toReport
  )
  where


import Data.Monoid ((<>))

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Report as Report
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L



-- ALL POSSIBLE WARNINGS


data Warning
  = UnusedImport R.Region N.Name
  | UnusedVariable R.Region Context N.Name
  | MissingTypeAnnotation R.Region N.Name Can.Type


data Context = Def | Pattern



-- TO REPORT


toReport :: L.Localizer -> Code.Source -> Warning -> Report.Report
toReport localizer source warning =
  case warning of
    UnusedImport region moduleName ->
      Report.Report "unused import" region [] $
        Report.toCodeSnippet source region Nothing
          (
            D.reflow $
              "Nothing from the `" <> N.toString moduleName <> "` module is used in this file."
          ,
            "I recommend removing unused imports."
          )

    UnusedVariable region context name ->
      let title = defOrPat context "unused definition" "unused variable" in
      Report.Report title region [] $
        Report.toCodeSnippet source region Nothing
          (
            D.reflow $
              "You are not using `" <> N.toString name <> "` anywhere."
          ,
            D.stack
              [ D.reflow $
                  "Is there a typo? Maybe you intended to use `" <> N.toString name
                  <> "` somewhere but typed another name instead?"
              , D.reflow $
                  defOrPat context
                    ( "If you are sure there is no typo, remove the definition.\
                      \ This way future readers will not have to wonder why it is there!"
                    )
                    ( "If you are sure there is no typo, replace `" <> N.toString name
                      <> "` with _ so future readers will not have to wonder why it is there!"
                    )
              ]
          )

    MissingTypeAnnotation region name inferredType ->
        Report.Report "missing type annotation" region [] $
          Report.toCodeSnippet source region Nothing
            (
              D.reflow $
                case Type.deepDealias inferredType of
                  Can.TLambda _ _ ->
                    "The `" <> N.toString name <> "` function has no type annotation."

                  _ ->
                    "The `" <> N.toString name <> "` definition has no type annotation."
            ,
              D.stack
                [ "I inferred the type annotation myself though! You can copy it into your code:"
                , D.green $ D.hang 4 $ D.sep $
                    [ D.fromName name <> " :"
                    , RT.canToDoc localizer RT.None inferredType
                    ]
                ]
            )


defOrPat :: Context -> a -> a -> a
defOrPat context def pat =
  case context of
    Def -> def
    Pattern -> pat

