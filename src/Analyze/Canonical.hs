{-# LANGUAGE OverloadedStrings #-}

module Analyze.Canonical
  ( canonicalize
  ) where


import qualified AST.Canonical as Can
import AST.Valid (Module)
import qualified Canonicalize.Module as Canonicalize
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Elm.Compiler.Module as Module
import qualified Elm.Name as N
import Elm.Project.Json (Project)
import qualified Elm.Project.Json as Project
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import Misc ((<|))
import qualified Stuff.Verify as Verify
import Task (Task)
import qualified Task


-- Is `projectRoot` here the cloned root?
-- If so, file path must be the cloned file path
canonicalize :: Text -> Project -> N.Name -> BS.ByteString -> Module -> Task Can.Module
canonicalize projectRoot project moduleName source validModule =
  do
    let projectRootString = Text.unpack projectRoot
    Task.fromElmTask <| Project.check project
    summary <- Task.fromElmTask <| Verify.verify projectRootString project
    graph <- Task.fromElmTask <| Crawl.crawlFromSource summary source
    (dirty, ifaces) <- Task.fromElmTask <| Plan.plan Nothing summary graph
    let maybeInfo = Map.lookup moduleName dirty
    case maybeInfo of
      Nothing ->
        Task.throw "file not found in build plan"

      Just info ->
        do
          let imports = makeImports project info
          let pkgName = Project.getName project
          Task.fromElmResult (\_ -> "") <|
            Canonicalize.canonicalize pkgName imports ifaces validModule


makeImports :: Project -> Plan.Info -> Map.Map Module.Raw Module.Canonical
makeImports project (Plan.Info _ _ _ clean dirty foreign_) =
  let
    pkgName =
      Project.getName project

    mkLocal name =
      ( name, Module.Canonical pkgName name )

    mkForeign canonicalName@(Module.Canonical _ name) =
      ( name, canonicalName )
  in
    Map.fromList $
      map mkLocal clean
      ++ map mkLocal dirty
      ++ map mkForeign foreign_
