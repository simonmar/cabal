-- | cabal-install CLI command: buck2
--
-- Sets up (or refreshes) a buck2 build for the current project, using the
-- prelude and support scripts checked out at @buck2\/@ (a checkout of
-- <https://github.com/simonmar/haskell-buck2>, see @buck2\/README.md@):
--
--   1. Build every dependency (never the local packages themselves), the
--      same as @cabal build all --only-dependencies@ - reusing
--      'CmdBuild.buildAction' directly, so every flag @cabal build@\/
--      @cabal configure@ understands (@-f...@, @--enable-profiling@, ...)
--      works here too.
--   2. Create @.buckconfig@\/@PACKAGE@ if they don't exist yet.
--   3. Run @buck2\/gen-haskell-prebuilt.py@ to turn the resolved dependency
--      closure into @third-party\/haskell@.
--   4. Generate a @BUCK.cabal.bzl@ (and, where missing, a @BUCK@) for
--      every local package - see "Distribution.Client.Buck2.Generate".
module Distribution.Client.CmdBuck2
  ( buck2Command
  , buck2Action
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Client.CmdBuild as CmdBuild
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.DistDirLayout (DistDirLayout (distProjectRootDirectory))
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
  ( GlobalFlags
  , InstallFlags (installOnlyDeps)
  )
import Distribution.Client.Types.PackageLocation (PackageLocation (..))

import Distribution.Package (packageId)
import Distribution.Simple.Command (CommandUI (..), usageAlternatives)
import Distribution.Simple.Flag (toFlag)
import Distribution.Simple.Utils (dieWithException, notice)
import Distribution.Verbosity (normal)

import Distribution.Client.Buck2.Generate (generateAllPackages)
import Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  , runGenHaskellPrebuilt
  )
import Distribution.Client.Errors
  ( CabalInstallException (Buck2ActionExtraArgs, Buck2NonLocalPackageLocation)
  )

buck2Command :: CommandUI (NixStyleFlags ())
buck2Command =
  CommandUI
    { commandName = "buck2"
    , commandSynopsis = "Set up (or refresh) a buck2 build for this project."
    , commandUsage = usageAlternatives "buck2" ["[FLAGS]"]
    , commandDescription = Just $ \_ ->
        "Builds every dependency of the project (as `cabal build all "
          ++ "--only-dependencies` would), then generates the buck2 build "
          ++ "files (.buckconfig, PACKAGE, third-party/haskell, and a "
          ++ "BUCK.cabal.bzl for each local package) needed to build the "
          ++ "project with buck2 instead of cabal. Requires a checkout of "
          ++ "https://github.com/simonmar/haskell-buck2 at ./buck2. See "
          ++ "buck2/README.md for details.\n\n"
          ++ "Flags that would normally be passed to `cabal build`/`cabal "
          ++ "configure` (-f, --enable-profiling, etc.) are honoured here "
          ++ "too, and apply to the dependency build."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

buck2Action :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
buck2Action flags extraArgs globalFlags = do
  unless (null extraArgs) $
    dieWithException verbosity (Buck2ActionExtraArgs extraArgs)

  ProjectBaseContext{distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
    establishProjectBaseContext verbosity cliConfig OtherCommand
  let projectRoot = distProjectRootDirectory distDirLayout

  checkBuck2Prelude verbosity projectRoot

  notice verbosity "cabal buck2: building dependencies (cabal build all --only-dependencies)"
  CmdBuild.buildAction (toBuildDepsFlags flags) ["all"] globalFlags

  ensureBuckconfigAndPackage verbosity projectRoot
  runGenHaskellPrebuilt verbosity projectRoot

  (_, elaboratedPlan, _, _, _) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing
  localPkgs <-
    sequenceA
      [ do
          dir <- localPackageDir verbosity elab
          return (dir, elabPkgDescription elab)
      | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlan
      , elabLocalToProject elab
      ]
  -- Per-component elaboration gives each local package one
  -- 'ElaboratedConfiguredPackage' per component (library, executable, ...),
  -- all sharing the same directory and the same (whole-package)
  -- 'PackageDescription' - so without this, a package with N buildable
  -- components would get regenerated N times over.
  generateAllPackages verbosity projectRoot (nubBy ((==) `on` fst) localPkgs)

  notice verbosity $
    unlines
      [ "cabal buck2: done. You can now:"
      , "    buck2 build //...          # build everything"
      , "    buck2 test //...           # test everything"
      , "    buck2 build //... -m opt   # build everything in opt mode"
      ]
  where
    verbosity = cfgVerbosity normal flags
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here

toBuildDepsFlags :: NixStyleFlags () -> NixStyleFlags CmdBuild.BuildFlags
toBuildDepsFlags flags =
  flags
    { installFlags = (installFlags flags){installOnlyDeps = toFlag True}
    , extraFlags = CmdBuild.defaultBuildFlags
    }

localPackageDir :: Verbosity -> ElaboratedConfiguredPackage -> IO FilePath
localPackageDir verbosity elab = case elabPkgSourceLocation elab of
  LocalUnpackedPackage dir -> return dir
  _ -> dieWithException verbosity (Buck2NonLocalPackageLocation (prettyShow (packageId elab)))
