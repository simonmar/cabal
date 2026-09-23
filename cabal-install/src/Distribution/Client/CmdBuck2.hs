-- | cabal-install CLI command: buck2
--
-- Sets up (or refreshes) a buck2 build for the current project, using the
-- prelude and support scripts checked out at @buck2\/@ (a checkout of
-- <https://github.com/simonmar/haskell-buck2>, see @buck2\/README.md@):
--
--   1. Build every dependency (never the local packages themselves), the
--      same as @cabal build all --only-dependencies@ would - by resolving
--      and pruning the install plan exactly as 'CmdBuild.buildAction'
--      does (reusing its 'CmdBuild.selectPackageTargets'\/
--      'CmdBuild.selectComponentTarget'), so every flag @cabal build@\/
--      @cabal configure@ understands (@-f...@, @--enable-profiling@,
--      @--enable-tests@, ...) works here too. Doing this in-process,
--      rather than delegating to 'CmdBuild.buildAction' as an opaque
--      action, is what lets step 3 below see exactly the same
--      (test\/benchmark-flag-aware) dependency closure that just got
--      built - see "Distribution.Client.Buck2.Prebuilt".
--   2. Create @.buckconfig@\/@PACKAGE@ if they don't exist yet (copied
--      verbatim from @buck2\/example@).
--   3. Turn the resolved dependency closure into @third-party\/haskell@ -
--      see "Distribution.Client.Buck2.Prebuilt".
--   4. Generate a @BUCK.cabal.bzl@ (and, where missing, a @BUCK@) for
--      every local package - see "Distribution.Client.Buck2.Generate".
module Distribution.Client.CmdBuck2
  ( buck2Command
  , buck2Action
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as Map

import qualified Distribution.Client.CmdBuild as CmdBuild
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.DistDirLayout (DistDirLayout (distProjectRootDirectory))
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectOrchestration
-- 'pruneInstallPlanToTargets' is hidden: 'ProjectOrchestration' re-exports
-- its own wrapper of the same name (taking a 'TargetsMap' directly,
-- matching what 'resolveTargetsFromSolver' below returns), which would
-- otherwise be ambiguous with 'ProjectPlanning's lower-level original.
import Distribution.Client.ProjectPlanning hiding (pruneInstallPlanToTargets)
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (..)
  , TargetContext (..)
  , updateContextAndWriteProjectFile
  , withContextAndSelectors
  )
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
import Distribution.Client.Buck2.Prebuilt (generatePrebuilt)
import Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  )
import Distribution.Client.Errors
  ( CabalInstallException (Buck2ActionExtraArgs, Buck2NonLocalPackageLocation, ReportCannotPruneDependencies)
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
          ++ "configure` (-f, --enable-profiling, --enable-tests, etc.) are "
          ++ "honoured here too, and apply to the dependency build."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

buck2Action :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
buck2Action flags extraArgs globalFlags = do
  unless (null extraArgs) $
    dieWithException verbosity (Buck2ActionExtraArgs extraArgs)

  withContextAndSelectors verbosity RejectNoTargets Nothing depsFlags ["all"] globalFlags BuildCommand $
    \targetCtx ctx targetSelectors -> do
      baseCtx <- case targetCtx of
        ProjectContext -> return ctx
        GlobalContext -> return ctx
        ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

      let projectRoot = distProjectRootDirectory (distDirLayout baseCtx)
      checkBuck2Prelude verbosity projectRoot

      -- The same target resolution + pruning 'CmdBuild.buildAction ["all"]
      -- --only-dependencies' does, inlined here (rather than delegated to
      -- it as an opaque action) so 'elaboratedPlanToExecute' below - the
      -- exact, test/benchmark-flag-aware dependency closure that's about
      -- to be built - stays in hand for 'generatePrebuilt'.
      buildCtx@ProjectBuildContext{elaboratedPlanOriginal, elaboratedPlanToExecute, elaboratedShared} <-
        runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
          targets <-
            either (reportTargetProblems verbosity "buck2") return $
              resolveTargetsFromSolver
                CmdBuild.selectPackageTargets
                CmdBuild.selectComponentTarget
                elaboratedPlan
                Nothing
                targetSelectors
          let elaboratedPlan' = pruneInstallPlanToTargets TargetActionBuild targets elaboratedPlan
          elaboratedPlan'' <-
            either (dieWithException verbosity . ReportCannotPruneDependencies . renderCannotPruneDependencies) return $
              pruneInstallPlanToDependencies (Map.keysSet targets) elaboratedPlan'
          return (elaboratedPlan'', targets)

      notice verbosity "cabal buck2: building dependencies (cabal build all --only-dependencies)"
      printPlan verbosity baseCtx buildCtx
      buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
      runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

      ensureBuckconfigAndPackage verbosity projectRoot
      generatePrebuilt
        verbosity
        projectRoot
        (cabalDirLayout baseCtx)
        elaboratedShared
        elaboratedPlanToExecute

      localPkgs <-
        sequenceA
          [ do
              dir <- localPackageDir verbosity elab
              return (dir, elabPkgDescription elab)
          | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlanOriginal
          , elabLocalToProject elab
          ]
      -- Per-component elaboration gives each local package one
      -- 'ElaboratedConfiguredPackage' per component (library, executable,
      -- ...), all sharing the same directory and the same (whole-package)
      -- 'PackageDescription' - so without this, a package with N
      -- buildable components would get regenerated N times over.
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
    depsFlags = flags{installFlags = (installFlags flags){installOnlyDeps = toFlag True}}

localPackageDir :: Verbosity -> ElaboratedConfiguredPackage -> IO FilePath
localPackageDir verbosity elab = case elabPkgSourceLocation elab of
  LocalUnpackedPackage dir -> return dir
  _ -> dieWithException verbosity (Buck2NonLocalPackageLocation (prettyShow (packageId elab)))
