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
import qualified Data.Set as Set

import qualified Distribution.Client.CmdBuild as CmdBuild
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.DistDirLayout
  ( DistDirLayout (distProjectRootDirectory, distUnpackedSrcDirectory)
  )
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

import Distribution.Package (HasUnitId (installedUnitId), packageId, packageName, packageVersion)
import Distribution.Simple.Command (CommandUI (..), usageAlternatives)
import Distribution.Simple.Flag (toFlag)
import Distribution.Simple.Utils (dieWithException, notice)
import Distribution.Types.UnitId (UnitId)
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
              pruneToDependenciesNeeded (Map.keysSet targets) elaboratedPlan'
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
        (distDirLayout baseCtx)
        elaboratedShared
        elaboratedPlanToExecute

      -- Every genuinely local package, *plus* every non-local one
      -- whose own build was forced 'inplace' by depending on
      -- one. When a non-local package is forced inplace we must
      -- include it in the set of buck2-built packages, otherwise the
      -- build will contain multiple incompatible versions of the
      -- local dependency. A real-world example of this is
      -- hackage-security in the cabal project, which is not a local
      -- package but depends on the local Cabal-syntax.
      localPkgs <-
        sequenceA
          [ do
              dir <- packageSourceDir verbosity (distDirLayout baseCtx) elab
              return (dir, elabPkgDescription elab)
          | InstallPlan.Configured elab <- InstallPlan.toList elaboratedPlanOriginal
          , elabLocalToProject elab || elabBuildStyle elab /= BuildAndInstall
          ]
      -- The resolved version of every package in the plan (local and
      -- external alike), needed to generate each component's own
      -- @cabal_macros.h@ (see 'Distribution.Client.Buck2.CabalToBuck') -
      -- real Cabal defines @VERSION_x@\/@MIN_VERSION_x@ for a package's
      -- whole build-depends closure using exactly these solver-resolved
      -- versions, not just whatever version range the @.cabal@ file
      -- itself names.
      let pkgVersions =
            Map.fromList
              [ (packageName pid, packageVersion pid)
              | pkg <- InstallPlan.toList elaboratedPlanOriginal
              , let pid = InstallPlan.foldPlanPackage packageId packageId pkg
              ]
      -- Per-component elaboration gives each local package one
      -- 'ElaboratedConfiguredPackage' per component (library, executable,
      -- ...), all sharing the same directory and the same (whole-package)
      -- 'PackageDescription' - so without this, a package with N
      -- buildable components would get regenerated N times over.
      generateAllPackages verbosity projectRoot pkgVersions (nubBy ((==) `on` fst) localPkgs)

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

-- | Real on-disk source directory for any package this run is going to
-- generate a buck2 rule for - a genuinely local one (always
-- 'LocalUnpackedPackage'; delegates to 'localPackageDir') or an inplace
-- non-local one, resolved the same way
-- 'Distribution.Client.ProjectPlanning.Types.dataDirEnvVarForPackage'
-- does for the same 'BuildInplaceOnly' case: a plain source checkout
-- uses its own path directly, anything fetched as a tarball\/repo was
-- already unpacked to 'distUnpackedSrcDirectory' to be built inplace in
-- the first place.
packageSourceDir :: Verbosity -> DistDirLayout -> ElaboratedConfiguredPackage -> IO FilePath
packageSourceDir verbosity distDirLayout elab
  | elabLocalToProject elab = localPackageDir verbosity elab
  | otherwise = case elabPkgSourceLocation elab of
      LocalUnpackedPackage dir -> return dir
      LocalTarballPackage{} -> return unpackedPath
      RemoteTarballPackage{} -> return unpackedPath
      RepoTarballPackage{} -> return unpackedPath
      RemoteSourceRepoPackage _ (Just localCheckout) -> return localCheckout
      RemoteSourceRepoPackage{} -> dieWithException verbosity (Buck2NonLocalPackageLocation (prettyShow (packageId elab)))
  where
    unpackedPath = distUnpackedSrcDirectory distDirLayout (elabPkgSourceId elab)

-- | Like 'pruneInstallPlanToDependencies', but when excluding every
-- selected target would leave a dangling edge, keep exactly the targets
-- the failure says are still needed instead of giving up outright - and
-- retry, since keeping one target in can itself reveal another one is
-- needed too (transitively).
--
-- This is a real project shape, not a hypothetical: a @build-type:
-- Custom@ local package's Setup.hs can have @setup-depends@ on another
-- *local* package (e.g. cabal-testsuite's Setup needs Cabal-syntax to be
-- built) - the Setup component that creates is a real node in the plan,
-- but isn't itself one of the ordinary library\/exe\/test\/bench targets
-- 'resolveTargetsFromSolver' selects, so plain
-- 'pruneInstallPlanToDependencies' (asked to exclude literally every
-- selected target) sees its now-dangling edge to Cabal-syntax and
-- refuses outright, even though building Cabal-syntax here is exactly
-- what's needed - it's a real dependency of the build, just not of any
-- selected target directly.
pruneToDependenciesNeeded
  :: Set UnitId
  -> ElaboratedInstallPlan
  -> Either CannotPruneDependencies ElaboratedInstallPlan
pruneToDependenciesNeeded excluded plan =
  case pruneInstallPlanToDependencies excluded plan of
    Right pruned -> Right pruned
    Left err@(CannotPruneDependencies broken)
      | Set.null keepIds || excluded' == excluded -> Left err
      | otherwise -> pruneToDependenciesNeeded excluded' plan
      where
        keepIds = Set.fromList [installedUnitId dep | (_, missing) <- broken, dep <- missing]
        excluded' = excluded `Set.difference` keepIds
