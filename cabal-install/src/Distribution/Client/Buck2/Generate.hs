-- | Writes the per-package @BUCK.cabal.bzl@\/@BUCK@ files for every local
-- project package.
--
-- Two files, not one, per package directory:
--
--   * @BUCK.cabal.bzl@ is fully regenerated on every run (it's marked
--     @\@generated@ and never hand-edited) and defines a single
--     @generated_targets()@ macro with one rule call per buildable
--     component.
--   * @BUCK@ is created only if it doesn't already exist, as a two-line
--     file that loads and calls that macro. This is the file a user is
--     free to hand-edit - to add extra targets, or stop calling
--     @generated_targets()@ altogether for a package that needs fully
--     custom rules - without a re-run of @cabal buck2@ ever touching it.
module Distribution.Client.Buck2.Generate
  ( generateAllPackages
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (doesFileExist)
import System.FilePath (makeRelative, (</>))

import qualified Data.Map as Map

import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (packageName)
import Distribution.PackageDescription
  ( Library (exposedModules, reexportedModules)
  , PackageDescription
  , library
  )
import Distribution.Types.ModuleReexport
  ( ModuleReexport (moduleReexportOriginalName, moduleReexportOriginalPackage)
  )
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (Version)

import Distribution.Simple.Utils (notice, warn)

import Distribution.Client.Buck2.CabalToBuck
import Distribution.Client.Buck2.Starlark

-- | Generate\/refresh @BUCK.cabal.bzl@ (and @BUCK@, where missing) for
-- every local package. @projectRoot@ is the buck2 cell root (the
-- directory containing @.buckconfig@), used to turn each package's
-- absolute directory into the cell-relative one buck2 target labels need.
-- @pkgVersions@ is the solver-resolved version of every package in the
-- build plan (local and external), needed to generate each component's
-- own @cabal_macros.h@.
generateAllPackages :: Verbosity -> FilePath -> Map PackageName Version -> [(FilePath, PackageDescription)] -> IO ()
generateAllPackages verbosity projectRoot pkgVersions pkgs = do
  traverse_ (generateOnePackage verbosity localIndex projectRoot pkgVersions) pkgs
  where
    localIndex :: LocalPackageIndex
    localIndex =
      Map.fromList
        [ (packageName pkgDesc, (rootRelativeDir projectRoot pkgDir, reexportOrigins pkgDesc))
        | (pkgDir, pkgDesc) <- pkgs
        ]
    -- Every module exposed by any local package's main library, to
    -- resolve a `reexported-modules:` entry that (as is typical - see
    -- Cabal.cabal's own reexport of Cabal-syntax) names only the bare
    -- module, not an explicit `origin-package:Module` - Cabal itself
    -- resolves that form by searching the reexporting package's own
    -- build-depends for whichever one actually defines it, which for a
    -- *local* origin this index can do too (an external origin doesn't
    -- need this: its real .conf file already declares the reexport
    -- directly to ghc-pkg).
    moduleOwners :: Map.Map ModuleName.ModuleName PackageName
    moduleOwners =
      Map.fromList
        [ (m, packageName pkgDesc)
        | (_, pkgDesc) <- pkgs
        , Just lib <- [library pkgDesc]
        , m <- exposedModules lib
        ]
    reexportOrigins pkgDesc =
      nub
        [ pn
        | Just lib <- [library pkgDesc]
        , reexport <- reexportedModules lib
        , Just pn <- [originPackage reexport]
        , pn /= packageName pkgDesc
        ]
    originPackage reexport = case moduleReexportOriginalPackage reexport of
      Just pn -> Just pn
      Nothing -> Map.lookup (moduleReexportOriginalName reexport) moduleOwners

rootRelativeDir :: FilePath -> FilePath -> FilePath
rootRelativeDir projectRoot pkgDir = case makeRelative projectRoot pkgDir of
  "" -> "."
  rel -> rel

generateOnePackage :: Verbosity -> LocalPackageIndex -> FilePath -> Map PackageName Version -> (FilePath, PackageDescription) -> IO ()
generateOnePackage verbosity localIndex projectRoot pkgVersions (pkgDir, pkgDesc) = do
  targets <- generatePackageTargets verbosity localIndex (rootRelativeDir projectRoot pkgDir) pkgVersions pkgDir pkgDesc
  let pkgName = packageName pkgDesc
  if null (ptCalls targets)
    then warn verbosity $ "cabal buck2: no buck2 targets generated for package " ++ show pkgName
    else do
      let bzlPath = pkgDir </> "BUCK.cabal.bzl"
          buckPath = pkgDir </> "BUCK"
      writeFile bzlPath (renderGeneratedBzl pkgName targets)
      buckExists <- doesFileExist buckPath
      unless buckExists $ writeFile buckPath renderBuckWrapper
      notice verbosity $
        "cabal buck2: generated "
          ++ (rootRelativeDir projectRoot pkgDir </> "BUCK.cabal.bzl")
          ++ " ("
          ++ show (length (ptCalls targets))
          ++ " target(s))"
          ++ (if buckExists then "" else ", created " ++ (rootRelativeDir projectRoot pkgDir </> "BUCK"))

renderGeneratedBzl :: PackageName -> PackageTargets -> String
renderGeneratedBzl pkgName targets =
  unlines
    [ "# @generated by `cabal buck2` from " ++ prettyShow pkgName ++ ".cabal - do not edit by hand."
    , "# Re-run `cabal buck2` after editing the .cabal file to refresh this file."
    ]
    ++ "\n"
    ++ concatMap (uncurry renderLoad) (ptLoads targets)
    ++ "\n"
    ++ "def generated_targets():\n"
    ++ indentBlock (intercalate "\n" (map renderCall (ptCalls targets)))

indentBlock :: String -> String
indentBlock = unlines . map indentLine . lines
  where
    indentLine "" = ""
    indentLine l = "    " ++ l

renderBuckWrapper :: String
renderBuckWrapper =
  unlines
    [ "# Hand-maintained: add extra targets below, or stop calling"
    , "# generated_targets() to fully take over this package's BUCK rules."
    , "load(\":BUCK.cabal.bzl\", \"generated_targets\")"
    , ""
    , "generated_targets()"
    ]
