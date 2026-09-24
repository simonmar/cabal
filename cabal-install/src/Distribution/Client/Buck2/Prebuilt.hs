-- | Turns the already-resolved, already-built dependency closure of the
-- project (everything @cabal buck2@ just built via @--only-dependencies@)
-- into @third-party\/haskell@: a @haskell_prebuilt_library()@ per package,
-- a filtered\/recached package db holding just the @.conf@ files those
-- rules reference, and repo-relative symlinks to GHC and the cabal store
-- so the generated paths aren't host-specific.
--
-- This is an in-process rewrite of what used to be the external
-- @buck2\/gen-haskell-prebuilt.py@ script. Doing it here instead means:
--
--   * no re-deriving GHC's version\/paths from @dist-newstyle\/cache\/
--     plan.json@ - they're already sitting in the 'ElaboratedSharedConfig'
--     \/'CabalDirLayout' this command already elaborated.
--   * no shelling out to a (possibly different-version) @cabal@ on
--     @$PATH@ - which used to cause index-cache parse errors when it
--     didn't match the @cabal@ actually running.
--   * no globbing for the store root's ABI-tag suffix - 'storeDirectory'
--     computes it exactly.
--   * the dependency closure comes from 'elaboratedPlanToExecute' - the
--     exact, already test\/benchmark-flag-pruned plan @cabal buck2@ just
--     built - rather than re-walking every local component's
--     @build-depends@ unconditionally (which used to pull in test-only
--     dependencies that were never actually built when
--     @--enable-tests@ wasn't passed, producing spurious "not found"
--     warnings).
module Distribution.Client.Buck2.Prebuilt
  ( generatePrebuilt
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.List (stripPrefix)
import qualified Data.Map as Map

import System.Directory
  ( createDirectoryIfMissing
  , createFileLink
  , doesFileExist
  , doesPathExist
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath
  ( isAbsolute
  , isPathSeparator
  , joinPath
  , makeRelative
  , pathSeparator
  , splitDirectories
  , splitPath
  , takeDirectory
  , (<.>)
  , (</>)
  )

import qualified Distribution.Client.InstallPlan as InstallPlan

import Distribution.Client.DistDirLayout
  ( CabalDirLayout (cabalStoreDirLayout)
  , StoreDirLayout (storeDirectory, storePackageDBPath)
  )
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage (elabInstallDirs)
  , ElaboratedInstallPlan
  , ElaboratedSharedConfig (pkgConfigCompiler, pkgConfigCompilerProgs, pkgConfigPlatform)
  )

import Distribution.InstalledPackageInfo (parseInstalledPackageInfo)
import Distribution.Package (HasUnitId (installedUnitId), packageName, packageVersion)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.Compiler
  ( Compiler (compilerProperties)
  , compilerVersion
  )
import Distribution.Simple.GHC (getGlobalPackageDB)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program.Builtin (ghcPkgProgram, ghcProgram)
import Distribution.Simple.Program.Db (lookupProgram)
import Distribution.Simple.Program.Types (ConfiguredProgram, programPath)
import Distribution.Simple.Utils (dieWithException, notice, rawSystemExit, warn)
import Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo
      ( depends
      , extraLibraries
      , hsLibraries
      , includeDirs
      , libraryDirs
      , libraryDynDirs
      )
  )
import Distribution.Types.UnitId (UnitId, unUnitId)

import Distribution.Client.Errors
  ( CabalInstallException (Buck2NoGhcPkgProgram, Buck2NoGhcProgram)
  )

import Distribution.Client.Buck2.Starlark

-- | Generate\/refresh @third-party\/haskell@ from the dependency closure
-- of an already-built, already-pruned install plan.
generatePrebuilt
  :: Verbosity
  -> FilePath
  -- ^ project root (the buck2 cell root)
  -> CabalDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -- ^ 'elaboratedPlanToExecute': already pruned to exactly the (test\/
  -- benchmark-flag-aware) dependency closure that was just built - local
  -- packages themselves are not in this plan.
  -> IO ()
generatePrebuilt verbosity projectRoot cabalDirLayout shared depsPlan = do
  ghcProg <-
    maybe (dieWithException verbosity Buck2NoGhcProgram) return $
      lookupProgram ghcProgram (pkgConfigCompilerProgs shared)
  ghcPkgProg <-
    maybe (dieWithException verbosity Buck2NoGhcPkgProgram) return $
      lookupProgram ghcPkgProgram (pkgConfigCompilerProgs shared)

  let compiler = pkgConfigCompiler shared
      ghcVersionStr = prettyShow (compilerVersion compiler)
      ghcDynamic = Map.lookup "GHC Dynamic" (compilerProperties compiler) == Just "YES"
      storeLayout = cabalStoreDirLayout cabalDirLayout
      storeDB = storePackageDBPath storeLayout compiler
      storeRootAbs = storeDirectory storeLayout compiler
      targetDir = projectRoot </> "third-party" </> "haskell"
      targetStoreDB = targetDir </> "store-db"
      ghcBinAbs = takeDirectory (programPath ghcProg)

  globalDB <- getGlobalPackageDB verbosity ghcProg
  let globalRootAbs = takeDirectory globalDB
      unitIds = nub [installedUnitId pkg | pkg <- InstallPlan.toList depsPlan]
      paths = RepoPaths{rpGhcVersion = ghcVersionStr, rpGlobalRootAbs = globalRootAbs, rpStoreRootAbs = storeRootAbs}

  createDirectoryIfMissing True targetDir
  ensureSymlink (targetDir </> ("ghc-" ++ ghcVersionStr)) globalRootAbs
  ensureSymlink (targetDir </> "ghc-bin") ghcBinAbs

  notice verbosity "cabal buck2: resolving prebuilt dependency closure"
  packages <- catMaybes <$> traverse (readPackage verbosity paths storeDB) unitIds
  let alexPath = findToolBinary paths shared depsPlan "alex"
      happyPath = findToolBinary paths shared depsPlan "happy"

  -- Needed by any package's own library files, *and* independently by
  -- alex/happy's own binary path - a build-tool-only dependency (an
  -- executable, no library) contributes no ResolvedPackage at all (see
  -- readPackage), so checking `packages` alone would miss a project
  -- that needs alex/happy but nothing else store-installed.
  when (any (not . rpIsGlobal) packages || any inStore [alexPath, happyPath]) $
    ensureSymlink (targetDir </> "cabal-store") storeRootAbs

  notice verbosity "cabal buck2: building filtered store package db"
  setupStoreDB verbosity ghcPkgProg targetStoreDB packages

  notice verbosity "cabal buck2: generating third-party/haskell/BUCK"
  writeBuckFile targetDir paths packages

  writeToolsFile targetDir ghcVersionStr ghcDynamic alexPath happyPath

-- | The three repo-relative anchors every generated path is expressed
-- against: the symlinks 'generatePrebuilt' just created, plus the GHC
-- version string (needed for both the global db's own relative path and
-- shared-library sonames).
data RepoPaths = RepoPaths
  { rpGhcVersion :: String
  , rpGlobalRootAbs :: FilePath
  , rpStoreRootAbs :: FilePath
  }

-- | A resolved package: its metadata plus which package db it came from
-- (needed to pick @db = "ghc-<ver>/package.conf.d"@ vs @db = "store-db"@
-- in the generated rule, and to know where to find its @.conf@ file to
-- symlink into the filtered store db).
data ResolvedPackage = ResolvedPackage
  { rpInfo :: InstalledPackageInfo
  , rpUnitId :: UnitId
  , rpIsGlobal :: Bool
  , rpConfPath :: FilePath
  , rpStaticLibs :: [FilePath]
  , rpProfiledLibs :: [FilePath]
  , rpSharedLibs :: [(String, FilePath)]
  }

-- | Every package's @.conf@ file lives at exactly one of two deterministic
-- locations - no searching required, unlike the Python version (which had
-- to glob for the store root, not knowing its ABI-tag suffix in advance).
-- Which one is decided by the unit id's own shape: GHC's global\/boot
-- packages have a plain @name-version@ id, everything else gets a
-- hash-suffixed one once installed to the store (the same heuristic
-- gen-haskell-prebuilt.py used).
confPath :: FilePath -> FilePath -> UnitId -> FilePath
confPath globalRootAbs storeDB uid
  | isGlobalUnitId uid = globalRootAbs </> "package.conf.d" </> unUnitId uid <.> "conf"
  | otherwise = storeDB </> unUnitId uid <.> "conf"

isGlobalUnitId :: UnitId -> Bool
isGlobalUnitId uid = case break (== '-') (reverse (unUnitId uid)) of
  (revSuffix, '-' : _) -> not (length revSuffix >= 20 && all isHexDigit revSuffix)
  _ -> True

readPackage :: Verbosity -> RepoPaths -> FilePath -> UnitId -> IO (Maybe ResolvedPackage)
readPackage verbosity paths storeDB uid = do
  let path = confPath (rpGlobalRootAbs paths) storeDB uid
  exists <- doesFileExist path
  if not exists
    then do
      warn verbosity $ "cabal buck2: no .conf file for " ++ unUnitId uid ++ " (expected at " ++ path ++ ")"
      return Nothing
    else do
      contents <- BS.readFile path
      case parseInstalledPackageInfo contents of
        Left errs -> do
          warn verbosity $ "cabal buck2: couldn't parse " ++ path ++ ": " ++ intercalate "; " (toList errs)
          return Nothing
        Right (_warnings, ipi0) -> do
          -- '${pkgroot}' is the directory *containing* the package db (one
          -- level above the .conf file's own db directory, e.g.
          -- ".../lib" for a ".../lib/package.conf.d/<uid>.conf" file) -
          -- not the db directory itself.
          let ipi = mungePkgroot (takeDirectory (takeDirectory path)) ipi0
              isRts = prettyShow (packageName ipi) == "rts"
          staticLibs <- findLibs paths (libraryDirs ipi) [("lib" ++ stem <.> "a") | stem <- hsLibraries ipi]
          -- GHC doesn't build profiled RTS libraries the normal way - see
          -- gen-haskell-prebuilt.py's own note on this, which this
          -- inherits without fully understanding why either.
          profiledLibs <-
            if isRts
              then return []
              else findLibs paths (libraryDirs ipi) [("lib" ++ stem ++ "_p" <.> "a") | stem <- hsLibraries ipi]
          sharedLibs <- findSharedLibs paths ipi
          return $ Just (ResolvedPackage ipi uid (isGlobalUnitId uid) path staticLibs profiledLibs sharedLibs)

-- | Resolve each candidate filename against @dirs@ in turn, keeping only
-- the ones that actually exist on disk (unlike gen-haskell-prebuilt.py's
-- Python predecessor, this doesn't assume every @hs-libraries@ stem has a
-- library of every flavour - e.g. rts's second stem, \"Cffi\", only ever
-- ships as a static archive, never as a @.so@).
findLibs :: RepoPaths -> [FilePath] -> [String] -> IO [FilePath]
findLibs paths dirs fnames = catMaybes <$> traverse (findOne paths dirs) fnames

findOne :: RepoPaths -> [FilePath] -> String -> IO (Maybe FilePath)
findOne paths dirs fname = go dirs
  where
    go [] = return Nothing
    go (d : ds) = do
      let absPath = d </> fname
      exists <- doesFileExist absPath
      if exists
        then return (toRepoRelative paths absPath)
        else go ds

findSharedLibs :: RepoPaths -> InstalledPackageInfo -> IO [(String, FilePath)]
findSharedLibs paths ipi = catMaybes <$> traverse oneStem (hsLibraries ipi)
  where
    dirs = libraryDynDirs ipi ++ libraryDirs ipi
    oneStem stem = do
      let soname = "lib" ++ stem ++ "-ghc" ++ rpGhcVersion paths ++ ".so"
      mpath <- findOne paths dirs soname
      return ((,) soname <$> mpath)

-- | @.conf@ files use @${pkgroot}@ (the directory containing the package
-- db) as a portable stand-in for their own absolute location - GHC's own
-- @ghc-pkg@ expands this when it serves package info, but reading the
-- file directly (as 'parseInstalledPackageInfo' does here) doesn't, so
-- library-dirs\/include-dirs come back as literal, unusable
-- @"${pkgroot}/..."@ strings unless expanded by hand. Mirrors
-- 'Distribution.Simple.Program.HcPkg.mungePackagePaths', which isn't
-- exported.
mungePkgroot :: FilePath -> InstalledPackageInfo -> InstalledPackageInfo
mungePkgroot pkgroot ipi =
  ipi
    { libraryDirs = map munge (libraryDirs ipi)
    , libraryDynDirs = map munge (libraryDynDirs ipi)
    , includeDirs = map munge (includeDirs ipi)
    }
  where
    munge p = maybe p collapseDotDot (stripPkgroot p)
    stripPkgroot p = case splitPath p of
      (root : rest) -> case stripPrefix "${pkgroot}" root of
        Just [sep] | isPathSeparator sep -> Just (pkgroot </> joinPath rest)
        _ -> Nothing
      _ -> Nothing

-- | @${pkgroot}@ substitution routinely produces a @.../package.conf.d/
-- ../lib/...@ path (since @${pkgroot}@ is the db directory, and the real
-- libraries live next to it, not under it) - harmless as a real filesystem
-- path, but buck2 rejects any @attrs.source()@ containing a literal
-- @".."@ component ("expected a normalized path"), so it has to be
-- collapsed before it ever reaches a generated rule. Unlike
-- 'System.FilePath.normalise' (which only tidies separators\/dots, not
-- @".."@ segments - not safe in general with symlinks in play, but safe
-- here since every path this is applied to is already fully resolved:
-- real, non-symlink directories under GHC's own libdir or the cabal
-- store).
collapseDotDot :: FilePath -> FilePath
collapseDotDot = joinPath . reverse . foldl' step [] . splitDirectories
  where
    step (top : rest) ".." | top /= ".." && top /= [pathSeparator] = rest
    step stack "." = stack
    step stack seg = seg : stack

-- | Point @link@ at @target@, creating or repointing it as needed.
ensureSymlink :: FilePath -> FilePath -> IO ()
ensureSymlink link target = do
  exists <- doesPathExist link
  when exists $ removeFile link
  createFileLink target link

setupStoreDB :: Verbosity -> ConfiguredProgram -> FilePath -> [ResolvedPackage] -> IO ()
setupStoreDB verbosity ghcPkgProg targetStoreDB packages = do
  exists <- doesPathExist targetStoreDB
  when exists $ removeDirectoryRecursive targetStoreDB
  createDirectoryIfMissing True targetStoreDB
  traverse_
    (\p -> createFileLink (rpConfPath p) (targetStoreDB </> unUnitId (rpUnitId p) <.> "conf"))
    (filter (not . rpIsGlobal) packages)
  rawSystemExit verbosity Nothing (programPath ghcPkgProg) ["--package-db", targetStoreDB, "recache"]

writeBuckFile :: FilePath -> RepoPaths -> [ResolvedPackage] -> IO ()
writeBuckFile targetDir paths packages =
  writeFile (targetDir </> "BUCK") (renderFile header [] calls)
  where
    header = "@generated by `cabal buck2` - do not edit by hand.\nRe-run `cabal buck2` to update."
    uidToTarget = Map.fromList [(rpUnitId p, targetName p) | p <- packages]
    calls = [prebuiltCall paths uidToTarget p | p <- packages]

targetName :: ResolvedPackage -> String
targetName p = prettyShow (packageName (rpInfo p))

prebuiltCall :: RepoPaths -> Map UnitId String -> ResolvedPackage -> Call
prebuiltCall paths uidToTarget p =
  call
    "haskell_prebuilt_library"
    ( [ ("name", str (targetName p))
      , ("version", str (prettyShow (packageVersion (rpInfo p))))
      , ("id", str (unUnitId (rpUnitId p)))
      , ("db", str (if rpIsGlobal p then globalDbRel else "store-db"))
      , ("static_libs", strList (rpStaticLibs p))
      ]
        ++ [("profiled_static_libs", strList (rpProfiledLibs p)) | not (null (rpProfiledLibs p))]
        ++ [("pic_profiled_static_libs", strList (rpProfiledLibs p)) | not (null (rpProfiledLibs p))]
        ++ [("shared_libs", VDict [(soname, str sopath) | (soname, sopath) <- rpSharedLibs p])]
        ++ [("cxx_header_dirs", strList headerDirs) | not (null headerDirs)]
        ++ [("exported_linker_flags", strList extraLinkerFlags) | not (null extraLinkerFlags)]
        ++ [("deps", strList depTargets) | not (null depTargets)]
        ++ [("visibility", strList ["PUBLIC"])]
    )
  where
    info = rpInfo p
    globalDbRel = ("ghc-" ++ rpGhcVersion paths) </> "package.conf.d"
    headerDirs = mapMaybe (toRepoRelative paths) (includeDirs info)
    extraLinkerFlags = ["-l" ++ lib | lib <- extraLibraries info]
    depTargets = nub [":" ++ t | d <- depends info, Just t <- [Map.lookup d uidToTarget]]

-- | Convert an absolute path under GHC's libdir or the cabal store into
-- one relative to @third-party\/haskell@, via whichever of the two
-- symlinks 'generatePrebuilt' pointed there actually contains it - the
-- same translation gen-haskell-prebuilt.py's own @abs_to_rel@ did.
toRepoRelative :: RepoPaths -> FilePath -> Maybe FilePath
toRepoRelative paths path
  | not (isAbsolute path) = Nothing
  | otherwise =
      relTo (rpGlobalRootAbs paths) ("ghc-" ++ rpGhcVersion paths) path
        <|> relTo (rpStoreRootAbs paths) "cabal-store" path

relTo :: FilePath -> FilePath -> FilePath -> Maybe FilePath
relTo root repoRelPrefix path =
  let r = makeRelative root path
   in if r /= path then Just (repoRelPrefix </> r) else Nothing

-- | The store-installed binary path (repo-relative, like everything else
-- 'toRepoRelative' produces) for a build-tool dependency package - e.g.
-- @alex@\/@happy@, needed by buck2/alex_happy.bzl to preprocess @.x@\/@.y@
-- sources - or 'Nothing' if the project doesn't need it at all. A
-- simplified 'CmdListBin.elaboratedPackage'\/@bin_file'@: alex\/happy are
-- always external Hackage dependencies, never a local package, so the
-- inplace-build-style branch that logic also has to handle never applies
-- here, and (being plain, single-executable packages) their own
-- executable is always named after the package itself, with no need to
-- resolve a target selector to find out which component that is.
findToolBinary :: RepoPaths -> ElaboratedSharedConfig -> ElaboratedInstallPlan -> String -> Maybe FilePath
findToolBinary paths shared plan toolName =
  listToMaybe
    [ rel
    | pkg <- InstallPlan.toList plan
    , Just elab <- [configuredOrInstalled pkg]
    , prettyShow (packageName elab) == toolName
    , let absPath = InstallDirs.bindir (elabInstallDirs elab) </> toolName <.> exeExtension (pkgConfigPlatform shared)
    , Just rel <- [toRepoRelative paths absPath]
    ]

-- | Same package, in either of the two states a *non-local* dependency
-- that's actually going to be used can be in: 'Configured' (needs
-- building this run) or 'Installed' (already built and installed from a
-- previous run, nothing to do - which is what alex/happy settle into on
-- any @cabal buck2@ after the first, once their build is cached). Unlike
-- 'installedUnitId' (a 'HasUnitId' method, already defined uniformly
-- across all three 'GenericPlanPackage' constructors), there's no
-- existing helper for this, since most other call sites here only need
-- the unit id, not the full 'ElaboratedConfiguredPackage'.
configuredOrInstalled :: InstallPlan.GenericPlanPackage ipkg srcpkg -> Maybe srcpkg
configuredOrInstalled (InstallPlan.Configured spkg) = Just spkg
configuredOrInstalled (InstallPlan.Installed spkg) = Just spkg
configuredOrInstalled InstallPlan.PreExisting{} = Nothing

inStore :: Maybe FilePath -> Bool
inStore = maybe False ("cabal-store" `isPrefixOf`)

writeToolsFile :: FilePath -> String -> Bool -> Maybe FilePath -> Maybe FilePath -> IO ()
writeToolsFile targetDir ghcVersionStr ghcDynamic alexPath happyPath =
  writeFile (targetDir </> "tools.bzl") $
    unlines
      [ "# @generated by `cabal buck2` - do not edit by hand."
      , "# Re-run `cabal buck2` to update."
      , ""
      , "GHC_VERSION = " ++ show ghcVersionStr
      , "GHC_BIN_DIR = \"third-party/haskell/ghc-bin\""
      , "GHC_DYNAMIC = " ++ (if ghcDynamic then "True" else "False")
      , ""
      , "ALEX = " ++ show ("third-party/haskell/" ++ fromMaybe "missing" alexPath)
      , "HAPPY = " ++ show ("third-party/haskell/" ++ fromMaybe "missing" happyPath)
      ]
