-- | Turns one local package's already-resolved 'PackageDescription' (flags
-- and conditionals already flattened by the solver, so gated
-- @cxx-sources@\/@ghc-options@\/etc. from an @if flag(...)@ stanza show up
-- here exactly as they should for the resolved build) into the buck2 rule
-- calls for its @BUCK.cabal.bzl@ file: 'haskell_library' \/ 'haskell_binary'
-- \/ 'haskell_test' for each buildable component, plus a 'cxx_library' for
-- any component with @cxx-sources@\/@c-sources@ and an
-- @external_pkgconfig_library@ for each distinct @pkgconfig-depends@.
module Distribution.Client.Buck2.CabalToBuck
  ( LocalPackageIndex
  , PackageTargets (..)
  , generatePackageTargets
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

import qualified Data.Map as Map

import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Compiler (CompilerFlavor (GHC))
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (packageName)
import Distribution.PackageDescription
  ( BuildInfo
  , Executable (exeName, modulePath)
  , Library (exposedModules, libBuildInfo, libName)
  , LibraryName (..)
  , PackageDescription
  , TestSuite (testInterface, testName)
  , TestSuiteInterface (..)
  , cppOptions
  , cxxOptions
  , cxxSources
  , cSources
  , defaultExtensions
  , extraLibs
  , hcOptions
  , hsSourceDirs
  , includeDirs
  , otherModules
  , pkgBuildableComponents
  , pkgconfigDepends
  , targetBuildDepends
  )
import Distribution.Types.Component (Component (..), componentBuildInfo)
import Distribution.Types.Dependency (depLibraries, depPkgName)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (unPkgconfigName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)

import Distribution.Simple.Utils (warn)

import Distribution.Client.Buck2.Starlark

-- | Maps every local project package's name to the buck2 cell-relative
-- directory its @BUCK@ file lives in (@.@ for one at the project root), so
-- a dependency on another local package can be turned into a fully
-- qualified target label.
type LocalPackageIndex = Map PackageName FilePath

-- | The generated rule calls for one package, plus the @load@ statements
-- they need at the top of the file.
data PackageTargets = PackageTargets
  { ptLoads :: [(String, [String])]
  , ptCalls :: [Call]
  }

instance Semigroup PackageTargets where
  PackageTargets l1 c1 <> PackageTargets l2 c2 = PackageTargets (foldl' addLoad l1 l2) (c1 ++ c2)
    where
      addLoad acc (tgt, names) = case lookup tgt acc of
        Nothing -> acc ++ [(tgt, names)]
        Just _ -> map (\(t, ns) -> if t == tgt then (t, nub (ns ++ names)) else (t, ns)) acc

instance Monoid PackageTargets where
  mempty = PackageTargets [] []

-- | Generate the buck2 targets for every buildable component of a local
-- package. @pkgDir@ is the package's own directory (where the generated
-- @BUCK.cabal.bzl@ will live) - every emitted path is relative to it.
generatePackageTargets
  :: Verbosity
  -> LocalPackageIndex
  -> FilePath
  -> PackageDescription
  -> IO PackageTargets
generatePackageTargets verbosity localIndex pkgDir pkgDesc = do
  targets <- mconcat <$> traverse (generateComponent verbosity localIndex pkgDir pkgDesc) (pkgBuildableComponents pkgDesc)
  return targets{ptCalls = dedupPkgconfigCalls (ptCalls targets)}

-- | Two components of the *same* package sharing a @pkgconfig-depends@
-- each generate their own @external_pkgconfig_library()@ call (from
-- 'cxxLibraryFor', called once per component) - harmless on its own, but
-- both would declare the same target @name@ in the same
-- @generated_targets()@, which buck2 rejects as a duplicate target. Kept
-- as a post-pass here (rather than threading a running set through
-- component generation) so each component's own generation stays
-- self-contained; cross-*package* duplicates - two different local
-- packages needing the same system library - aren't addressed by this,
-- since each package's calls only ever collide with its own.
dedupPkgconfigCalls :: [Call] -> [Call]
dedupPkgconfigCalls = go []
  where
    go _ [] = []
    go seen (c : cs)
      | callFn c == "external_pkgconfig_library"
      , Just (VStr n) <- lookup "name" (callArgs c) =
          if n `elem` seen then go seen cs else c : go (n : seen) cs
      | otherwise = c : go seen cs

generateComponent
  :: Verbosity
  -> LocalPackageIndex
  -> FilePath
  -> PackageDescription
  -> Component
  -> IO PackageTargets
generateComponent verbosity localIndex pkgDir pkgDesc comp = case comp of
  CLib lib -> library (libTargetName (packageName pkgDesc) (libName lib)) lib
  CExe exe -> executable exe
  CTest test -> testSuite test
  CFLib _ -> skip "foreign library (not supported yet)"
  CBench _ -> skip "benchmark (not supported yet)"
  where
    skip why = do
      warn verbosity $ "cabal buck2: skipping " ++ why ++ " in package " ++ unPackageName (packageName pkgDesc)
      return mempty

    library targetName lib = do
      let bi = libBuildInfo lib
      srcs <- resolveModules verbosity pkgDir bi (exposedModules lib ++ otherModules bi)
      (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex pkgDir targetName bi
      let (pkgs, deps) = classifyDeps localIndex bi
          hlCall =
            call
              "haskell_library"
              ( [ ("name", str targetName)
                , ("srcs", VDict srcs)
                ]
                  ++ compilerFlagsArg bi
                  ++ exportedLinkerFlagsArg bi
                  ++ optionalListArg "packages" pkgs
                  ++ optionalListArg "deps" (deps ++ cxxDeps)
                  ++ [("visibility", strList ["PUBLIC"])]
              )
      return $
        PackageTargets
          (("//buck2:haskell.bzl", ["haskell_library"]) : cxxLoads)
          (cxxCalls ++ [hlCall])

    executable exe = do
      let bi = componentBuildInfo (CExe exe)
          targetName = unUnqualComponentName (exeName exe)
      mainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath (modulePath exe))
      (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex pkgDir targetName bi
      let (pkgs, deps) = classifyDeps localIndex bi
          binCall =
            call
              "haskell_binary"
              ( [ ("name", str targetName)
                , ("srcs", VDict [("Main.hs", str mainSrc)])
                ]
                  ++ compilerFlagsArg bi
                  ++ linkerFlagsArg bi
                  ++ optionalListArg "packages" pkgs
                  ++ optionalListArg "deps" (deps ++ cxxDeps)
                  ++ [("visibility", strList ["PUBLIC"])]
              )
      return $
        PackageTargets
          (("//buck2:haskell.bzl", ["haskell_binary"]) : cxxLoads)
          (cxxCalls ++ [binCall])

    testSuite test = case testInterface test of
      TestSuiteExeV10 _ver mainIs -> do
        let bi = componentBuildInfo (CTest test)
            targetName = unUnqualComponentName (testName test)
        mainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath mainIs)
        (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex pkgDir targetName bi
        let (pkgs, deps) = classifyDeps localIndex bi
            testCall =
              call
                "haskell_test"
                ( [ ("name", str targetName)
                  , ("srcs", VDict [("Main.hs", str mainSrc)])
                  ]
                    ++ compilerFlagsArg bi
                    ++ linkerFlagsArg bi
                    ++ optionalListArg "packages" pkgs
                    ++ optionalListArg "deps" (deps ++ cxxDeps)
                )
        return $
          PackageTargets
            (("//buck2:haskell.bzl", ["haskell_test"]) : cxxLoads)
            (cxxCalls ++ [testCall])
      _ ->
        skip
          ( "test-suite "
              ++ unUnqualComponentName (testName test)
              ++ " (only exitcode-stdio-1.0 test-suites are supported)"
          )

-- | @ghc-options@ + @cpp-options@ + @default-extensions@ (as @-X...@
-- flags), the sources of per-component GHC flags buck2's @compiler_flags@
-- covers. @other-extensions@ is deliberately excluded: those are declared
-- via in-module @LANGUAGE@ pragmas, not enabled component-wide.
compilerFlagsArg :: BuildInfo -> [(String, Value)]
compilerFlagsArg bi = optionalListArg "compiler_flags" (hcOptions GHC bi ++ cppOptions bi ++ extensionFlags)
  where
    extensionFlags = ["-X" ++ prettyShow ext | ext <- defaultExtensions bi]

-- | @extra-libraries@ on a library, as @-l@ flags on its
-- @exported_linker_flags@ - a local fork of buck2/prelude/haskell/
-- haskell.bzl (see buck2\/buck2.md) adds this attr to haskell_library(),
-- propagating to whatever finally links against it via the standard
-- native-link-info machinery (unlike plain @linker_flags@, which
-- haskell_library() only ever applies to its own @.so@ link step - see
-- that fork's own commit for the full story of why this was needed
-- instead of just using @linker_flags@ here).
exportedLinkerFlagsArg :: BuildInfo -> [(String, Value)]
exportedLinkerFlagsArg bi = optionalListArg "exported_linker_flags" ["-l" ++ lib | lib <- extraLibs bi]

-- | @extra-libraries@ on an executable\/test-suite, as @-l@ flags on its
-- plain @linker_flags@ - correct as-is here (unlike on a library): both
-- rules already apply @linker_flags@ directly to their own, one and only,
-- final executable link.
linkerFlagsArg :: BuildInfo -> [(String, Value)]
linkerFlagsArg bi = optionalListArg "linker_flags" ["-l" ++ lib | lib <- extraLibs bi]

optionalListArg :: String -> [String] -> [(String, Value)]
optionalListArg _ [] = []
optionalListArg name xs = [(name, strList (nub xs))]

-- | The buck2 target name for one of a package's libraries: the package
-- name itself for the main (unnamed) library, matching every other
-- reference to it (@packages = [...]@, other packages' @build-depends@,
-- ...); the sub-library's own unqualified name otherwise - always unique
-- within one package's BUCK file, since Cabal itself already requires
-- every component name in a package to be distinct.
libTargetName :: PackageName -> LibraryName -> String
libTargetName pn LMainLibName = unPackageName pn
libTargetName _ (LSubLibName n) = unUnqualComponentName n

-- | Split a component's @build-depends@ (each of which may name one or
-- more specific sub-libraries of a package via @pkg:sublib@ - see
-- 'depLibraries') into external package names (fed to
-- buck2/haskell.bzl's @packages =@ convenience param - which only
-- resolves a package's main library, so a named sub-library of an
-- *external* package still only contributes its package name here, same
-- as before this distinguished sub-libraries at all) and local-project
-- target labels (fed to @deps =@, correctly pointing at the specific
-- local sub-library's own target when one was named).
classifyDeps :: LocalPackageIndex -> BuildInfo -> ([String], [String])
classifyDeps localIndex bi =
  ( nub [unPackageName pn | (pn, _) <- depPairs, not (Map.member pn localIndex)]
  , nub [localTargetLabel dir (libTargetName pn ln) | (pn, ln) <- depPairs, Just dir <- [Map.lookup pn localIndex]]
  )
  where
    depPairs =
      nub
        [ (depPkgName d, ln)
        | d <- targetBuildDepends bi
        , ln <- NES.toList (depLibraries d)
        ]

localTargetLabel :: FilePath -> String -> String
localTargetLabel dir targetName = "//" ++ (if dir == "." then "" else dir) ++ ":" ++ targetName

-- | Resolve each module in @hs-source-dirs@ to its real file, trying
-- @.hs@\/@.lhs@\/@.hsc@ in turn (the extensions buck2/hsc2hs.bzl knows how
-- to handle) - returning @(moduleDerivedPath, realRelativePath)@ pairs for
-- the dict form of @srcs@, which - unlike the plain-list form - is
-- unaffected by @hs-source-dirs@ not matching the BUCK package's own
-- directory.
resolveModules :: Verbosity -> FilePath -> BuildInfo -> [ModuleName.ModuleName] -> IO [(String, Value)]
resolveModules verbosity pkgDir bi mods = traverse (resolveOne verbosity pkgDir (sourceDirs bi)) mods

resolveOne :: Verbosity -> FilePath -> [FilePath] -> ModuleName.ModuleName -> IO (String, Value)
resolveOne verbosity pkgDir dirs m = do
  let modPath = ModuleName.toFilePath m
      hsPath = modPath <.> "hs"
      guess = firstDir dirs </> hsPath
  -- buck2/haskell.bzl's own srcs-resolution (_resolve_src) auto-detects
  -- .hsc/.x/.y by the *source* file's extension and runs it through
  -- hsc2hs()/alex()/happy() - already loaded by haskell.bzl itself, so
  -- nothing extra needs to be loaded here for that to work.
  found <- firstExisting pkgDir dirs [modPath <.> ext | ext <- ["hs", "lhs", "hsc", "x", "y"]]
  case found of
    Just real -> return (hsPath, str real)
    Nothing -> do
      warn verbosity $
        "cabal buck2: couldn't find a source file for module "
          ++ prettyShow m
          ++ " under "
          ++ intercalate ", " dirs
          ++ " - guessing "
          ++ guess
      return (hsPath, str guess)

resolveMainIs :: Verbosity -> FilePath -> BuildInfo -> FilePath -> IO String
resolveMainIs verbosity pkgDir bi mainIs = do
  found <- firstExisting pkgDir (sourceDirs bi) [mainIs]
  case found of
    Just real -> return real
    Nothing -> do
      warn verbosity $
        "cabal buck2: couldn't find main-is file " ++ mainIs ++ " under " ++ intercalate ", " (sourceDirs bi)
      return (firstDir (sourceDirs bi) </> mainIs)

-- | 'sourceDirs' is never actually empty (it defaults to @["."]@), but its
-- type doesn't say so - and 'Distribution.Client.Compat.Prelude' shadows
-- the standard partial 'head' with a 'NonEmpty'-only one, so a plain
-- fallback is simpler here than threading a 'NonEmpty' through.
firstDir :: [FilePath] -> FilePath
firstDir = fromMaybe "." . listToMaybe

firstExisting :: FilePath -> [FilePath] -> [FilePath] -> IO (Maybe FilePath)
firstExisting pkgDir dirs candidates =
  listToMaybe . catMaybes
    <$> sequenceA
      [ do
        exists <- doesFileExist (pkgDir </> dir </> candidate)
        return (if exists then Just (dir </> candidate) else Nothing)
      | dir <- dirs
      , candidate <- candidates
      ]

sourceDirs :: BuildInfo -> [FilePath]
sourceDirs bi = case map getSymbolicPath (hsSourceDirs bi) of
  [] -> ["."]
  ds -> ds

-- | A 'cxx_library' for a component's @cxx-sources@\/@c-sources@, plus an
-- @external_pkgconfig_library@ for each distinct @pkgconfig-depends@ it
-- needs - or nothing at all if the component has no C\/C++ sources.
cxxLibraryFor
  :: LocalPackageIndex
  -> FilePath
  -> String
  -> BuildInfo
  -> IO ([(String, [String])], [String], [Call])
cxxLibraryFor _localIndex _pkgDir targetName bi
  | null srcs = return ([], [], [])
  | otherwise =
      return
        ( ("//buck2:cxx.bzl", ["cxx_library"])
            : [("@prelude//third-party:pkgconfig.bzl", ["external_pkgconfig_library"]) | not (null pkgconfigNames)]
        , [":" ++ cxxTargetName]
        , pkgconfigCalls ++ [cxxCall]
        )
  where
    srcs = map getSymbolicPath (cSources bi ++ cxxSources bi)
    cxxTargetName = targetName ++ "-cxx"
    includeFlags = ["-I" ++ getSymbolicPath d | d <- includeDirs bi]
    pkgconfigNames = nub [unPkgconfigName n | PkgconfigDependency n _ <- pkgconfigDepends bi]
    pkgconfigCalls =
      [ call
        "external_pkgconfig_library"
        [("name", str ("pkgconfig-" ++ n)), ("package", str n), ("visibility", strList ["PUBLIC"])]
      | n <- pkgconfigNames
      ]
    -- buck2/cxx.bzl's cxx_library() wrapper adds -std=c++20 to
    -- compiler_flags whenever cxx_std isn't explicitly turned off - and
    -- that flag applies to every source in the target, C included, so a
    -- component with c-sources but no cxx-sources needs it turned off
    -- entirely (clang/gcc reject -std=c++20 for a plain .c compile).
    cxxCall =
      call
        "cxx_library"
        ( [ ("name", str cxxTargetName)
          , ("srcs", strList srcs)
          ]
            ++ optionalListArg "exported_preprocessor_flags" includeFlags
            ++ optionalListArg "compiler_flags" (cxxOptions bi)
            ++ optionalListArg "deps" [":pkgconfig-" ++ n | n <- pkgconfigNames]
            ++ [("visibility", strList ["PUBLIC"])]
            ++ [("cxx_std", VBool False) | null (cxxSources bi)]
        )
