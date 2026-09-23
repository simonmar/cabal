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
  , hcOptions
  , hsSourceDirs
  , includeDirs
  , otherModules
  , pkgBuildableComponents
  , pkgconfigDepends
  , targetBuildDepends
  )
import Distribution.Types.Component (Component (..), componentBuildInfo)
import Distribution.Types.Dependency (depPkgName)
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
generatePackageTargets verbosity localIndex pkgDir pkgDesc =
  mconcat <$> traverse (generateComponent verbosity localIndex pkgDir pkgDesc) (pkgBuildableComponents pkgDesc)

generateComponent
  :: Verbosity
  -> LocalPackageIndex
  -> FilePath
  -> PackageDescription
  -> Component
  -> IO PackageTargets
generateComponent verbosity localIndex pkgDir pkgDesc comp = case comp of
  CLib lib
    | libName lib == LMainLibName -> library (unPackageName (packageName pkgDesc)) lib
    | otherwise -> skip ("named sub-library " ++ prettyLibName (libName lib))
  CExe exe -> executable exe
  CTest test -> testSuite test
  CFLib _ -> skip "foreign library (not supported yet)"
  CBench _ -> skip "benchmark (not supported yet)"
  where
    skip why = do
      warn verbosity $ "cabal buck2: skipping " ++ why ++ " in package " ++ unPackageName (packageName pkgDesc)
      return mempty

    prettyLibName (LSubLibName n) = unUnqualComponentName n
    prettyLibName LMainLibName = "(main library)"

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

optionalListArg :: String -> [String] -> [(String, Value)]
optionalListArg _ [] = []
optionalListArg name xs = [(name, strList (nub xs))]

-- | Split a component's @build-depends@ into external package names (fed
-- to buck2/haskell.bzl's @packages =@ convenience param) and local-project
-- target labels (fed to @deps =@).
classifyDeps :: LocalPackageIndex -> BuildInfo -> ([String], [String])
classifyDeps localIndex bi =
  ( [unPackageName pn | pn <- depNames, not (Map.member pn localIndex)]
  , [localTargetLabel dir pn | pn <- depNames, Just dir <- [Map.lookup pn localIndex]]
  )
  where
    depNames = nub (map depPkgName (targetBuildDepends bi))

localTargetLabel :: FilePath -> PackageName -> String
localTargetLabel dir pn = "//" ++ (if dir == "." then "" else dir) ++ ":" ++ unPackageName pn

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
  found <- firstExisting pkgDir dirs [modPath <.> ext | ext <- ["hs", "lhs", "hsc"]]
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
