-- | The non-BUCK-file-generation setup steps from buck2\/README.md's "How
-- to use it": checking for a @buck2\/@ checkout, creating @.buckconfig@\/
-- @PACKAGE@ if they don't exist yet, and running
-- @buck2\/gen-haskell-prebuilt.py@ to turn the resolved dependency closure
-- into @third-party\/haskell@.
module Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  , runGenHaskellPrebuilt
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Distribution.Simple.Utils (dieWithException, notice, rawSystemExit)

import Distribution.Client.Errors (CabalInstallException (Buck2NoPrelude))

-- | Check that @buck2\/@ (the checkout of
-- <https://github.com/simonmar/haskell-buck2>) exists, dying with
-- instructions to clone it if it doesn't - everything downstream of this
-- (@.buckconfig@\/@PACKAGE@'s own @load()@s, @gen-haskell-prebuilt.py@)
-- depends on it being there.
checkBuck2Prelude :: Verbosity -> FilePath -> IO ()
checkBuck2Prelude verbosity projectRoot = do
  exists <- doesDirectoryExist (projectRoot </> "buck2")
  unless exists $ dieWithException verbosity Buck2NoPrelude

ensureBuckconfigAndPackage :: Verbosity -> FilePath -> IO ()
ensureBuckconfigAndPackage verbosity projectRoot = do
  writeIfMissing verbosity (projectRoot </> ".buckconfig") buckconfigContents
  writeIfMissing verbosity (projectRoot </> "PACKAGE") packageContents

writeIfMissing :: Verbosity -> FilePath -> String -> IO ()
writeIfMissing verbosity path contents = do
  exists <- doesFileExist path
  unless exists $ do
    writeFile path contents
    notice verbosity $ "cabal buck2: created " ++ path

runGenHaskellPrebuilt :: Verbosity -> FilePath -> IO ()
runGenHaskellPrebuilt verbosity projectRoot = do
  notice verbosity "cabal buck2: running buck2/gen-haskell-prebuilt.py"
  rawSystemExit verbosity Nothing "python3" [projectRoot </> "buck2" </> "gen-haskell-prebuilt.py"]

-- | Matches buck2\/example\/.buckconfig, the version of this config that's
-- actually exercised by buck2\/'s own CI (buck2\/README.md's copy differs
-- in its @execution_platforms@ line).
buckconfigContents :: String
buckconfigContents =
  unlines
    [ "[cells]"
    , "  root = ."
    , "  prelude = buck2/prelude"
    , "  toolchains = buck2/toolchains"
    , "  third-party-haskell = third-party/haskell"
    , "  none = none"
    , ""
    , "[cell_aliases]"
    , "  config = prelude"
    , "  ovr_config = prelude"
    , "  fbcode = none"
    , "  fbsource = none"
    , "  buck = none"
    , ""
    , "[parser]"
    , "  target_platform_detector_spec = target:root//...->prelude//platforms:default \\"
    , "    target:prelude//...->prelude//platforms:default \\"
    , "    target:toolchains//...->prelude//platforms:default \\"
    , "    target:third-party-haskell//...->prelude//platforms:default"
    , ""
    , "[build]"
    , "  execution_platforms = root//buck2/platforms:exec"
    ]

packageContents :: String
packageContents =
  unlines
    [ "load(\"//buck2:cfg_constructor_if_standalone.bzl\", \"cfg_constructor_if_standalone\", \"dev_modifiers_if_standalone\")"
    , "load(\"@prelude//cfg/modifier/set_cfg_modifiers.bzl\", \"set_cfg_modifiers\")"
    , ""
    , "cfg_constructor_if_standalone()"
    , ""
    , "set_cfg_modifiers("
    , "    cfg_modifiers = dev_modifiers_if_standalone(),"
    , ")"
    ]
