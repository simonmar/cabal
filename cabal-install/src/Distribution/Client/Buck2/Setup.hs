-- | The non-BUCK-file-generation setup steps from buck2\/README.md's "How
-- to use it": checking for a @buck2\/@ checkout, and creating
-- @.buckconfig@\/@PACKAGE@ if they don't exist yet (see
-- "Distribution.Client.Buck2.Prebuilt" for turning the resolved
-- dependency closure into @third-party\/haskell@).
module Distribution.Client.Buck2.Setup
  ( checkBuck2Prelude
  , ensureBuckconfigAndPackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Distribution.Simple.Utils (dieWithException, notice)

import Distribution.Client.Errors (CabalInstallException (Buck2NoPrelude))

-- | Check that @buck2\/@ (the checkout of
-- <https://github.com/simonmar/haskell-buck2>) exists, dying with
-- instructions to clone it if it doesn't - everything downstream of this
-- (@.buckconfig@\/@PACKAGE@'s own @load()@s, third-party generation)
-- depends on it being there.
checkBuck2Prelude :: Verbosity -> FilePath -> IO ()
checkBuck2Prelude verbosity projectRoot = do
  exists <- doesDirectoryExist (projectRoot </> "buck2")
  unless exists $ dieWithException verbosity Buck2NoPrelude

-- | Copies @.buckconfig@\/@PACKAGE@ verbatim from @buck2\/example@ - the
-- version of these two files that's actually exercised by buck2\/'s own
-- CI, rather than a copy hardcoded here that could silently drift from
-- what a newer buck2\/ checkout expects.
ensureBuckconfigAndPackage :: Verbosity -> FilePath -> IO ()
ensureBuckconfigAndPackage verbosity projectRoot = do
  copyIfMissing verbosity (exampleDir </> ".buckconfig") (projectRoot </> ".buckconfig")
  copyIfMissing verbosity (exampleDir </> "PACKAGE") (projectRoot </> "PACKAGE")
  where
    exampleDir = projectRoot </> "buck2" </> "example"

copyIfMissing :: Verbosity -> FilePath -> FilePath -> IO ()
copyIfMissing verbosity src dest = do
  exists <- doesFileExist dest
  unless exists $ do
    contents <- readFile src
    writeFile dest contents
    notice verbosity $ "cabal buck2: created " ++ dest ++ " (from " ++ src ++ ")"
