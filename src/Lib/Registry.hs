{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Lib.Registry where

import           Startlude

import           Data.HashMap.Lazy hiding (mapMaybe)
import qualified GHC.Read          (Read (..))
import qualified GHC.Show          (Show (..))
import           System.Directory
import           System.FilePath
import           Yesod.Core

import           Data.Text         (isSuffixOf)

import           Constants
import           Lib.Semver
import           Lib.Types.Semver

appResourceDir :: FilePath
appResourceDir = resourcesDir </> "apps"

sysResourceDir :: FilePath
sysResourceDir = resourcesDir </> "sys"

iconsResourceDir :: FilePath
iconsResourceDir = resourcesDir </> "icons"

appManifestPath :: FilePath
appManifestPath = appResourceDir </> appManifestFile

appManifestFile :: FilePath
appManifestFile = "apps.yaml"

type Registry = HashMap String (HashMap AppVersion FilePath)

newtype RegisteredAppVersion = RegisteredAppVersion (AppVersion, FilePath) deriving (Eq, Show)
instance HasAppVersion RegisteredAppVersion where
    version (RegisteredAppVersion (av, _)) = av

getAvailableAppVersions :: KnownSymbol a => FilePath -> Extension a -> IO [RegisteredAppVersion]
getAvailableAppVersions rootDirectory ext@(Extension appId) = do
    versions <- mapMaybe readMaybe <$> getSubDirectories (rootDirectory </> appId)
    fmap catMaybes . for versions $ \v ->
        getVersionedFileFromDir rootDirectory ext v
            >>= \case
                Nothing      -> pure Nothing
                Just appFile -> pure . Just $ RegisteredAppVersion (v, appFile)
    where
        getSubDirectories path = (fmap (fromRight []) . try @SomeException $ listDirectory path) >>= filterM (doesDirectoryExist . (path </>))

-- /root/appId/version/appId.ext
getVersionedFileFromDir :: KnownSymbol a => FilePath -> Extension a -> AppVersion -> IO (Maybe FilePath)
getVersionedFileFromDir rootDirectory ext@(Extension appId) v = getUnversionedFileFromDir (rootDirectory </> appId </> show v) ext

-- /root/appId.ext
getUnversionedFileFromDir :: KnownSymbol a => FilePath -> Extension a -> IO (Maybe FilePath)
getUnversionedFileFromDir rootDirectory appExt = fmap (join . hush) . try @SomeException $ do
    dirContents <- listDirectory rootDirectory
    pure . fmap (rootDirectory </>) $ find (== show appExt) dirContents

newtype Extension (a :: Symbol) = Extension String deriving (Eq)
type S9PK = Extension "s9pk"
type SYS_EXTENSIONLESS = Extension ""
type PNG = Extension "png"

instance IsString (Extension a) where
    fromString = Extension

def :: Extension a
def = Extension ""

extension :: KnownSymbol a => Extension a -> String
extension = symbolVal

instance KnownSymbol a => Show (Extension a) where
    show e@(Extension file) = file <.> extension e

instance KnownSymbol a => Read (Extension a) where
    readsPrec _ s = [(Extension . take (m - n - 1) $ s, "") | toS ext' `isSuffixOf` toS s]
        where
            m = length s
            ext' = extension (def :: Extension a)
            n = if length ext' == 0
                    then -1
                    else length ext'

instance KnownSymbol a => PathPiece (Extension a) where
    fromPathPiece = readMaybe . toS
    toPathPiece = show
