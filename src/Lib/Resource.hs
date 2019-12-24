{-# LANGUAGE TupleSections #-}

module Lib.Resource where

import           Startlude         hiding (empty, toList)

import           Data.HashMap.Lazy hiding (mapMaybe)
import           System.Directory
import           System.FilePath

import           Lib.Semver
import           Lib.Types.Semver


resourcePath :: FilePath
resourcePath = "./resources"

manifestPath :: FilePath
manifestPath = resourcePath </> "apps.yml"

manifestFile :: FilePath
manifestFile = "apps.yml"

s9pkFile :: String -> FilePath
s9pkFile appId = toS appId <.> "s9pk"

type Registry = HashMap String (HashMap AppVersion FilePath)

newtype RegisteredAppVersion = RegisteredAppVersion (AppVersion, FilePath)
instance HasAppVersion RegisteredAppVersion where
    version (RegisteredAppVersion (av, _)) = av

loadResources :: MonadIO m => m Registry
loadResources = liftIO $ do
    appDirectories <- getSubDirectories resourcePath
    foldM
        ( \hm appId -> do
            subdirs <- getSubDirectories (resourcePath </> appId)
            let validVersions = mapMaybe readMaybe subdirs
            let newAppVersions = fromList $ fmap (\v -> (v, resourcePath </> show v </> s9pkFile appId)) validVersions
            pure $ insert appId newAppVersions hm
        ) empty appDirectories
    where
        getSubDirectories path = listDirectory path >>= filterM (fmap not . doesFileExist)

getAppFile :: Registry -> String -> AppVersion -> Maybe FilePath
getAppFile r appId av = lookup av <=< lookup appId $ r

registeredAppVersions :: Registry -> String -> [RegisteredAppVersion]
registeredAppVersions r appId = fromMaybe [] $ fmap RegisteredAppVersion . toList <$> lookup appId r
