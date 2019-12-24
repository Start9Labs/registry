{-# LANGUAGE TupleSections #-}

module Lib.Resource where

import           Startlude         hiding (empty)

import           Data.Aeson
import           Data.HashMap.Lazy hiding (mapMaybe)
import           System.Directory
import           System.FilePath

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

loadResources :: MonadIO m => m Registry
loadResources = liftIO $ do
    appDirectories <- getSubDirectories resourcePath
    foldM
        ( \hm appId -> do
            subdirs <- getSubDirectories (resourcePath </> appId)
            let validVersions = mapMaybe readMaybe subdirs
            let newAppVersions = fromList $ fmap (, s9pkFile appId) validVersions
            pure $ insert appId newAppVersions hm
        ) empty appDirectories
    where
        getSubDirectories path = listDirectory path >>= filterM (fmap not . doesFileExist)

getAppFile :: Registry -> String -> AppVersion -> Maybe FilePath
getAppFile r appId av = do
    s9pk <- lookup av <=< lookup appId $ r
    pure $ resourcePath </> appId </> show av </> s9pk
