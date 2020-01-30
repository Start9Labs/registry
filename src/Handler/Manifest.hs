{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Manifest where

import           Startlude

import           Control.Lens.Combinators
import qualified Data.HashMap.Strict      as HM
import           Data.Yaml                as Yaml
import           Network.HTTP.Types
import           System.FilePath
import           Yesod.Core

import           Foundation
import           Lib.Registry
import           Lib.Semver
import           Lib.Types.Semver

data VersionInfo = VersionInfo
    { versionInfoVersion           :: AppVersion
    , versionInfoMeshOsRequirement :: AppVersionSpecification
    , versionInfoReleaseNotes      :: Text
    } deriving (Eq, Show)
makeClassy_ ''VersionInfo
instance ToJSON VersionInfo where
    toJSON VersionInfo{..} = object
        [ "version" .= versionInfoVersion
        , "meshos-version-requirement" .= versionInfoMeshOsRequirement
        , "release-notes" .= versionInfoReleaseNotes
        ]
instance FromJSON VersionInfo where
    parseJSON = withObject "Version Info" $ \o -> do
        versionInfoVersion           <- o .: "version"
        versionInfoMeshOsRequirement <- o .:? "meshos-version-requirement" .!= "=0.1.0"
        versionInfoReleaseNotes      <- o .: "release-notes"
        pure VersionInfo{..}

data AppSpec = AppSpec
    { appSpecTitle            :: Text
    , appSpecDescriptionShort :: Text
    , appSpecDescriptionLong  :: Text
    , appSpecVersionInfo      :: [VersionInfo]
    , appSpecIconType         :: Text
    } deriving (Eq, Show)
makeClassy_ ''AppSpec
instance ToJSON AppSpec where
    toJSON AppSpec{..} = object
        [ "title" .= appSpecTitle
        , "description" .= object
            [ "short" .= appSpecDescriptionShort
            , "long" .= appSpecDescriptionLong
            ]
        , "version-info" .= appSpecVersionInfo
        , "icon-type" .= appSpecIconType
        ]
instance FromJSON AppSpec where
    parseJSON = withObject "App Spec" $ \o -> do
        appSpecTitle <- o .: "title"
        appSpecDescriptionShort <- o .: "description" >>= (.: "short")
        appSpecDescriptionLong <- o .: "description" >>= (.: "long")
        appSpecVersionInfo <- o .: "version-info"
        appSpecIconType <- o .: "icon-type"
        pure AppSpec{..}

newtype AppManifest = AppManifest { unAppManifest :: HM.HashMap Text AppSpec } deriving (Eq, Show)
instance ToJSON AppManifest where
    toJSON AppManifest{unAppManifest} = object $ uncurry (.=) <$> HM.toList unAppManifest
instance FromJSON AppManifest where
    parseJSON = withObject "App Manifest" $ \o -> AppManifest <$> traverse parseJSON o
instance ToTypedContent AppManifest where
    toTypedContent man = TypedContent "application/x-yaml" (toContent man)
instance ToContent AppManifest where
    toContent man = toContent $ Yaml.encode man

appManifestPath :: FilePath
appManifestPath = appResourceDir </> appManifestFile

appManifestFile :: FilePath
appManifestFile = "apps.yaml"

getAppsManifestR :: Handler AppManifest
getAppsManifestR = do
    manifest <- liftIO (Yaml.decodeFileEither appManifestPath) >>= \case
        Left e -> do
            -- TODO: message founders via telegram
            $logError $ "INVALID App Manifest! Resolve immediately!!!"
            $logError $ show e
            sendResponseStatus status500 ()
        Right a -> pure a
    v <- lookupHeaders "User-Agent" `orDefaultTo` AppVersion (0,1,0,0)
    pure $ compressManifest v manifest
    -- respondSource typePlain $ CB.sourceFile appManifestPath .| awaitForever sendChunkBS
    where
        orDefaultTo :: (Monad m, Read a) => m [ByteString] -> a -> m a
        orDefaultTo m a = do
            m >>= \case
                [] -> pure a
                (h:_) -> case readMaybe (toS h) of
                    Nothing -> pure a
                    Just x -> pure x

compressManifest :: AppVersion -> AppManifest -> AppManifest
compressManifest v = filterApps . filterVersions v

filterVersions :: AppVersion -> AppManifest -> AppManifest
filterVersions v = AppManifest . fmap (over _appSpecVersionInfo $ filter ((v <||) . versionInfoMeshOsRequirement)) . unAppManifest

filterApps :: AppManifest -> AppManifest
filterApps = AppManifest . HM.filter (not . null . appSpecVersionInfo) . unAppManifest
