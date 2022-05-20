{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Util.Shared where

import           Startlude               hiding ( Any
                                                , Handler
                                                , yield
                                                )

import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Conduit                        ( ConduitT
                                                , awaitForever
                                                , yield
                                                )
import           Control.Monad.Reader.Has       ( Has )
import           Data.Semigroup                 ( Max(Max)
                                                , getMax
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Esqueleto.Experimental
                                                ( Entity
                                                , Key
                                                , entityKey
                                                , entityVal
                                                )
import           Foundation
import           GHC.List                       ( lookup )
import           Handler.Types.Marketplace      ( PackageDependencyMetadata(..)
                                                , PackageMetadata(..)
                                                )
import           Lib.PkgRepository              ( PkgRepo
                                                , getHash
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Emver
import           Model                          ( Category
                                                , Key(unPkgRecordKey)
                                                , PkgDependency(pkgDependencyDepId, pkgDependencyDepVersionRange)
                                                , PkgRecord(pkgRecordTitle)
                                                , VersionRecord(versionRecordNumber, versionRecordOsVersion)
                                                , pkgDependencyPkgId
                                                )

getVersionSpecFromQuery :: Handler VersionRange
getVersionSpecFromQuery = do
    specString <- T.filter (not . isSpace) . fromMaybe "*" <$> lookupGetParam "spec"
    case readMaybe specString of
        Nothing -> sendResponseStatus status400 ("Invalid App Version Specification" :: Text)
        Just t  -> pure t

versionPriorityFromQueryIsMin :: Handler Bool
versionPriorityFromQueryIsMin = do
    priorityString <- lookupGetParam "version-priority"
    case priorityString of
        Nothing      -> pure False
        (Just "max") -> pure False
        (Just "min") -> pure True
        (Just t    ) -> sendResponseStatus status400 ("Invalid Version Priority Specification: " <> t)

addPackageHeader :: (MonadUnliftIO m, MonadHandler m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ()
addPackageHeader pkg version = do
    packageHash <- getHash pkg version
    addHeader "X-S9PK-HASH" $ decodeUtf8 packageHash

orThrow :: MonadHandler m => m (Maybe a) -> m a -> m a
orThrow action other = action >>= \case
    Nothing -> other
    Just x  -> pure x

filterPkgOsCompatible :: Monad m => (Version -> Bool) -> ConduitT PackageMetadata PackageMetadata m ()
filterPkgOsCompatible p =
    awaitForever
        $ \PackageMetadata { packageMetadataPkgRecord = pkg, packageMetadataPkgVersionRecords = versions, packageMetadataPkgCategories = cats, packageMetadataPkgVersion = requestedVersion } ->
              do
                  let compatible = filter (p . versionRecordOsVersion . entityVal) versions
                  unless (null compatible) $ yield PackageMetadata { packageMetadataPkgRecord         = pkg
                                                                   , packageMetadataPkgVersionRecords = compatible
                                                                   , packageMetadataPkgCategories     = cats
                                                                   , packageMetadataPkgVersion        = requestedVersion
                                                                   }

filterDependencyOsCompatible :: (Version -> Bool) -> PackageDependencyMetadata -> PackageDependencyMetadata
filterDependencyOsCompatible p PackageDependencyMetadata { packageDependencyMetadataPkgDependencyRecord = pkgDeps, packageDependencyMetadataDepPkgRecord = pkg, packageDependencyMetadataDepVersions = depVersions }
    = do
        let compatible = filter (p . versionRecordOsVersion . entityVal) depVersions
        PackageDependencyMetadata { packageDependencyMetadataPkgDependencyRecord = pkgDeps
                                  , packageDependencyMetadataDepPkgRecord        = pkg
                                  , packageDependencyMetadataDepVersions         = compatible
                                  }

filterLatestVersionFromSpec :: (Monad m, MonadLogger m)
                            => [(PkgId, VersionRange)]
                            -> ConduitT
                                   (Entity PkgRecord, [Entity VersionRecord], [Entity Category])
                                   PackageMetadata
                                   m
                                   ()
filterLatestVersionFromSpec versionMap = awaitForever $ \(a, vs, cats) -> do
    let pkgId = entityKey a
    -- if no packages are specified, the VersionRange is implicitly `*`
    let spec = fromMaybe Any $ lookup (unPkgRecordKey $ entityKey a) versionMap
    case headMay . sortOn Down $ filter (`satisfies` spec) $ fmap (versionRecordNumber . entityVal) vs of
        Nothing -> $logInfo [i|No version for #{pkgId} satisfying #{spec}|]
        Just v  -> yield $ PackageMetadata { packageMetadataPkgRecord         = a
                                           , packageMetadataPkgVersionRecords = vs
                                           , packageMetadataPkgCategories     = cats
                                           , packageMetadataPkgVersion        = v
                                           }

-- get best version of the dependency based on what is specified in the db (ie. what is specified in the manifest for the package)
filterDependencyBestVersion :: MonadLogger m => PackageDependencyMetadata -> m (Maybe (Key PkgRecord, Text, Version))
filterDependencyBestVersion PackageDependencyMetadata { packageDependencyMetadataPkgDependencyRecord = pkgDepRecord, packageDependencyMetadataDepPkgRecord = depRecord, packageDependencyMetadataDepVersions = depVersions }
    = do
        -- get best version from VersionRange of dependency
        let pkgId    = pkgDependencyPkgId $ entityVal pkgDepRecord
        let depId    = pkgDependencyDepId $ entityVal pkgDepRecord
        let depTitle = pkgRecordTitle $ entityVal depRecord
        let satisfactory = filter (<|| (pkgDependencyDepVersionRange $ entityVal pkgDepRecord))
                                  (versionRecordNumber . entityVal <$> depVersions)
        case getMax <$> foldMap (Just . Max) satisfactory of
            -- QUESTION is this an acceptable transformation here? These are the only values that we care about after this filter.
            Just bestVersion -> pure $ Just (depId, depTitle, bestVersion)
            Nothing          -> do
                -- TODO it would be better if we could return the requirements for display
                $logInfo [i|No satisfactory version of #{depId} for dependent package #{pkgId}|]
                pure Nothing

sendResponseText :: MonadHandler m => Status -> Text -> m a
sendResponseText = sendResponseStatus @_ @Text
