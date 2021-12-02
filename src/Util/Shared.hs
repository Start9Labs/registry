{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Util.Shared where

import           Startlude               hiding ( Handler
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
import           Lib.PkgRepository              ( PkgRepo
                                                , getHash
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Emver
import           Model                          ( Category
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

addPackageHeader :: (MonadUnliftIO m, MonadHandler m, MonadReader r m, Has PkgRepo r) => PkgId -> Version -> m ()
addPackageHeader pkg version = do
    packageHash <- getHash pkg version
    addHeader "X-S9PK-HASH" $ decodeUtf8 packageHash

orThrow :: MonadHandler m => m (Maybe a) -> m a -> m a
orThrow action other = action >>= \case
    Nothing -> other
    Just x  -> pure x


filterPkgOsCompatible :: Monad m
                      => (Version -> Bool)
                      -> ConduitT
                             (Entity PkgRecord, [Entity VersionRecord], [Entity Category], Version)
                             (Entity PkgRecord, [Entity VersionRecord], [Entity Category], Version)
                             m
                             ()
filterPkgOsCompatible p = awaitForever $ \(app, versions, cats, requestedVersion) -> do
    let compatible = filter (p . versionRecordOsVersion . entityVal) versions
    when (not $ null compatible) $ yield (app, compatible, cats, requestedVersion)

filterDependencyOsCompatible :: (Version -> Bool)
                             -> (Entity PkgDependency, Entity PkgRecord, [Entity VersionRecord])
                             -> (Entity PkgDependency, Entity PkgRecord, [Entity VersionRecord])
filterDependencyOsCompatible p (pkgDeps, pkg, versions) = do
    let compatible = filter (p . versionRecordOsVersion . entityVal) versions
    (pkgDeps, pkg, compatible)

filterLatestVersionFromSpec :: (Monad m, MonadLogger m)
                            => ConduitT
                                   (Entity PkgRecord, [Entity VersionRecord], [Entity Category], VersionRange)
                                   (Entity PkgRecord, [Entity VersionRecord], [Entity Category], Version)
                                   m
                                   ()
filterLatestVersionFromSpec = awaitForever $ \(a, vs, cats, spec) -> do
    let pkgId = entityKey a
    case headMay . sortOn Down $ filter (`satisfies` spec) $ fmap (versionRecordNumber . entityVal) vs of
        Nothing -> $logInfo [i|No version for #{pkgId} satisfying #{spec}|]
        Just v  -> yield $ (,,,) a vs cats v

-- get best version of the dependency based on what is specified in the db (ie. what is specified in the manifest for the package)
filterDependencyBestVersion :: MonadLogger m
                            => (Entity PkgDependency, Entity PkgRecord, [Entity VersionRecord])
                            -> m (Maybe (Key PkgRecord, Text, Version))
filterDependencyBestVersion (pkgDepRecord, depPkgRecord, depVersions) = do
    -- get best version from VersionRange of dependency
    let pkgId    = pkgDependencyPkgId $ entityVal pkgDepRecord
    let depId    = pkgDependencyDepId $ entityVal pkgDepRecord
    let depTitle = pkgRecordTitle $ entityVal depPkgRecord
    let satisfactory = filter (<|| (pkgDependencyDepVersionRange $ entityVal pkgDepRecord))
                              (versionRecordNumber . entityVal <$> depVersions)
    case getMax <$> foldMap (Just . Max) satisfactory of
        -- QUESTION is this an acceptable transformation here? These are the only values that we care about after this filter.
        Just bestVersion -> pure $ Just (depId, depTitle, bestVersion)
        Nothing          -> do
            $logInfo [i|No satisfactory version of #{depId} for dependent package #{pkgId}|]
    -- TODO it would be better if we could return the requirements for display
            pure Nothing
