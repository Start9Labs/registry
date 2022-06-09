{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Conduit where

import Conduit (ConduitT, awaitForever, yield)
import Control.Monad.Logger (logInfo)
import Control.Monad.Logger.CallStack (MonadLogger)
import Data.List (lookup, null)
import Data.String.Interpolate.IsString (i)
import Database.Marketplace (PackageDependencyMetadata (..), PackageMetadata (..))
import Database.Persist (Entity (..))
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (Version, VersionRange (..), satisfies)
import Model (Category, Key, PkgDependency (..), PkgRecord (PkgRecord), VersionRecord (..))
import Startlude (Bool, Down (..), Maybe (..), Monad, Text, filter, fmap, fromMaybe, headMay, sortOn, unless, ($), (.))


filterPkgOsCompatible :: Monad m => (Version -> Bool) -> ConduitT PackageMetadata PackageMetadata m ()
filterPkgOsCompatible p =
    awaitForever $
        \PackageMetadata{packageMetadataPkgId = pkg, packageMetadataPkgVersionRecords = versions, packageMetadataPkgCategories = cats, packageMetadataPkgVersion = requestedVersion} ->
            do
                let compatible = filter (p . versionRecordOsVersion . entityVal) versions
                unless (null compatible) $
                    yield
                        PackageMetadata
                            { packageMetadataPkgId = pkg
                            , packageMetadataPkgVersionRecords = compatible
                            , packageMetadataPkgCategories = cats
                            , packageMetadataPkgVersion = requestedVersion
                            }


filterLatestVersionFromSpec ::
    (Monad m, MonadLogger m) =>
    [(PkgId, VersionRange)] ->
    ConduitT (PkgId, [Entity VersionRecord], [Entity Category]) PackageMetadata m ()
filterLatestVersionFromSpec versionMap = awaitForever $ \(pkgId, vs, cats) -> do
    -- if no packages are specified, the VersionRange is implicitly `*`
    let spec = fromMaybe Any $ lookup pkgId versionMap
    case headMay . sortOn Down $ filter (`satisfies` spec) $ fmap (versionRecordNumber . entityVal) vs of
        Nothing -> $logInfo [i|No version for #{pkgId} satisfying #{spec}|]
        Just v ->
            yield $
                PackageMetadata
                    { packageMetadataPkgId = pkgId
                    , packageMetadataPkgVersionRecords = vs
                    , packageMetadataPkgCategories = cats
                    , packageMetadataPkgVersion = v
                    }


filterDependencyOsCompatible :: (Version -> Bool) -> PackageDependencyMetadata -> PackageDependencyMetadata
filterDependencyOsCompatible p PackageDependencyMetadata{packageDependencyMetadataPkgDependencyRecord = pkgDeps, packageDependencyMetadataDepPkgRecord = pkg, packageDependencyMetadataDepVersions = depVersions} =
    do
        let compatible = filter (p . versionRecordOsVersion . entityVal) depVersions
        PackageDependencyMetadata
            { packageDependencyMetadataPkgDependencyRecord = pkgDeps
            , packageDependencyMetadataDepPkgRecord = pkg
            , packageDependencyMetadataDepVersions = compatible
            }


-- get best version of the dependency based on what is specified in the db (ie. what is specified in the manifest for the package)
filterDependencyBestVersion :: MonadLogger m => PackageDependencyMetadata -> m (Maybe (Key PkgRecord, Text, Version))
filterDependencyBestVersion PackageDependencyMetadata{packageDependencyMetadataPkgDependencyRecord = pkgDepRecord, packageDependencyMetadataDepVersions = depVersions} =
    do
        -- get best version from VersionRange of dependency
        let pkgId = pkgDependencyPkgId $ entityVal pkgDepRecord
        let depId = pkgDependencyDepId $ entityVal pkgDepRecord
        let versionRequirement = pkgDependencyDepVersionRange $ entityVal pkgDepRecord
        let satisfactory = filter ((<|| versionRequirement) . versionRecordNumber) (entityVal <$> depVersions)
        case maximumOn versionRecordNumber satisfactory of
            Just bestVersion -> pure $ Just (depId, versionRecordTitle bestVersion, versionRecordNumber bestVersion)
            Nothing -> do
                $logInfo
                    [i|No satisfactory version of #{depId} for dependent package #{pkgId}, needs #{versionRequirement}|]
                pure Nothing
