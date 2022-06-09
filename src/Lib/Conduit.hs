{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Conduit where

import Control.Monad.Logger (logInfo)
import Control.Monad.Logger.CallStack (MonadLogger)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate.IsString (i)
import Database.Marketplace (PackageDependencyMetadata (..))
import Database.Persist (Entity (..))
import Lib.Ord (maximumOn)
import Lib.Types.AppIndex (PkgId)
import Lib.Types.Emver (Version, VersionRange (..), satisfies, (<||))
import Model (Key (..), PkgDependency (..), PkgRecord (..), VersionRecord (..))
import Startlude (Bool, Down (..), Maybe (..), NonEmpty, Text, filter, headMay, pure, sortOn, ($), (.), (<$>))


selectLatestVersionFromSpec ::
    (PkgId -> VersionRange) ->
    NonEmpty VersionRecord ->
    Maybe VersionRecord
selectLatestVersionFromSpec pkgRanges vs =
    let pkgId = NE.head $ versionRecordPkgId <$> vs
        spec = pkgRanges (unPkgRecordKey pkgId)
     in headMay . sortOn (Down . versionRecordNumber) $ NE.filter ((`satisfies` spec) . versionRecordNumber) vs


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
