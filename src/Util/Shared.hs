{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Util.Shared where


import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Conduit                        ( ConduitT
                                                , awaitForever
                                                , yield
                                                )
import           Control.Monad.Reader.Has       ( Has
                                                , MonadReader
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Esqueleto.Experimental
                                                ( Entity
                                                , Key
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
                                                , PkgDependency(pkgDependencyDepId, pkgDependencyDepVersionRange)
                                                , PkgRecord
                                                , VersionRecord(..)
                                                , pkgDependencyPkgId
                                                )
import           Startlude                      ( ($)
                                                , (.)
                                                , (<$>)
                                                , Alternative((<|>))
                                                , Applicative(pure)
                                                , Bool(..)
                                                , Down(Down)
                                                , Foldable(foldr, null)
                                                , Functor(fmap)
                                                , Maybe(..)
                                                , Monad((>>=))
                                                , Ord((>))
                                                , Text
                                                , decodeUtf8
                                                , filter
                                                , fromMaybe
                                                , headMay
                                                , isSpace
                                                , not
                                                , readMaybe
                                                , sortOn
                                                , unless
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
        $ \PackageMetadata { packageMetadataPkgId = pkg, packageMetadataPkgVersionRecords = versions, packageMetadataPkgCategories = cats, packageMetadataPkgVersion = requestedVersion } ->
              do
                  let compatible = filter (p . versionRecordOsVersion . entityVal) versions
                  unless (null compatible) $ yield PackageMetadata { packageMetadataPkgId             = pkg
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
                            -> ConduitT (PkgId, [Entity VersionRecord], [Entity Category]) PackageMetadata m ()
filterLatestVersionFromSpec versionMap = awaitForever $ \(pkgId, vs, cats) -> do
    -- if no packages are specified, the VersionRange is implicitly `*`
    let spec = fromMaybe Any $ lookup pkgId versionMap
    case headMay . sortOn Down $ filter (`satisfies` spec) $ fmap (versionRecordNumber . entityVal) vs of
        Nothing -> $logInfo [i|No version for #{pkgId} satisfying #{spec}|]
        Just v  -> yield $ PackageMetadata { packageMetadataPkgId             = pkgId
                                           , packageMetadataPkgVersionRecords = vs
                                           , packageMetadataPkgCategories     = cats
                                           , packageMetadataPkgVersion        = v
                                           }

-- get best version of the dependency based on what is specified in the db (ie. what is specified in the manifest for the package)
filterDependencyBestVersion :: MonadLogger m => PackageDependencyMetadata -> m (Maybe (Key PkgRecord, Text, Version))
filterDependencyBestVersion PackageDependencyMetadata { packageDependencyMetadataPkgDependencyRecord = pkgDepRecord, packageDependencyMetadataDepVersions = depVersions }
    = do
        -- get best version from VersionRange of dependency
        let pkgId              = pkgDependencyPkgId $ entityVal pkgDepRecord
        let depId              = pkgDependencyDepId $ entityVal pkgDepRecord
        let versionRequirement = pkgDependencyDepVersionRange $ entityVal pkgDepRecord
        let satisfactory = filter ((<|| versionRequirement) . versionRecordNumber) (entityVal <$> depVersions)
        case maximumOn versionRecordNumber satisfactory of
            Just bestVersion -> pure $ Just (depId, versionRecordTitle bestVersion, versionRecordNumber bestVersion)
            Nothing          -> do
                $logInfo
                    [i|No satisfactory version of #{depId} for dependent package #{pkgId}, needs #{versionRequirement}|]
                pure Nothing

sendResponseText :: MonadHandler m => Status -> Text -> m a
sendResponseText s = sendResponseStatus s . TypedContent typePlain . toContent

maximumOn :: forall a b t . (Ord b, Foldable t) => (a -> b) -> t a -> Maybe a
maximumOn f = foldr (\x y -> maxOn f x <$> y <|> Just x) Nothing

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x > f y then x else y
