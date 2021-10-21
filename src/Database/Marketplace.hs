{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Marketplace where

import           Conduit                        ( ConduitT
                                                , MonadResource
                                                , MonadUnliftIO
                                                , awaitForever
                                                , yield
                                                )
import           Data.Aeson
import           Data.HashMap.Strict            ( HashMap )
import           Data.Version
import           Database.Esqueleto.Experimental
                                                ( (%)
                                                , (&&.)
                                                , (++.)
                                                , (==.)
                                                , Entity(entityKey, entityVal)
                                                , PersistField(..)
                                                , PersistValue(..)
                                                , SqlBackend
                                                , (^.)
                                                , desc
                                                , from
                                                , fromPersistValueJSON
                                                , ilike
                                                , in_
                                                , innerJoin
                                                , on
                                                , orderBy
                                                , select
                                                , selectSource
                                                , val
                                                , valList
                                                , where_
                                                , (||.)
                                                )
import           Database.Esqueleto.Experimental
                                                ( (:&)(..)
                                                , table
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Category
import           Lib.Types.Emver                ( VersionRange )
import           Model
import           Startlude               hiding ( (%)
                                                , from
                                                , on
                                                , yield
                                                )

searchServices :: (MonadResource m, MonadIO m)
               => Maybe CategoryTitle
               -> Text
               -> ConduitT () (Entity SApp) (ReaderT SqlBackend m) ()
searchServices Nothing query = selectSource $ do
    service <- from $ table @SApp
    where_
        (   (service ^. SAppDescShort `ilike` (%) ++. val query ++. (%))
        ||. (service ^. SAppDescLong `ilike` (%) ++. val query ++. (%))
        ||. (service ^. SAppTitle `ilike` (%) ++. val query ++. (%))
        )
    orderBy [desc (service ^. SAppUpdatedAt)]
    pure service
searchServices (Just category) query = selectSource $ do
    services <- from
        (do
            (service :& sc) <-
                from
                $           table @SApp
                `innerJoin` table @ServiceCategory
                `on`        (\(s :& sc) -> sc ^. ServiceCategoryServiceId ==. s ^. SAppId)
            -- if there is a cateogry, only search in category
            -- weight title, short, long (bitcoin should equal Bitcoin Core)
            where_
                $   sc
                ^.  ServiceCategoryCategoryName
                ==. val category
                &&. (   (service ^. SAppDescShort `ilike` (%) ++. val query ++. (%))
                    ||. (service ^. SAppDescLong `ilike` (%) ++. val query ++. (%))
                    ||. (service ^. SAppTitle `ilike` (%) ++. val query ++. (%))
                    )
            pure service
        )
    orderBy [desc (services ^. SAppUpdatedAt)]
    pure services

getPkgData :: (MonadResource m, MonadIO m) => [PkgId] -> ConduitT () (Entity SApp) (ReaderT SqlBackend m) ()
getPkgData pkgs = selectSource $ do
    pkgData <- from $ table @SApp
    where_ (pkgData ^. SAppAppId `in_` valList pkgs)
    pure pkgData

zipVersions :: MonadUnliftIO m => ConduitT (Entity SApp) (Entity SApp, [Entity SVersion]) (ReaderT SqlBackend m) ()
zipVersions = awaitForever $ \i -> do
    let appDbId = entityKey i
    res <- lift $ select $ do
        v <- from $ table @SVersion
        where_ $ v ^. SVersionAppId ==. val appDbId
        pure v
    yield (i, res)

filterOsCompatible :: Monad m
                   => (VersionRange -> Bool)
                   -> ConduitT (Entity SApp, [Entity SVersion]) (Entity SApp, [Entity SVersion]) m ()
filterOsCompatible p = awaitForever $ \(app, versions) -> do
    let compatible = filter (p . sVersionOsVersionRecommended . entityVal) versions
    when (not $ null compatible) $ yield (app, compatible)
