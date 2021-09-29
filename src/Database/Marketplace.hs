{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Marketplace where

import           Startlude               hiding ( (%)
                                                , from
                                                , on
                                                )
import           Database.Esqueleto.Experimental
import           Lib.Types.Category
import           Model
import qualified Database.Persist              as P
import           Data.HashMap.Strict
import           Data.Version
import           Data.Aeson

searchServices :: MonadIO m => Maybe CategoryTitle -> Int64 -> Int64 -> Text -> ReaderT SqlBackend m [P.Entity SApp]
searchServices Nothing pageItems offset' query = select $ do
    service <- from $ table @SApp
    where_
        (   (service ^. SAppDescShort `ilike` (%) ++. val query ++. (%))
        ||. (service ^. SAppDescLong `ilike` (%) ++. val query ++. (%))
        ||. (service ^. SAppTitle `ilike` (%) ++. val query ++. (%))
        )
    orderBy [desc (service ^. SAppUpdatedAt)]
    limit pageItems
    offset offset'
    pure service
searchServices (Just category) pageItems offset' query = select $ do
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
    limit pageItems
    offset offset'
    pure services

newtype VersionsWithReleaseNotes = VersionsWithReleaseNotes (HashMap Version Text) deriving (Eq, Show, Generic)
instance FromJSON VersionsWithReleaseNotes
instance PersistField VersionsWithReleaseNotes where
    fromPersistValue = fromPersistValueJSON
    toPersistValue   = PersistText . show

-- in progress attempt to do postgres aggregation with raw sql in esqueleto
-- getServiceVersionsWithReleaseNotes :: MonadIO m => Text -> ReaderT SqlBackend m (Entity SApp)
-- getServiceVersionsWithReleaseNotes appId = rawSql "SELECT ??, json_agg(json_build_object(v.number, v.release_notes)) as versions FROM s_app s LEFT JOIN version v ON v.app_id = s.id WHERE s.app_id = ? GROUP BY s.id;" [PersistText appId]
