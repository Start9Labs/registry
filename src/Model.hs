{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds  #-}

module Model where

import           Database.Persist.TH
import           Lib.Types.AppIndex
import           Lib.Types.Category
import           Lib.Types.Emver
import           Orphans.Emver                  ( )
import           Startlude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SApp
    createdAt UTCTime
    updatedAt UTCTime Maybe
    title Text
    appId PkgId
    descShort Text
    descLong Text
    iconType Text
    UniqueAppId appId
    deriving Eq
    deriving Show

SVersion sql=version
    createdAt UTCTime
    updatedAt UTCTime Maybe
    appId SAppId
    number Version
    releaseNotes Text
    osVersionRequired VersionRange default='*'
    osVersionRecommended VersionRange default='*'
    arch Text Maybe
    UniqueBin appId number
    deriving Eq
    deriving Show

OsVersion
    createdAt UTCTime
    updatedAt UTCTime
    number Version
    headline Text
    releaseNotes Text
    deriving Eq
    deriving Show

Metric
    createdAt UTCTime
    appId SAppId
    version SVersionId
    deriving Eq
    deriving Show

Category
    createdAt UTCTime
    name CategoryTitle
    parent CategoryId Maybe
    description Text
    priority Int default=0
    UniqueName name
    deriving Eq
    deriving Show

ServiceCategory
    createdAt UTCTime
    serviceId SAppId
    categoryId CategoryId
    serviceName Text -- SAppAppId
    categoryName CategoryTitle -- CategoryTitle
    priority Int Maybe
    deriving Eq
    deriving Show
|]
