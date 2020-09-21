{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Model where

import           Startlude
import           Database.Persist.TH
import           Lib.Types.Semver


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SApp
    createdAt UTCTime
    updatedAt UTCTime Maybe
    title Text
    appId Text
    descShort Text
    descLong Text
    iconType Text
    UniqueAppId appId
    deriving Eq
    deriving Show

Version
    createdAt UTCTime
    updatedAt UTCTime Maybe
    appId SAppId
    number AppVersion
    releaseNotes Text
    osVersionRequired AppVersionSpec default='*'
    osVersionRecommended AppVersionSpec default='*'
    UniqueBin appId number
    deriving Eq
    deriving Show


Metric
    createdAt UTCTime
    appId SAppId
    version VersionId
    deriving Eq
    deriving Show
|]
