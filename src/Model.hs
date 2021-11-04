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

import           Startlude
import           Database.Persist.TH
import           Lib.Types.Emver
import           Orphans.Emver                  ( )


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

SVersion sql=version
    createdAt UTCTime
    updatedAt UTCTime Maybe
    appId SAppId
    number Version
    releaseNotes Text
    osVersionRequired VersionRange default='*'
    osVersionRecommended VersionRange default='*'
    UniqueBin appId number
    deriving Eq
    deriving Show


Metric
    createdAt UTCTime
    appId SAppId
    version SVersionId
    deriving Eq
    deriving Show
|]
