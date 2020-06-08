{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Model where

import Startlude
import           Database.Persist.TH
import           Lib.Types.Semver


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
App
    createdAt UTCTime
    updatedAt UTCTime Maybe
    title Text
    appId Text
    descShort Text
    descLong Text
    semver AppVersion
    releaseNotes Text
    iconType Text
    UniqueSemver semver
    deriving Eq
    deriving Show

Metric
    createdAt UTCTime
    appId AppId Maybe default=null
    event Text
    deriving Eq
    deriving Show
|]
