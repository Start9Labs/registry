{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Model where

import           Crypto.Hash
import           Database.Persist.TH
import           Lib.Types.AppIndex
import           Lib.Types.Emver
import           Orphans.Cryptonite             ( )
import           Orphans.Emver                  ( )
import           Startlude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PkgRecord
    Id PkgId sql=pkg_id
    createdAt UTCTime
    updatedAt UTCTime Maybe
    title Text
    descShort Text
    descLong Text
    iconType Text
    deriving Eq
    deriving Show

VersionRecord sql=version
    createdAt UTCTime
    updatedAt UTCTime Maybe
    pkgId PkgRecordId
    number Version
    releaseNotes Text
    osVersion Version
    arch Text Maybe
    Primary pkgId number
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
    pkgId PkgRecordId
    version Version
    deriving Eq
    deriving Show

Category
    createdAt UTCTime
    name Text
    parent CategoryId Maybe
    description Text
    priority Int default=0
    UniqueName name
    deriving Eq
    deriving Show

PkgCategory
    createdAt UTCTime
    pkgId PkgRecordId
    categoryId CategoryId
    deriving Eq
    deriving Show

EosHash
    version Version
    hash Text
    UniqueVersion version

ErrorLogRecord
    createdAt UTCTime
    epoch Text
    commitHash Text
    sourceFile Text
    line Word32
    target Text
    level Text
    message Text
    incidents Word32
    UniqueLogRecord epoch commitHash sourceFile line target level message

PkgDependency
    createdAt UTCTime
    pkgId PkgRecordId
    pkgVersion Version
    depId PkgRecordId
    depVersionRange VersionRange
    UniquePkgDepVersion pkgId pkgVersion depId
    deriving Eq
    deriving Show

Admin
    createdAt UTCTime
    name Text
    passHash (Digest SHA256)
    UniqueAdmin name

Upload
    uploader AdminId
    pkgId PkgRecordId
    pkgVersion Version
    createdAt UTCTime
|]
