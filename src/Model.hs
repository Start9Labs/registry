{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Crypto.Hash (
    Digest,
    SHA256,
 )
import Database.Persist.TH (
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
 )
import Lib.Types.Core (PkgId (PkgId), OsArch)
import Lib.Types.Emver (
    Version,
    VersionRange,
 )
import Orphans.Cryptonite ()
import Orphans.Emver ()
import Startlude (
    Eq,
    Int,
    Show,
    Text,
    UTCTime,
    Word32,
 )


share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
PkgRecord
    Id PkgId sql=pkg_id
    createdAt UTCTime
    updatedAt UTCTime Maybe
    deriving Eq
    deriving Show

VersionRecord sql=version
    createdAt UTCTime
    updatedAt UTCTime Maybe
    pkgId PkgRecordId
    number Version
    title Text
    descShort Text
    descLong Text
    iconType Text
    releaseNotes Text
    osVersion Version
    Primary pkgId number
    deriving Eq
    deriving Show

VersionPlatform 
    createdAt UTCTime
    updatedAt UTCTime Maybe
    pkgId PkgRecordId
    versionNumber Version
    arch OsArch
    Primary pkgId versionNumber
    deriving Eq
    deriving Show

OsVersion
    createdAt UTCTime
    updatedAt UTCTime
    number Version
    headline Text
    releaseNotes Text
    arch OsArch Maybe
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
    description Text
    priority Int default=0
    UniqueName name
    deriving Eq
    deriving Show

PkgCategory
    createdAt UTCTime
    pkgId PkgRecordId
    categoryId CategoryId
    UniquePkgCategory pkgId categoryId
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

UserActivity
    createdAt UTCTime
    serverId Text
    osVersion Version Maybe
    arch OsArch Maybe

Admin
    Id Text
    createdAt UTCTime
    passHash (Digest SHA256)

Upload
    uploader AdminId
    pkgId PkgRecordId
    pkgVersion Version
    createdAt UTCTime
|]
