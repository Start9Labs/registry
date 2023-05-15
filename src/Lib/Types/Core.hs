{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Types.Core where

import Startlude (
    ConvertText (toS),
    Either (Left, Right),
    Eq ((==)),
    Functor (fmap),
    Hashable (hashWithSalt),
    IsString (..),
    KnownSymbol,
    Ord,
    Proxy (Proxy),
    Read,
    Show,
    String,
    Symbol,
    Text,
    readMaybe,
    show,
    symbolVal,
    ($),
    (.), Enum,
 )

import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
 )
import Data.Functor.Contravariant (contramap)
import Data.String.Interpolate.IsString (
    i,
 )
import Database.Persist (
    PersistField (..),
    PersistValue (PersistText),
    SqlType (..),
 )
import Database.Persist.Sql (PersistFieldSql (sqlType))
import GHC.Read (Read (readsPrec))
import Orphans.Emver ()
import Protolude.Base qualified as P (
    Show (..),
 )
import System.FilePath (splitExtension, (<.>))
import Web.HttpApiData (
    FromHttpApiData,
    ToHttpApiData,
 )
import Yesod (PathPiece (..))
import Prelude (read)


newtype PkgId = PkgId {unPkgId :: Text}
    deriving stock (Eq, Ord)
    deriving newtype (FromHttpApiData, ToHttpApiData)
instance IsString PkgId where
    fromString = PkgId . fromString
instance P.Show PkgId where
    show = toS . unPkgId
instance Read PkgId where
    readsPrec _ s = [(PkgId $ toS s, "")]
instance Hashable PkgId where
    hashWithSalt n = hashWithSalt n . unPkgId
instance FromJSON PkgId where
    parseJSON = fmap PkgId . parseJSON
instance ToJSON PkgId where
    toJSON = toJSON . unPkgId
instance FromJSONKey PkgId where
    fromJSONKey = fmap PkgId fromJSONKey
instance ToJSONKey PkgId where
    toJSONKey = contramap unPkgId toJSONKey
instance PersistField PkgId where
    toPersistValue = PersistText . show
    fromPersistValue (PersistText t) = Right . PkgId $ toS t
    fromPersistValue other = Left [i|Invalid AppId: #{other}|]
instance PersistFieldSql PkgId where
    sqlType _ = SqlString
instance PathPiece PkgId where
    fromPathPiece = fmap PkgId . fromPathPiece
    toPathPiece = unPkgId

data OsArch = X86_64 | AARCH64 | RASPBERRYPI | X86_64_NONFREE | AARCH64_NONFREE
    deriving (Eq, Ord, Enum)
instance Show OsArch where
    show X86_64 = "x86_64"
    show AARCH64 = "aarch64"
    show RASPBERRYPI = "raspberrypi"
    show X86_64_NONFREE = "x86_64-nonfree"
    show AARCH64_NONFREE = "aarch64-nonfree"
instance Read OsArch where
    readsPrec _ "x86_64" = [(X86_64, "")]
    readsPrec _ "aarch64" = [(AARCH64, "")]
    readsPrec _ "raspberrypi" = [(RASPBERRYPI, "")]
    readsPrec _ "rasberrypi" = [(RASPBERRYPI, "")]
    readsPrec _ "x86_64-nonfree" = [(X86_64_NONFREE, "")]
    readsPrec _ "aarch64-nonfree" = [(AARCH64_NONFREE, "")]
    readsPrec _ _ = []
instance PersistField OsArch where
    toPersistValue = PersistText . show
    fromPersistValue (PersistText t) = Right $ read $ toS t
    fromPersistValue other = Left [i|Invalid OsArch: #{other}|]
instance PersistFieldSql OsArch where
    sqlType _ = SqlString
instance FromJSON OsArch where
    parseJSON = parseJSON
instance ToJSON OsArch where
    toJSON = toJSON

newtype Extension (a :: Symbol) = Extension String deriving (Eq)
type S9PK = Extension "s9pk"
instance KnownSymbol a => Show (Extension a) where
    show e@(Extension file) = file <.> extension e
instance KnownSymbol a => Read (Extension a) where
    readsPrec _ s = case symbolVal $ Proxy @a of
        "" -> [(Extension s, "")]
        other -> [(Extension file, "") | ext' == "" <.> other]
        where
            (file, ext') = splitExtension s
instance KnownSymbol a => PathPiece (Extension a) where
    fromPathPiece = readMaybe . toS
    toPathPiece = show


extension :: KnownSymbol a => Extension a -> String
extension = symbolVal