{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module is here for the express purpose of keeping ecosystem dependencies separate from the core library.
-- The core library should in theory be only dependent on base, text, and attoparsec. These are reasonable dependencies.
-- aeson, persistent, and yesod are not. So we put those here as they will not be extracted into a separate library.
module Orphans.Emver where

import           Startlude

import           Data.Aeson
import qualified Data.Attoparsec.Text          as Atto

import           Lib.Types.Emver
import           Database.Persist.Sql
import qualified Data.Text                     as T
import           Control.Monad.Fail             ( MonadFail(fail) )
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

instance FromJSON Version where
    parseJSON = withText "Emver Version" $ either fail pure . Atto.parseOnly parseVersion
instance ToJSON Version where
    toJSON = String . show
instance FromJSON VersionRange where
    parseJSON = withText "Emver" $ either fail pure . Atto.parseOnly parseRange
instance ToJSON VersionRange where
    toJSON = String . show

instance PersistField Version where
    toPersistValue   = PersistText . show
    fromPersistValue = first T.pack . Atto.parseOnly parseVersion <=< fromPersistValue
instance PersistFieldSql Version where
    sqlType _ = SqlString
instance PersistField VersionRange where
    toPersistValue   = PersistText . show
    fromPersistValue = first T.pack . Atto.parseOnly parseRange <=< fromPersistValue
instance PersistFieldSql VersionRange where
    sqlType _ = SqlString
instance FromField Version where
    fromField a = fromJSONField a 
instance FromField [Version] where
    fromField a = fromJSONField a 
instance ToField [Version] where
    toField a = toJSONField a 