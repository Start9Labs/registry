{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Types.Category where

import           Startlude
import           Database.Persist.Postgresql
import           Data.Aeson
import           Control.Monad
import           Yesod.Core

data CategoryTitle = FEATURED
        | BITCOIN
        | LIGHTNING
        | DATA
        | MESSAGING
        | SOCIAL
        | ALTCOIN
    deriving (Eq, Enum, Show, Read)
instance PersistField CategoryTitle where
    fromPersistValue = fromPersistValueJSON
    toPersistValue   = toPersistValueJSON
instance PersistFieldSql CategoryTitle where
    sqlType _ = SqlString
instance ToJSON CategoryTitle where
    -- toJSON = String . T.toLower . show
    toJSON = \case
        FEATURED  -> "featured"
        BITCOIN   -> "bitcoin"
        LIGHTNING -> "lightning"
        DATA      -> "data"
        MESSAGING -> "messaging"
        SOCIAL    -> "social"
        ALTCOIN   -> "alt coin"
instance FromJSON CategoryTitle where
    parseJSON = withText "CategoryTitle" $ \case
        "featured"  -> pure FEATURED
        "bitcoin"   -> pure BITCOIN
        "lightning" -> pure LIGHTNING
        "data"      -> pure DATA
        "messaging" -> pure MESSAGING
        "social"    -> pure SOCIAL
        "alt coin"  -> pure ALTCOIN
        _           -> fail "unknown category title"
instance ToContent CategoryTitle where
    toContent = toContent . toJSON
instance ToTypedContent CategoryTitle where
    toTypedContent = toTypedContent . toJSON
