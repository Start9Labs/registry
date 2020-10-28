{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Handler.Types.Status where

import           Startlude

import           Data.Aeson
import           Yesod.Core.Content

import           Lib.Types.Emver
import           Orphans.Emver                  ( )

data AppVersionRes = AppVersionRes
    { appVersionVersion      :: Version
    , appVersionMinCompanion :: Maybe Version
    }
    deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes { appVersionVersion, appVersionMinCompanion } =
        object $ ["version" .= appVersionVersion] <> case appVersionMinCompanion of
            Nothing -> []
            Just x  -> ["minCompanion" .= x]

instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON

-- Ugh
instance ToContent (Maybe AppVersionRes) where
    toContent = toContent . toJSON
instance ToTypedContent (Maybe AppVersionRes) where
    toTypedContent = toTypedContent . toJSON
