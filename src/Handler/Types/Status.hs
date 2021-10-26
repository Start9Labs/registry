{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Handler.Types.Status where

import           Startlude               hiding ( toLower )

import           Data.Aeson
import           Yesod.Core.Content

import           Lib.Types.Emver
import           Orphans.Emver                  ( )

data AppVersionRes = AppVersionRes
    { appVersionVersion :: Version
    }
    deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes { appVersionVersion } = object $ ["version" .= appVersionVersion]
instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent (Maybe AppVersionRes) where
    toContent = toContent . toJSON
instance ToTypedContent (Maybe AppVersionRes) where
    toTypedContent = toTypedContent . toJSON
