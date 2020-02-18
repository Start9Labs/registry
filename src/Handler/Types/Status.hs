{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Handler.Types.Status where

import           Startlude

import           Data.Aeson
import           Yesod.Core.Content

import           Lib.Types.Semver

data AppVersionRes = AppVersionRes
    { appVersionVersion      :: AppVersion
    , appVersionMinCompanion :: Maybe AppVersion
    } deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes{ appVersionVersion } = object ["version" .= appVersionVersion]

instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON

-- Ugh
instance ToContent (Maybe AppVersionRes) where
    toContent = toContent . toJSON
instance ToTypedContent (Maybe AppVersionRes) where
    toTypedContent = toTypedContent . toJSON

querySpec :: Maybe Text -> Maybe AppVersionSpecification
querySpec = (readMaybe . toS =<<)

querySpecD :: AppVersionSpecification -> Maybe Text -> AppVersionSpecification
querySpecD defaultSpec = fromMaybe defaultSpec . querySpec
