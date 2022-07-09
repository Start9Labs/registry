{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.Version where

import Data.Aeson (
    ToJSON,
    object,
    (.=),
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Foundation (Handler)
import Handler.Package.V1.Index (getOsVersionQuery)
import Handler.Util (
    filterOsCompat,
    getVersionSpecFromQuery,
    orThrow,
    versionPriorityFromQueryIsMin,
 )
import Lib.Error (S9Error (..))
import Lib.PkgRepository (getBestVersion)
import Lib.Types.Core (PkgId)
import Lib.Types.Emver (Version (..))
import Network.HTTP.Types (status404)
import Startlude (
    Eq,
    Maybe,
    Show,
    (.),
    (<$>),
 )
import Yesod (
    ToContent (..),
    ToTypedContent,
    sendResponseStatus,
 )
import Yesod.Core (
    ToJSON (..),
    ToTypedContent (..),
 )


newtype AppVersionRes = AppVersionRes
    { appVersionVersion :: Version
    }
    deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes{appVersionVersion} = object ["version" .= appVersionVersion]
instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent (Maybe AppVersionRes) where
    toContent = toContent . toJSON
instance ToTypedContent (Maybe AppVersionRes) where
    toTypedContent = toTypedContent . toJSON


getPkgVersionR :: PkgId -> Handler AppVersionRes
getPkgVersionR pkg = do
    osVersion <- getOsVersionQuery
    osCompatibleVersions <- filterOsCompat osVersion pkg
    spec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    AppVersionRes <$> getBestVersion spec preferMin osCompatibleVersions
        `orThrow` sendResponseStatus
            status404
            (NotFoundE [i|Version for #{pkg} satisfying #{spec}|])
