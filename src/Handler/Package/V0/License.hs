{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.License where

import Conduit (
    awaitForever,
    (.|),
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Foundation (Handler)
import Handler.Package.V1.Index (getOsVersionCompat)
import Handler.Util (
    fetchCompatiblePkgVersions,
    getVersionSpecFromQuery,
    orThrow,
    versionPriorityFromQueryIsMin,
 )
import Lib.Error (S9Error (..))
import Lib.PkgRepository (
    getBestVersion,
    getLicense,
 )
import Lib.Types.Core (PkgId)
import Network.HTTP.Types (status400)
import Startlude (
    pure,
    show,
    ($),
 )
import Yesod (
    TypedContent,
    addHeader,
    respondSource,
    sendChunkBS,
    sendResponseStatus,
    typePlain,
 )


getLicenseR :: PkgId -> Handler TypedContent
getLicenseR pkg = do
    osVersion <- getOsVersionCompat
    osCompatibleVersions <- fetchCompatiblePkgVersions osVersion pkg
    spec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        (pure $ getBestVersion spec preferMin osCompatibleVersions)
            `orThrow` sendResponseStatus status400 (NotFoundE [i|License for #{pkg} satisfying #{spec}|])
    (len, src) <- getLicense pkg version
    addHeader "Content-Length" (show len)
    respondSource typePlain $ src .| awaitForever sendChunkBS
