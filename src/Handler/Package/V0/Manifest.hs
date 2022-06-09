{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.Manifest where

import Conduit (awaitForever, (.|))
import Data.String.Interpolate.IsString (i)
import Foundation (Handler)
import Handler.Util (addPackageHeader, getVersionSpecFromQuery, orThrow, versionPriorityFromQueryIsMin)
import Lib.Error (S9Error (..))
import Lib.PkgRepository (getBestVersion, getManifest)
import Lib.Types.Core (PkgId)
import Network.HTTP.Types (status404)
import Startlude (show, ($))
import Yesod (TypedContent, addHeader, respondSource, sendChunkBS, sendResponseStatus, typeJson)


getAppManifestR :: PkgId -> Handler TypedContent
getAppManifestR pkg = do
    versionSpec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion pkg versionSpec preferMin
            `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    (len, src) <- getManifest pkg version
    addHeader "Content-Length" (show len)
    respondSource typeJson $ src .| awaitForever sendChunkBS