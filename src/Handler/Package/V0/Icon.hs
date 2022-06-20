{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.Icon where

import Conduit (awaitForever, (.|))
import Data.String.Interpolate.IsString (
    i,
 )
import Foundation (Handler)
import Handler.Util (
    getVersionSpecFromQuery,
    orThrow,
    versionPriorityFromQueryIsMin,
 )
import Lib.Error (S9Error (..))
import Lib.PkgRepository (getBestVersion, getIcon)
import Lib.Types.Core (PkgId)
import Network.HTTP.Types (status400)
import Startlude (show, ($))
import Yesod (TypedContent, addHeader, respondSource, sendChunkBS, sendResponseStatus)


getIconsR :: PkgId -> Handler TypedContent
getIconsR pkg = do
    spec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion pkg spec preferMin
            `orThrow` sendResponseStatus status400 (NotFoundE [i|Icon for #{pkg} satisfying #{spec}|])
    (ct, len, src) <- getIcon pkg version
    addHeader "Content-Length" (show len)
    respondSource ct $ src .| awaitForever sendChunkBS
