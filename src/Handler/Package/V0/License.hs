{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.License where

import Data.String.Interpolate.IsString (i)
import Foundation (Handler)
import Handler.Util (getVersionSpecFromQuery)
import Lib.Types.AppIndex (PkgId)
import Yesod (TypedContent)


getLicenseR :: PkgId -> Handler TypedContent
getLicenseR pkg = do
    spec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion pkg spec preferMin
            `orThrow` sendResponseStatus status400 (NotFoundE [i|License for #{pkg} satisfying #{spec}|])
    (len, src) <- getLicense pkg version
    addHeader "Content-Length" (show len)
    respondSource typePlain $ src .| awaitForever sendChunkBS
