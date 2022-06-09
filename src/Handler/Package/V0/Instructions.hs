{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.Instructions where

import Conduit (awaitForever, (.|))
import Data.String.Interpolate.IsString (i)
import Foundation (Handler)
import Handler.Util (getVersionSpecFromQuery, orThrow, versionPriorityFromQueryIsMin)
import Lib.Error (S9Error (..))
import Lib.PkgRepository (getBestVersion, getInstructions)
import Lib.Types.AppIndex (PkgId)
import Network.HTTP.Types (status400)
import Startlude (show, ($))
import Yesod (TypedContent, addHeader, respondSource, sendChunkBS, sendResponseStatus, typePlain)


getInstructionsR :: PkgId -> Handler TypedContent
getInstructionsR pkg = do
    spec <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        getBestVersion pkg spec preferMin
            `orThrow` sendResponseStatus status400 (NotFoundE [i|Instructions for #{pkg} satisfying #{spec}|])
    (len, src) <- getInstructions pkg version
    addHeader "Content-Length" (show len)
    respondSource typePlain $ src .| awaitForever sendChunkBS
