{-# LANGUAGE QuasiQuotes #-}

module Handler.Package.V0.Instructions where

import Conduit (
    awaitForever,
    (.|),
 )
import Data.String.Interpolate.IsString (
    i,
 )
import Foundation (Handler)
import Handler.Package.V1.Index (getOsVersionQuery)
import Handler.Util (
    fetchCompatiblePkgVersions,
    getVersionSpecFromQuery,
    orThrow,
    versionPriorityFromQueryIsMin,
 )
import Lib.Error (S9Error (..))
import Lib.PkgRepository (
    getBestVersion,
    getInstructions,
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


getInstructionsR :: PkgId -> Handler TypedContent
getInstructionsR pkg = do
    spec <- getVersionSpecFromQuery
    osVersion <- getOsVersionQuery
    osCompatibleVersions <- fetchCompatiblePkgVersions osVersion pkg
    preferMin <- versionPriorityFromQueryIsMin
    version <-
        ( pure $
                getBestVersion spec preferMin osCompatibleVersions
            )
            `orThrow` sendResponseStatus status400 (NotFoundE [i|Instructions for #{pkg} satisfying #{spec}|])
    (len, src) <- getInstructions pkg version
    addHeader "Content-Length" (show len)
    respondSource typePlain $ src .| awaitForever sendChunkBS
