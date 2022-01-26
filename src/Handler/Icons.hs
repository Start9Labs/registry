{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Icons where

import           Startlude               hiding ( Handler )

import           Data.Conduit                   ( (.|)
                                                , awaitForever
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Foundation
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion
                                                , getIcon
                                                , getInstructions
                                                , getLicense
                                                )
import           Lib.Types.AppIndex
import           Network.HTTP.Types
import           Util.Shared
import           Yesod.Core

data IconType = PNG | JPG | JPEG | SVG
    deriving (Eq, Show, Generic, Read)
instance ToJSON IconType
instance FromJSON IconType

getIconsR :: PkgId -> Handler TypedContent
getIconsR pkg = do
    spec      <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version   <- getBestVersion pkg spec preferMin
        `orThrow` sendResponseStatus status400 (NotFoundE [i|Icon for #{pkg} satisfying #{spec}|])
    (ct, len, src) <- getIcon pkg version
    addHeader "Content-Length" (show len)
    respondSource ct $ src .| awaitForever sendChunkBS

getLicenseR :: PkgId -> Handler TypedContent
getLicenseR pkg = do
    spec      <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version   <- getBestVersion pkg spec preferMin
        `orThrow` sendResponseStatus status400 (NotFoundE [i|License for #{pkg} satisfying #{spec}|])
    (len, src) <- getLicense pkg version
    addHeader "Content-Length" (show len)
    respondSource typePlain $ src .| awaitForever sendChunkBS

getInstructionsR :: PkgId -> Handler TypedContent
getInstructionsR pkg = do
    spec      <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    version   <- getBestVersion pkg spec preferMin
        `orThrow` sendResponseStatus status400 (NotFoundE [i|Instructions for #{pkg} satisfying #{spec}|])
    (len, src) <- getInstructions pkg version
    addHeader "Content-Length" (show len)
    respondSource typePlain $ src .| awaitForever sendChunkBS
