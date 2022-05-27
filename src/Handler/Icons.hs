{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Icons where

import           Startlude                      ( ($)
                                                , Eq
                                                , Generic
                                                , Read
                                                , Show
                                                , show
                                                )

import           Data.Conduit                   ( (.|)
                                                , awaitForever
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Foundation                     ( Handler )
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion
                                                , getIcon
                                                , getInstructions
                                                , getLicense
                                                )
import           Lib.Types.AppIndex             ( PkgId )
import           Network.HTTP.Types             ( status400 )
import           Util.Shared                    ( getVersionSpecFromQuery
                                                , orThrow
                                                , versionPriorityFromQueryIsMin
                                                )
import           Yesod.Core                     ( FromJSON
                                                , ToJSON
                                                , TypedContent
                                                , addHeader
                                                , respondSource
                                                , sendChunkBS
                                                , sendResponseStatus
                                                , typePlain
                                                )

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
