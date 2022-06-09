{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards  #-}

module Handler.Version where

import           Startlude                      ( (<$>) )

import           Yesod.Core                     ( sendResponseStatus )

import           Data.String.Interpolate.IsString
                                                ( i )
import           Foundation                     ( Handler )
import           Handler.Types.Status           ( AppVersionRes(AppVersionRes) )
import           Handler.Util                   ( orThrow )
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion )
import           Lib.Types.AppIndex             ( PkgId )
import           Network.HTTP.Types.Status      ( status404 )
import           Util.Shared                    ( getVersionSpecFromQuery
                                                , versionPriorityFromQueryIsMin
                                                )

getPkgVersionR :: PkgId -> Handler AppVersionRes
getPkgVersionR pkg = do
    spec      <- getVersionSpecFromQuery
    preferMin <- versionPriorityFromQueryIsMin
    AppVersionRes <$> getBestVersion pkg spec preferMin `orThrow` sendResponseStatus
        status404
        (NotFoundE [i|Version for #{pkg} satisfying #{spec}|])
