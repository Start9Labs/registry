{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards  #-}

module Handler.Version where

import           Startlude               hiding ( Handler )

import           Yesod.Core

import           Data.String.Interpolate.IsString
                                                ( i )
import           Foundation
import           Handler.Types.Status
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion )
import           Lib.Types.AppIndex             ( PkgId )
import           Network.HTTP.Types.Status      ( status404 )
import           Util.Shared                    ( getVersionSpecFromQuery
                                                , orThrow
                                                )

getPkgVersionR :: PkgId -> Handler AppVersionRes
getPkgVersionR pkg = do
    spec <- getVersionSpecFromQuery
    AppVersionRes <$> getBestVersion pkg spec `orThrow` sendResponseStatus
        status404
        (NotFoundE [i|Version for #{pkg} satisfying #{spec}|])
