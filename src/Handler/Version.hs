{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards  #-}

module Handler.Version where

import           Startlude               hiding ( Handler )

import           Yesod.Core

import qualified Data.Attoparsec.Text          as Atto
import           Data.String.Interpolate.IsString
                                                ( i )
import qualified Data.Text                     as T
import           Foundation
import           Handler.Types.Status
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion )
import           Lib.Types.AppIndex             ( PkgId )
import           Lib.Types.Emver                ( parseVersion
                                                , satisfies
                                                )
import           Network.HTTP.Types.Status      ( status404 )
import           Settings
import           System.FilePath                ( (</>) )
import           UnliftIO.Directory             ( listDirectory )
import           Util.Shared                    ( getVersionSpecFromQuery
                                                , orThrow
                                                )

getVersionR :: Handler AppVersionRes
getVersionR = AppVersionRes . registryVersion . appSettings <$> getYesod

getPkgVersionR :: PkgId -> Handler AppVersionRes
getPkgVersionR pkg = do
    spec <- getVersionSpecFromQuery
    AppVersionRes <$> getBestVersion pkg spec `orThrow` sendResponseStatus
        status404
        (NotFoundE [i|Version for #{pkg} satisfying #{spec}|])

getEosVersionR :: Handler AppVersionRes
getEosVersionR = do
    spec    <- getVersionSpecFromQuery
    root    <- getsYesod $ (</> "eos") . resourcesDir . appSettings
    subdirs <- listDirectory root
    let (failures, successes) = partitionEithers $ (Atto.parseOnly parseVersion . T.pack) <$> subdirs
    for_ failures $ \f -> $logWarn [i|Emver Parse Failure for EOS: #{f}|]
    let res = headMay . sortOn Down . filter (`satisfies` spec) $ successes
    maybe (sendResponseStatus status404 (NotFoundE [i|EOS version satisfying #{spec}|])) (pure . AppVersionRes) res
