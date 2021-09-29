{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Apps where

import           Startlude               hiding ( Handler )

import           Control.Monad.Logger           ( logError
                                                , logInfo
                                                )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                )
import qualified Data.Attoparsec.Text          as Atto
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import           Database.Persist               ( Entity(entityKey) )
import qualified GHC.Show                       ( Show(..) )
import           Network.HTTP.Types             ( status404 )
import           System.FilePath                ( (<.>)
                                                , takeBaseName
                                                )
import           Yesod.Core                     ( TypedContent
                                                , addHeader
                                                , notFound
                                                , respondSource
                                                , sendChunkBS
                                                , sendResponseStatus
                                                , typeJson
                                                , typeOctet
                                                , waiRequest
                                                )
import           Yesod.Persist.Core             ( YesodPersist(runDB) )

import           Conduit                        ( (.|)
                                                , awaitForever
                                                )
import           Data.String.Interpolate.IsString
                                                ( i )
import           Database.Queries               ( createMetric
                                                , fetchApp
                                                , fetchAppVersion
                                                )
import           Foundation                     ( Handler )
import           Lib.Error                      ( S9Error(NotFoundE) )
import           Lib.PkgRepository              ( getBestVersion
                                                , getManifest
                                                , getPackage
                                                )
import           Lib.Registry                   ( S9PK )
import           Lib.Types.AppIndex             ( PkgId(PkgId) )
import           Lib.Types.Emver                ( Version
                                                , parseVersion
                                                )
import           Network.Wai                    ( Request(requestHeaderUserAgent) )
import           Util.Shared                    ( addPackageHeader
                                                , getVersionSpecFromQuery
                                                , orThrow
                                                )

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . BS.toStrict . encode) pure)

data FileExtension = FileExtension FilePath (Maybe String)
instance Show FileExtension where
    show (FileExtension f Nothing ) = f
    show (FileExtension f (Just e)) = f <.> e

userAgentOsVersionParser :: Atto.Parser Version
userAgentOsVersionParser = do
    void $ (Atto.string "EmbassyOS" <|> Atto.string "AmbassadorOS" <|> Atto.string "MeshOS") *> Atto.char '/'
    parseVersion

getEmbassyOsVersion :: Handler (Maybe Version)
getEmbassyOsVersion = userAgentOsVersion
    where
        userAgentOsVersion =
            (hush . Atto.parseOnly userAgentOsVersionParser . decodeUtf8 <=< requestHeaderUserAgent) <$> waiRequest

getAppManifestR :: PkgId -> Handler TypedContent
getAppManifestR pkg = do
    versionSpec <- getVersionSpecFromQuery
    version     <- getBestVersion pkg versionSpec
        `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    (len, src) <- getManifest pkg version
    addHeader "Content-Length" (show len)
    respondSource typeJson $ src .| awaitForever sendChunkBS

getAppR :: S9PK -> Handler TypedContent
getAppR file = do
    let pkg = PkgId . T.pack $ takeBaseName (show file)
    versionSpec <- getVersionSpecFromQuery
    version     <- getBestVersion pkg versionSpec
        `orThrow` sendResponseStatus status404 (NotFoundE [i|#{pkg} satisfying #{versionSpec}|])
    addPackageHeader pkg version
    void $ recordMetrics pkg version
    (len, src) <- getPackage pkg version
    addHeader "Content-Length" (show len)
    respondSource typeOctet $ src .| awaitForever sendChunkBS


recordMetrics :: PkgId -> Version -> Handler ()
recordMetrics pkg appVersion = do
    sa <- runDB $ fetchApp $ pkg
    case sa of
        Nothing -> do
            $logError $ [i|#{pkg} not found in database|]
            notFound
        Just a -> do
            let appKey' = entityKey a
            existingVersion <- runDB $ fetchAppVersion appVersion appKey'
            case existingVersion of
                Nothing -> do
                    $logError $ [i|#{pkg}@#{appVersion} not found in database|]
                    notFound
                Just v -> runDB $ createMetric (entityKey a) (entityKey v)

