{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Admin where

import           Conduit                        ( (.|)
                                                , runConduit
                                                , sinkFile
                                                )
import           Control.Monad.Reader.Has       ( ask )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Aeson                     ( decodeFileStrict )
import           Foundation
import           Lib.PkgRepository              ( PkgRepo(PkgRepo, pkgRepoFileRoot)
                                                , extractPkg
                                                )
import           Lib.Types.AppIndex             ( PackageManifest(..) )
import           Network.HTTP.Types             ( status500 )
import           Startlude                      ( ($)
                                                , (.)
                                                , (<$>)
                                                , SomeException(..)
                                                , asum
                                                , hush
                                                , isNothing
                                                , liftIO
                                                , replicate
                                                , show
                                                , when
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           UnliftIO                       ( try
                                                , withSystemTempDirectory
                                                )
import           UnliftIO.Directory             ( renameDirectory )
import           Util.Shared                    ( orThrow
                                                , sendResponseText
                                                )
import           Yesod                          ( getsYesod
                                                , logError
                                                , rawRequestBody
                                                )

postPkgUploadR :: Handler ()
postPkgUploadR = do
    withSystemTempDirectory "newpkg" $ \path -> do
        runConduit $ rawRequestBody .| sinkFile (path </> "temp" <.> "s9pk")
        pool         <- getsYesod appConnPool
        PkgRepo {..} <- ask
        res          <- retry $ extractPkg pool path
        when (isNothing res) $ do
            $logError "Failed to extract package"
            sendResponseText status500 "Failed to extract package"
        PackageManifest {..} <- liftIO (decodeFileStrict (path </> "manifest.json"))
            `orThrow` sendResponseText status500 "Failed to parse manifest.json"
        renameDirectory path (pkgRepoFileRoot </> show packageManifestId </> show packageManifestVersion)
    where retry m = runMaybeT . asum $ replicate 3 (MaybeT $ hush <$> try @_ @SomeException m)

postPkgIndexR :: Handler ()
postPkgIndexR = _

postPkgDeindexR :: Handler ()
postPkgDeindexR = _
