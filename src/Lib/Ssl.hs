{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Ssl where

import           Startlude

import           Data.String.Interpolate.IsString
import           System.Directory
import           System.Process

import           Foundation
import           Settings

-- openssl genrsa -out key.pem 2048
-- openssl req -new -key key.pem -out certificate.csr
-- openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

setupSsl :: AppSettings -> IO ()
setupSsl AppSettings {..} = do
    exists <- checkForSslCert
    unless exists $ do
        void $ system $ "mkdir -p " <> sslPath
        void generateSslKey
        void $ generateSslCert registryHostname
        void selfSignSslCert
    where
        checkForSslCert :: IO Bool
        checkForSslCert = doesPathExist sslKeyLocation <&&> doesPathExist sslCertLocation

        generateSslKey :: IO ExitCode
        generateSslKey = rawSystem "openssl" ["genrsa", "-out", sslKeyLocation, "2048"]

        generateSslCert :: Text -> IO ExitCode
        generateSslCert name = rawSystem
            "openssl"
            ["req", "-new", "-key", sslKeyLocation, "-out", sslCsrLocation, "-subj", [i|/CN=#{name}.local|]]

        selfSignSslCert :: IO ExitCode
        selfSignSslCert = rawSystem
            "openssl"
            ["x509", "-req", "-in", sslCsrLocation, "-signkey", sslKeyLocation, "-out", sslCertLocation]

doesSslNeedRenew :: FilePath -> IO Bool
doesSslNeedRenew cert = do
    ec <- liftIO $ system [i|openssl x509 -checkend 2592000 -noout -in #{cert}|]
    pure $ ec /= ExitSuccess

renewSslCerts :: FilePath -> ReaderT RegistryCtx IO ()
renewSslCerts cert = do
    domain <- asks $ registryHostname . appSettings
    void . liftIO $ system [i|certbot renew|]
    void . liftIO $ system [i|cp /etc/letsencrypt/live/#{domain}/fullchain.pem #{cert}|]
