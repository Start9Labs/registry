{-# LANGUAGE QuasiQuotes #-}
module Lib.Ssl where

import           Startlude

import           Data.String.Interpolate.IsString
import           System.Directory
import           System.FilePath
import           System.Process

import           Constants

-- openssl genrsa -out key.pem 2048
-- openssl req -new -key key.pem -out certificate.csr
-- openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

sslKeyLocation :: FilePath
sslKeyLocation = sslPath </> "key.pem"

sslCsrLocation :: FilePath
sslCsrLocation = sslPath </> "certificate.csr"

sslCertLocation :: FilePath
sslCertLocation = sslPath </> "certificate.pem"

checkForSslCert :: IO Bool
checkForSslCert =
    doesPathExist sslKeyLocation <&&> doesPathExist sslCertLocation

generateSslKey :: IO ExitCode
generateSslKey = rawSystem "openssl" ["genrsa", "-out", sslKeyLocation, "2048"]

generateSslCert :: Text -> IO ExitCode
generateSslCert name = rawSystem
    "openssl"
    ["req", "-new", "-key", sslKeyLocation, "-out", sslCsrLocation, "-subj", [i|/CN=#{name}.local|]]

selfSignSslCert :: IO ExitCode
selfSignSslCert = rawSystem
    "openssl"
    [ "x509"
    , "-req"
    , "-in"
    , sslCsrLocation
    , "-signkey"
    , sslKeyLocation
    , "-out"
    , sslCertLocation
    ]

setupSsl :: IO ()
setupSsl = do
    exists <- checkForSslCert
    unless exists $ do
        void $ system $ "mkdir -p " <> sslPath
        void generateSslKey
        void $ generateSslCert getRegistryHostname
        void selfSignSslCert
