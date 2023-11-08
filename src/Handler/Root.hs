module Handler.Root where
import Startlude
import Yesod
import Foundation
import qualified Data.Text as T
import Settings (AppSettings(registryHostname, marketplaceName))
import Network.Wai (Request(pathInfo))
import Lib.Types.Core (PkgId)

getRootR :: HandlerFor RegistryCtx ()
getRootR =  do
  (url, name) <- getsYesod $ (registryHostname &&& marketplaceName) . appSettings
  redirect $ T.pack "https://marketplace.start9.com?api=" <> url <> T.pack "&name=" <> name

getMarketplaceR :: PkgId -> HandlerFor RegistryCtx ()
getMarketplaceR _ = do
  (url, name) <- getsYesod $ (registryHostname &&& marketplaceName) . appSettings
  req <- waiRequest
  pathSegments <- pure $ pathInfo req
  let pathPiece = "/" <> T.intercalate "/" pathSegments
  redirect $ T.pack "https://marketplace.start9.com" <> pathPiece <> "?api=" <> url <> T.pack "&name=" <> name