module Handler.Root where
import Startlude
import Yesod
import Foundation
import qualified Data.Text as T
import Settings (AppSettings(registryHostname, marketplaceName))

getRootR :: HandlerFor RegistryCtx ()
getRootR =  do
  url <- getsYesod $ registryHostname . appSettings
  name <- getsYesod $ marketplaceName . appSettings
  redirect $ T.pack "https://marketplace.start9.com?api=" <> url <> T.pack "&name=" <> name