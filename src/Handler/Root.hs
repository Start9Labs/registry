module Handler.Root where
import Startlude
import Yesod
import Foundation
import qualified Data.Text as T
import Settings (AppSettings(registryHostname))

getRootR :: HandlerFor RegistryCtx ()
getRootR =  do
  hostname <- getsYesod $ registryHostname . appSettings
  redirect $ T.pack "https://marketplace.start9.com?api=" <> hostname