module Handler.Root where
import Startlude
import Yesod
import Foundation
import qualified Data.Text as T

getRootR :: HandlerFor RegistryCtx ()
getRootR =  redirect $ T.pack "https://marketplace.start9.com"