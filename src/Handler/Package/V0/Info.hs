module Handler.Package.V0.Info where

import Data.Aeson (ToJSON (..))
import Database.Esqueleto.Experimental (Entity (..), asc, from, orderBy, select, table, (^.))
import Foundation (Handler, RegistryCtx (..))
import Handler.Util (tickleMAU)
import Model (Category (..), EntityField (..))
import Settings (AppSettings (..))
import Startlude (Generic, Show, Text, pure, ($), (.), (<$>), MonadIO (liftIO), decodeUtf8)
import Yesod (ToContent (..), ToTypedContent (..), YesodPersist (runDB), getsYesod)
import Yesod.Core.Types (JSONResponse (..))
import System.FilePath ((</>))
import Data.ByteString (readFile)


data InfoRes = InfoRes
    { name :: !Text
    , categories :: ![Text]
    , icon :: !Text -- base64
    }
    deriving (Show, Generic)
instance ToJSON InfoRes
instance ToContent InfoRes where
    toContent = toContent . toJSON
instance ToTypedContent InfoRes where
    toTypedContent = toTypedContent . toJSON


getInfoR :: Handler (JSONResponse InfoRes)
getInfoR = do
    name <- getsYesod $ marketplaceName . appSettings
    iconFile <- getsYesod $ (</> "icon") . iconPath . appSettings
    icon' <- liftIO $ readFile iconFile
    let icon = decodeUtf8 icon'

    allCategories <- runDB $
        select $ do
            cats <- from $ table @Category
            orderBy [asc (cats ^. CategoryPriority)]
            pure cats
    tickleMAU
    pure $ JSONResponse $ InfoRes name (categoryName . entityVal <$> allCategories) icon
