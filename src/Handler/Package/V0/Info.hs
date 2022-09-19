module Handler.Package.V0.Info where

import Data.Aeson (ToJSON (..))
import Database.Esqueleto.Experimental (Entity (..), asc, from, orderBy, select, table, (^.))
import Foundation (Handler, RegistryCtx (..))
import Handler.Util (tickleMAU)
import Model (Category (..), EntityField (..))
import Settings (AppSettings (..))
import Startlude (Generic, Show, Text, pure, ($), (.), (<$>), (&&&), Maybe)
import Yesod (ToContent (..), ToTypedContent (..), YesodPersist (runDB), getsYesod)
import Yesod.Core.Types (JSONResponse (..))


data InfoRes = InfoRes
    { name :: !Text
    , description:: !(Maybe Text)
    , categories :: ![Text]
    }
    deriving (Show, Generic)
instance ToJSON InfoRes
instance ToContent InfoRes where
    toContent = toContent . toJSON
instance ToTypedContent InfoRes where
    toTypedContent = toTypedContent . toJSON


getInfoR :: Handler (JSONResponse InfoRes)
getInfoR = do
    (name, description) <- getsYesod $ (marketplaceName &&& marketplaceDescription) . appSettings
    allCategories <- runDB $
        select $ do
            cats <- from $ table @Category
            orderBy [asc (cats ^. CategoryPriority)]
            pure cats
    tickleMAU
    pure $ JSONResponse $ InfoRes name description $ categoryName . entityVal <$> allCategories
