{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Package.V0.Index where

import Foundation (Handler)
import Handler.Package.Api (PackageListRes)
import Handler.Package.V1.Index qualified
import Handler.Types.Api (ApiVersion (..))
import Startlude ((<$>))
import Unsafe.Coerce (unsafeCoerce)


-- this use of unsafecoerce is OK because the 'V0 witness does not appear in any way in the representation
getPackageIndexR :: Handler (PackageListRes 'V0)
getPackageIndexR = unsafeCoerce <$> Handler.Package.V1.Index.getPackageIndexR