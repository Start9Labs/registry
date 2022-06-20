{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Package.V0.Index where

import Foundation (Handler)
import Handler.Package.Api (PackageListRes)
import Handler.Package.V1.Index qualified


-- implementation is the same but we will encode different payloads on the way out
getPackageIndexR :: Handler PackageListRes
getPackageIndexR = Handler.Package.V1.Index.getPackageIndexR