module Handler.Package where

import Foundation (Handler)
import Handler.Package.V0.Index (PackageListRes)
import Handler.Package.V0.Info (InfoRes)
import Handler.Package.V0.Latest (VersionLatestRes)
import Handler.Package.V0.ReleaseNotes (ReleaseNotes)
import Handler.Types.Api (ApiVersion)
import Handler.Types.Status (AppVersionRes)
import Lib.Registry (S9PK)
import Lib.Types.AppIndex (PkgId)
import Yesod.Core.Types (
    JSONResponse,
    TypedContent,
 )


getInfoR :: ApiVersion -> Handler (JSONResponse InfoRes)
getInfoR = _


getPackageListR :: ApiVersion -> Handler PackageListRes
getPackageListR = _


getVersionLatestR :: ApiVersion -> Handler VersionLatestRes
getVersionLatestR = _


getAppR :: ApiVersion -> S9PK -> Handler TypedContent
getAppR = _


getAppManifestR :: ApiVersion -> PkgId -> Handler TypedContent
getAppManifestR = _


getReleaseNotesR :: ApiVersion -> PkgId -> Handler ReleaseNotes
getReleaseNotesR = _


getIconsR :: ApiVersion -> PkgId -> Handler TypedContent
getIconsR = _


getLicenseR :: ApiVersion -> PkgId -> Handler TypedContent
getLicenseR = _


getInstructionsR :: ApiVersion -> PkgId -> Handler TypedContent
getInstructionsR = _


getPkgVersionR :: ApiVersion -> PkgId -> Handler AppVersionRes
getPkgVersionR = _
