module Handler.Package where

import Foundation (Handler)
import Handler.Package.V0.Icon qualified
import Handler.Package.V0.Index (PackageListRes, getPackageIndexR)
import Handler.Package.V0.Info (InfoRes, getInfoR)
import Handler.Package.V0.Instructions qualified
import Handler.Package.V0.Latest (VersionLatestRes, getVersionLatestR)
import Handler.Package.V0.License qualified
import Handler.Package.V0.Manifest qualified
import Handler.Package.V0.ReleaseNotes (ReleaseNotes, getReleaseNotesR)
import Handler.Package.V0.S9PK qualified
import Handler.Package.V0.Version (AppVersionRes, getPkgVersionR)
import Handler.Types.Api (ApiVersion (..))
import Lib.Types.Core (PkgId, S9PK)
import Yesod.Core.Types (
    JSONResponse,
    TypedContent,
 )


getInfoR :: ApiVersion -> Handler (JSONResponse InfoRes)
getInfoR _ = Handler.Package.V0.Info.getInfoR


getPackageIndexR :: ApiVersion -> Handler PackageListRes
getPackageIndexR _ = Handler.Package.V0.Index.getPackageIndexR


getVersionLatestR :: ApiVersion -> Handler VersionLatestRes
getVersionLatestR _ = Handler.Package.V0.Latest.getVersionLatestR


getAppR :: ApiVersion -> S9PK -> Handler TypedContent
getAppR _ = Handler.Package.V0.S9PK.getAppR


getAppManifestR :: ApiVersion -> PkgId -> Handler TypedContent
getAppManifestR _ = Handler.Package.V0.Manifest.getAppManifestR


getReleaseNotesR :: ApiVersion -> PkgId -> Handler ReleaseNotes
getReleaseNotesR _ = Handler.Package.V0.ReleaseNotes.getReleaseNotesR


getIconsR :: ApiVersion -> PkgId -> Handler TypedContent
getIconsR _ = Handler.Package.V0.Icon.getIconsR


getLicenseR :: ApiVersion -> PkgId -> Handler TypedContent
getLicenseR _ = Handler.Package.V0.License.getLicenseR


getInstructionsR :: ApiVersion -> PkgId -> Handler TypedContent
getInstructionsR _ = Handler.Package.V0.Instructions.getInstructionsR


getPkgVersionR :: ApiVersion -> PkgId -> Handler AppVersionRes
getPkgVersionR _ = Handler.Package.V0.Version.getPkgVersionR
