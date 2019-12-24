module Live.UpdateAgent where

import           Application
import           Lib.Types.StoreApp
import           Lib.UpdateAgent
import           Startlude

av :: AppVersion
av = AppVersion (0,0,0)

avs :: AppVersionSpecification
avs = AppVersionSpecification SVEquals av

-- Need a few things to run this...
-- 1) a running "registry" server, pointed to by the settings.yml this file is run against.
-- 2) that server needs to serve up an executable file at /agent.0.0.0 (the version of av above)
-- 3) the executable file must itself spin up a server on the same port as this application, defined again in settings.yml
-- 4) that server must also respond to /version with a semver version in the format "0.0.0"
-- 5) If all goes well, the stack ghci session which calls updateAgentLive should have been killed, and the executable should still be running

updateAgentLive :: IO ()
updateAgentLive = do
    (_, agentCtx, _) <- getApplicationRepl
    updateAgent' avs agentCtx
