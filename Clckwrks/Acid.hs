{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Acid where

import Clckwrks.NavBar.Acid          (NavBarState       , initialNavBarState)
import Clckwrks.ProfileData.Acid   (ProfileDataState, initialProfileDataState)
import Clckwrks.Types              (UUID)
import Clckwrks.URL                (ClckURL)
import Control.Applicative         ((<$>))
import Control.Exception           (bracket, catch, throw)
import Control.Concurrent          (killThread, forkIO)
import Control.Monad.Reader        (ask)
import Control.Monad.State         (modify)
import Data.Acid                   (AcidState, Query, Update, makeAcidic)
import Data.Acid.Local             (openLocalStateFrom, createArchive, createCheckpointAndClose)
import Data.Acid.Remote            (acidServer, skipAuthenticationCheck)
import Data.Data                   (Data, Typeable)
import Data.Maybe                  (fromMaybe)
import Data.SafeCopy               (base, deriveSafeCopy)
import Data.Text                   (Text)
import Happstack.Auth.Core.Auth    (AuthState       , initialAuthState)
import Happstack.Auth.Core.Profile (ProfileState    , initialProfileState)
import Network                     (PortID(UnixSocket))
import Prelude                     hiding (catch)
import System.Directory            (removeFile)
import System.FilePath             ((</>))
import System.IO.Error             (isDoesNotExistError)
import HSP.Google.Analytics        (UACCT)

-- | 'CoreState' holds some values that are required by the core
-- itself, or which are useful enough to be shared with numerous
-- plugins/themes.
data CoreState = CoreState
    { coreUACCT        :: Maybe UACCT  -- ^ Google Account UAACT
    , coreRootRedirect :: Maybe Text
    }
    deriving (Eq, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''CoreState)

initialCoreState :: CoreState
initialCoreState = CoreState
    { coreUACCT        = Nothing
    , coreRootRedirect = Nothing
    }

-- | get the 'UACCT' for Google Analytics
getUACCT :: Query CoreState (Maybe UACCT)
getUACCT = coreUACCT <$> ask

-- | set the 'UACCT' for Google Analytics
setUACCT :: Maybe UACCT -> Update CoreState ()
setUACCT mua = modify $ \cs -> cs { coreUACCT = mua }

-- | get the path that @/@ should redirect to
getRootRedirect :: Query CoreState (Maybe Text)
getRootRedirect = coreRootRedirect <$> ask

-- | set the path that @/@ should redirect to
setRootRedirect :: Maybe Text -> Update CoreState ()
setRootRedirect path = modify $ \cs -> cs { coreRootRedirect = path }

$(makeAcidic ''CoreState
  [ 'getUACCT
  , 'setUACCT
  , 'getRootRedirect
  , 'setRootRedirect
  ])

data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    , acidProfileData :: AcidState ProfileDataState
    , acidCore        :: AcidState CoreState
    , acidNavBar        :: AcidState NavBarState
    }

class GetAcidState m st where
    getAcidState :: m (AcidState st)

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createArchiveCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createArchiveCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createArchiveCheckpointAndClose) $ \profileData ->
    bracket (openLocalStateFrom (basePath </> "core")        initialCoreState)        (createArchiveCheckpointAndClose) $ \core ->
    bracket (openLocalStateFrom (basePath </> "navBar")      initialNavBarState)      (createArchiveCheckpointAndClose) $ \navBar ->
    bracket (forkIO (tryRemoveFile (basePath </> "profileData_socket") >> acidServer skipAuthenticationCheck (UnixSocket $ basePath </> "profileData_socket") profileData))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "profileData_socket"))
            (const $ f (Acid auth profile profileData core navBar))
    where
      tryRemoveFile fp = removeFile fp `catch` (\e -> if isDoesNotExistError e then return () else throw e)
      createArchiveCheckpointAndClose acid =
          do createArchive acid
             createCheckpointAndClose acid

