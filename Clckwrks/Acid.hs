{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Clckwrks.Acid where

import Clckwrks.Menu.Acid          (MenuState       , initialMenuState)
import Clckwrks.Page.Acid          (PageState       , initialPageState)
import Clckwrks.ProfileData.Acid   (ProfileDataState, initialProfileDataState)
import Clckwrks.URL                (ClckURL)
import Control.Exception           (bracket, catch, throw)
import Control.Concurrent          (killThread, forkIO)
import Data.Acid                   (AcidState)
import Data.Acid.Local             (openLocalStateFrom, createArchive, createCheckpointAndClose)
import Data.Acid.Remote            (acidServer)
import Data.Maybe                  (fromMaybe)
import Happstack.Auth.Core.Auth    (AuthState       , initialAuthState)
import Happstack.Auth.Core.Profile (ProfileState    , initialProfileState)
import Network                     (PortID(UnixSocket))
import Prelude                     hiding (catch)
import System.Directory            (removeFile)
import System.FilePath             ((</>))
import System.IO.Error             (isDoesNotExistError)

data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    , acidProfileData :: AcidState ProfileDataState
    , acidPage        :: AcidState PageState
    , acidMenu        :: AcidState (MenuState ClckURL)
    }

class GetAcidState m st where
    getAcidState :: m (AcidState st)

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    initialPageState >>= \ips ->
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createArchiveCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createArchiveCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createArchiveCheckpointAndClose) $ \profileData ->
    bracket (openLocalStateFrom (basePath </> "page")        ips)                     (createArchiveCheckpointAndClose) $ \page ->
    bracket (openLocalStateFrom (basePath </> "menu")        initialMenuState)        (createArchiveCheckpointAndClose) $ \menu ->
    bracket (forkIO (tryRemoveFile (basePath </> "profileData_socket") >> acidServer profileData (UnixSocket $ basePath </> "profileData_socket")))
            (\tid -> killThread tid >> tryRemoveFile (basePath </> "profileData_socket"))
            (const $ f (Acid auth profile profileData page menu))
    where
      tryRemoveFile fp = removeFile fp `catch` (\e -> if isDoesNotExistError e then return () else throw e)
      createArchiveCheckpointAndClose acid =
          do createArchive acid
             createCheckpointAndClose acid

