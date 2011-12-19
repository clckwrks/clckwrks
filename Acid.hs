{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Acid where

import Control.Exception           (bracket)
import Data.Acid                   (AcidState)
import Data.Acid.Local             (openLocalStateFrom, createCheckpointAndClose)
import Data.Maybe                  (fromMaybe)
import Menu.Acid                   (MenuState       , initialMenuState)
import Page.Acid                   (PageState       , initialPageState)
import ProfileData.Acid            (ProfileDataState, initialProfileDataState)
import Happstack.Auth.Core.Auth    (AuthState       , initialAuthState)
import Happstack.Auth.Core.Profile (ProfileState    , initialProfileState)
import System.FilePath             ((</>))
import URL

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
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
    bracket (openLocalStateFrom (basePath </> "page")        initialPageState)        (createCheckpointAndClose) $ \page ->
    bracket (openLocalStateFrom (basePath </> "menu")        initialMenuState)        (createCheckpointAndClose) $ \menu ->
        f (Acid auth profile profileData page menu)
