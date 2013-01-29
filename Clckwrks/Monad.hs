{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards, ScopedTypeVariables, UndecidableInstances, OverloadedStrings #-}
module Clckwrks.Monad
    ( Clck
    , ClckPlugins
    , ClckT(..)
    , ClckForm
    , ClckFormT
    , ClckFormError(..)
    , ChildType(..)
    , ClckwrksConfig(..)
    , TLSSettings(..)
    , AttributeType(..)
    , Theme(..)
    , ThemeName
    , themeTemplate
    , calcBaseURI
    , calcTLSBaseURI
    , evalClckT
    , execClckT
    , runClckT
    , mapClckT
    , withRouteClckT
    , ClckState(..)
    , getUserId
    , Content(..)
--    , markupToContent
--    , addPreProcessor
    , addAdminMenu
    , addPreProc
--     , getPrefix
    , getEnableAnalytics
    , getUnique
    , setUnique
    , requiresRole
    , requiresRole_
    , getUserRoles
    , query
    , update
    , nestURL
    , withAbs
    , withAbs'
    , segments
    , transform
    )
where

import Clckwrks.Admin.URL            (AdminURL(..))
import Clckwrks.Acid                 (Acid(..), CoreState, GetAcidState(..))
-- import Clckwrks.Page.Types           (Markup(..), runPreProcessors)
import Clckwrks.Menu.Acid            (MenuState)
-- import Clckwrks.Page.Acid            (PageState, PageId)
import Clckwrks.ProfileData.Acid     (ProfileDataState, ProfileDataError(..), GetRoles(..), HasRole(..))
import Clckwrks.ProfileData.Types    (Role(..))
import Clckwrks.Types                (Prefix, Trust(Trusted))
import Clckwrks.Unauthorized         (unauthorizedPage)
import Clckwrks.URL                  (ClckURL(..))
import Control.Applicative           (Alternative, Applicative, (<$>), (<|>), many)
import Control.Monad                 (MonadPlus, foldM)
import Control.Monad.State           (MonadState, StateT, evalStateT, execStateT, get, mapStateT, modify, put, runStateT)
import Control.Monad.Reader          (MonadReader, ReaderT, mapReaderT)
import Control.Monad.Trans           (MonadIO(liftIO), lift)
import Control.Concurrent.STM        (TVar, readTVar, writeTVar, atomically)
import Data.Acid                     (AcidState, EventState, EventResult, QueryEvent, UpdateEvent)
import Data.Acid.Advanced            (query', update')
import Data.Attoparsec.Text.Lazy     (Parser, parseOnly, char, asciiCI, try, takeWhile, takeWhile1)
import qualified Data.HashMap.Lazy   as HashMap
import qualified Data.List           as List
import qualified Data.Map            as Map
import Data.Monoid                   ((<>), mappend, mconcat)
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import Data.ByteString.Lazy          as LB (ByteString)
import Data.ByteString.Lazy.UTF8     as LB (toString)
import Data.Data                     (Data, Typeable)
import Data.Map                      (Map)
import Data.Maybe                    (fromJust)
import Data.SafeCopy                 (SafeCopy(..))
import Data.Set                      (Set)
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import           Data.Text.Lazy.Builder (Builder, fromText)
import qualified Data.Text.Lazy.Builder as B
import Data.Time.Clock               (UTCTime)
import Data.Time.Format              (formatTime)
import Happstack.Auth                (AuthProfileURL(..), AuthURL(..), AuthState, ProfileState, UserId)
import qualified Happstack.Auth      as Auth

import Happstack.Server              (Happstack, ServerMonad(..), FilterMonad(..), WebMonad(..), Input, Request(..), Response, HasRqData(..), ServerPartT, UnWebT, internalServerError, mapServerPartT, escape, toResponse)
import Happstack.Server.HSP.HTML     () -- ToMessage XML instance
import Happstack.Server.Internal.Monads (FilterFun)
import HSP                           hiding (Request, escape)
import HSP.Google.Analytics          (UACCT)
import HSP.ServerPartT               ()
import HSX.XMLGenerator              (XMLGen(..))
import HSX.JMacro                    (IntegerSupply(..))
import Language.Javascript.JMacro
import Prelude                       hiding (takeWhile)
import System.Locale                 (defaultTimeLocale)
import Text.Blaze.Html               (Html)
import Text.Blaze.Html.Renderer.String    (renderHtml)
import Text.Reform                   (CommonFormError, Form, FormError(..))
import Web.Routes                    (URL, MonadRoute(askRouteFn), RouteT(RouteT, unRouteT), mapRouteT, showURL, withRouteT)
import Web.Plugins.Core              (Plugins, getConfig, getPluginsSt, modifyPluginsSt, getTheme)
import qualified Web.Routes          as R
import Web.Routes.Happstack          (seeOtherURL) -- imported so that instances are scope even though we do not use them here
import Web.Routes.XMLGenT            () -- imported so that instances are scope even though we do not use them here

type ClckPlugins = Plugins Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ([TL.Text -> ClckT ClckURL IO TL.Text])

type ThemeName = T.Text

data Theme = Theme
    { themeName      :: ThemeName
    , _themeTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                        , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body) =>
                        T.Text
                     -> headers
                     -> body
                     -> XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
--    , themeBlog      :: XMLGenT (ClckT ClckURL (ServerPartT IO)) XML
    , themeDataDir   :: IO FilePath
    }


themeTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                 , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body
                 ) =>
                 ClckPlugins
              -> T.Text
              -> headers
              -> body
              -> ClckT ClckURL (ServerPartT IO) Response
themeTemplate plugins ttl hdrs bdy =
    do mTheme <- getTheme plugins
       case mTheme of
         Nothing -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: T.Text)
         (Just theme) -> fmap toResponse $ unXMLGenT $ (_themeTemplate theme ttl hdrs bdy)



data TLSSettings = TLSSettings
    { clckTLSPort :: Int
    , clckTLSCert :: FilePath
    , clckTLSKey  :: FilePath
    }

data ClckwrksConfig = ClckwrksConfig
    { clckHostname        :: String         -- ^ external name of the host
    , clckPort            :: Int            -- ^ port to listen on
    , clckTLS             :: Maybe TLSSettings -- ^ HTTPS
    , clckHidePort        :: Bool           -- ^ hide port number in URL (useful when running behind a reverse proxy)
    , clckJQueryPath      :: FilePath       -- ^ path to @jquery.js@ on disk
    , clckJQueryUIPath    :: FilePath       -- ^ path to @jquery-ui.js@ on disk
    , clckJSTreePath      :: FilePath       -- ^ path to @jstree.js@ on disk
    , clckJSON2Path       :: FilePath       -- ^ path to @JSON2.js@ on disk
    , clckTopDir          :: Maybe FilePath -- ^ path to top-level directory for all acid-state files/file uploads/etc
    , clckEnableAnalytics :: Bool           -- ^ enable google analytics
    , clckInitHook        :: T.Text -> ClckState -> ClckwrksConfig -> IO (ClckState, ClckwrksConfig) -- ^ init hook
    }

-- | calculate the baseURI from the 'clckHostname', 'clckPort' and 'clckHidePort' options
calcBaseURI :: ClckwrksConfig -> T.Text
calcBaseURI c =
    Text.pack $ "http://" ++ (clckHostname c) ++ if ((clckPort c /= 80) && (clckHidePort c == False)) then (':' : show (clckPort c)) else ""

calcTLSBaseURI  :: ClckwrksConfig -> Maybe T.Text
calcTLSBaseURI c =
    case clckTLS c of
      Nothing -> Nothing
      (Just tlsSettings) ->
          Just $ Text.pack $ "https://" ++ (clckHostname c) ++ if ((clckPort c /= 443) && (clckHidePort c == False)) then (':' : show (clckTLSPort tlsSettings)) else ""

data ClckState
    = ClckState { acidState        :: Acid
                , uniqueId         :: TVar Integer -- only unique for this request
                , adminMenus       :: [(T.Text, [(Set Role, T.Text, T.Text)])]
                , enableAnalytics  :: Bool -- ^ enable Google Analytics
                , plugins          :: ClckPlugins
                }

newtype ClckT url m a = ClckT { unClckT :: RouteT url (StateT ClckState m) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, ServerMonad, HasRqData, FilterMonad r, WebMonad r, MonadState ClckState)

instance (Happstack m) => Happstack (ClckT url m)

-- | evaluate a 'ClckT' returning the inner monad
--
-- similar to 'evalStateT'.
evalClckT :: (Monad m) =>
             (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -- ^ function to act as 'showURLParams'
          -> ClckState                                            -- ^ initial 'ClckState'
          -> ClckT url m a                                        -- ^ 'ClckT' to evaluate
          -> m a
evalClckT showFn clckState m = evalStateT (unRouteT (unClckT m) showFn) clckState

-- | execute a 'ClckT' returning the final 'ClckState'
--
-- similar to 'execStateT'.
execClckT :: (Monad m) =>
             (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -- ^ function to act as 'showURLParams'
          -> ClckState                                            -- ^ initial 'ClckState'
          -> ClckT url m a                                        -- ^ 'ClckT' to evaluate
          -> m ClckState
execClckT showFn clckState m =
    execStateT (unRouteT (unClckT m) showFn) clckState

-- | run a 'ClckT'
--
-- similar to 'runStateT'.
runClckT :: (Monad m) =>
            (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -- ^ function to act as 'showURLParams'
         -> ClckState                                            -- ^ initial 'ClckState'
         -> ClckT url m a                                        -- ^ 'ClckT' to evaluate
         -> m (a, ClckState)
runClckT showFn clckState m =
    runStateT (unRouteT (unClckT m) showFn) clckState

-- | map a transformation function over the inner monad
--
-- similar to 'mapStateT'
mapClckT :: (m (a, ClckState) -> n (b, ClckState)) -- ^ transformation function
         -> ClckT url m a                          -- ^ initial monad
         -> ClckT url n b
mapClckT f (ClckT r) = ClckT $ mapRouteT (mapStateT f) r

-- | error returned when a reform 'Form' fails to validate
data ClckFormError
    = ClckCFE (CommonFormError [Input])
    | PDE ProfileDataError
    | EmptyUsername
      deriving (Show)

instance FormError ClckFormError where
    type ErrorInputType ClckFormError = [Input]
    commonFormError = ClckCFE

-- | ClckForm - type for reform forms
type ClckFormT error m = Form m  [Input] error [XMLGenT m XML] ()
type ClckForm url    = Form (ClckT url (ServerPartT IO)) [Input] ClckFormError [XMLGenT (ClckT url (ServerPartT IO)) XML] ()

setUnique :: (Functor m, MonadIO m) => Integer -> ClckT url m ()
setUnique i =
    do u <- uniqueId <$> get
       liftIO $ atomically $ writeTVar u i

-- | get a unique 'Integer'.
--
-- Only unique for the current request
getUnique :: (Functor m, MonadIO m) => ClckT url m Integer
getUnique =
    do u <- uniqueId <$> get
       liftIO $ atomically $ do i <- readTVar u
                                writeTVar u (succ i)
                                return i

-- | get the 'Bool' value indicating if Google Analytics should be enabled or not
getEnableAnalytics :: (Functor m, MonadState ClckState m) => m Bool
getEnableAnalytics = enableAnalytics <$> get

addAdminMenu :: (Monad m) => (T.Text, [(Set Role, T.Text, T.Text)]) -> ClckT url m ()
addAdminMenu (category, entries) =
    modify $ \cs ->
        let oldMenus = adminMenus cs
            newMenus = Map.toAscList $ Map.insertWith List.union category entries $ Map.fromList oldMenus
        in cs { adminMenus = newMenus }

-- | change the route url
withRouteClckT :: ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> url -> [(T.Text, Maybe T.Text)] -> T.Text)
               -> ClckT url  m a
               -> ClckT url' m a
withRouteClckT f (ClckT routeT) = (ClckT $ withRouteT f routeT)

type Clck url = ClckT url (ServerPartT IO)

instance (Functor m, MonadIO m) => IntegerSupply (ClckT url m) where
    nextInteger = getUnique

instance ToJExpr Text.Text where
  toJExpr t = ValExpr $ JStr $ T.unpack t

nestURL :: (url1 -> url2) -> ClckT url1 m a -> ClckT url2 m a
nestURL f (ClckT r) = ClckT $ R.nestURL f r

withAbs :: (Happstack m) => ClckT url m a -> ClckT url m a
withAbs m =
    do secure <- rqSecure <$> askRq
       clckState <- get
       cc <- getConfig (plugins clckState)
       let base = if secure then (fromJust $ calcTLSBaseURI cc) else calcBaseURI cc
       withAbs' base m

withAbs' :: T.Text
      -> ClckT url m a
      -> ClckT url m a
withAbs' prefix (ClckT (RouteT r)) =
    ClckT $ RouteT $ \showFn ->
        r (\url params -> prefix <> showFn url params)

instance (Monad m) => MonadRoute (ClckT url m) where
    type URL (ClckT url m) = url
    askRouteFn = ClckT $ askRouteFn

-- | similar to the normal acid-state 'query' except it automatically gets the correct 'AcidState' handle from the environment
query :: forall event m. (QueryEvent event, GetAcidState m (EventState event), Functor m, MonadIO m, MonadState ClckState m) => event -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

-- | similar to the normal acid-state 'update' except it automatically gets the correct 'AcidState' handle from the environment
update :: forall event m. (UpdateEvent event, GetAcidState m (EventState event), Functor m, MonadIO m, MonadState ClckState m) => event -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event

instance (GetAcidState m st) => GetAcidState (XMLGenT m) st where
    getAcidState = XMLGenT getAcidState

instance (Functor m, Monad m) => GetAcidState (ClckT url m) AuthState where
    getAcidState = (acidAuth . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) ProfileState where
    getAcidState = (acidProfile . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) CoreState where
    getAcidState = (acidCore . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) (MenuState ClckURL) where
    getAcidState = (acidMenu . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) ProfileDataState where
    getAcidState = (acidProfileData . acidState) <$> get

-- | The the 'UserId' of the current user. While return 'Nothing' if they are not logged in.
getUserId :: (Happstack m, GetAcidState m AuthState, GetAcidState m ProfileState) => m (Maybe UserId)
getUserId =
    do authState    <- getAcidState
       profileState <- getAcidState
       Auth.getUserId authState profileState

-- * XMLGen / XMLGenerator instances for Clck

instance (Functor m, Monad m) => XMLGen (ClckT url m) where
    type XMLType          (ClckT url m) = XML
    newtype ChildType     (ClckT url m) = ClckChild { unClckChild :: XML }
    newtype AttributeType (ClckT url m) = ClckAttr { unClckAttr :: Attribute }
    genElement n attrs children =
        do attribs <- map unClckAttr <$> asAttr attrs
           childer <- flattenCDATA . map (unClckChild) <$> asChild children
           XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = ClckChild
    pcdataToChild = xmlToChild . pcdata

flattenCDATA :: [XML] -> [XML]
flattenCDATA cxml =
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs
    where
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1++s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)

instance (Functor m, Monad m) => IsAttrValue (ClckT url m) T.Text where
    toAttrValue = toAttrValue . T.unpack

instance (Functor m, Monad m) => IsAttrValue (ClckT url m) TL.Text where
    toAttrValue = toAttrValue . TL.unpack

instance (Functor m, Monad m) => EmbedAsAttr (ClckT url m) Attribute where
    asAttr = return . (:[]) . ClckAttr

instance (Functor m, Monad m, IsName n) => EmbedAsAttr (ClckT url m) (Attr n String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Functor m, Monad m, IsName n) => EmbedAsAttr (ClckT url m) (Attr n Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Functor m, Monad m, IsName n) => EmbedAsAttr (ClckT url m) (Attr n Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Functor m, Monad m, IsName n) => EmbedAsAttr (ClckT url m) (Attr n Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Functor m, Monad m, IsName n) => EmbedAsAttr (ClckT url m) (Attr n Integer) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (IsName n) => EmbedAsAttr (Clck ClckURL) (Attr n ClckURL) where
    asAttr (n := u) =
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))

instance (IsName n) => EmbedAsAttr (Clck AdminURL) (Attr n AdminURL) where
    asAttr (n := u) =
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))

instance (Functor m, Monad m, IsName n) => (EmbedAsAttr (ClckT url m) (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.unpack a)

instance (Functor m, Monad m, IsName n) => (EmbedAsAttr (ClckT url m) (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ T.unpack a)

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Char where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . (:[])

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) String where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Int where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Integer where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Double where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Float where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) TL.Text where
    asChild = asChild . TL.unpack

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) T.Text where
    asChild = asChild . T.unpack

instance (EmbedAsChild (ClckT url1 m) a, url1 ~ url2) => EmbedAsChild (ClckT url1 m) (ClckT url2 m a) where
    asChild c =
        do a <- XMLGenT c
           asChild a

instance (Functor m, MonadIO m, EmbedAsChild (ClckT url m) a) => EmbedAsChild (ClckT url m) (IO a) where
    asChild c =
        do a <- XMLGenT (liftIO c)
           asChild a

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) XML where
    asChild = XMLGenT . return . (:[]) . ClckChild

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Html where
    asChild = XMLGenT . return . (:[]) . ClckChild . cdata . renderHtml

instance (Functor m, MonadIO m, Happstack m) => EmbedAsChild (ClckT url m) ClckFormError where
    asChild formError = asChild (show formError)

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) () where
    asChild () = return []

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) UTCTime where
    asChild = asChild . formatTime defaultTimeLocale "%a, %F @ %r"

instance (Functor m, Monad m, EmbedAsChild (ClckT url m) a) => EmbedAsChild (ClckT url m) (Maybe a) where
    asChild Nothing = asChild ()
    asChild (Just a) = asChild a

instance (Functor m, Monad m) => AppendChild (ClckT url m) XML where
 appAll xml children = do
        chs <- children
        case xml of

         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unClckChild chs))

instance (Functor m, Monad m) => SetAttr (ClckT url m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unClckAttr attrs)) cs

instance (Functor m, Monad m) => XMLGenerator (ClckT url m)

-- | a wrapper which identifies how to treat different 'Text' values when attempting to embed them.
--
-- In general 'Content' values have already been
-- flatten/preprocessed/etc and are now basic formats like
-- @text/plain@, @text/html@, etc
data Content
    = TrustedHtml T.Text
    | PlainText   T.Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Content where
    asChild (TrustedHtml html) = asChild $ cdata (T.unpack html)
    asChild (PlainText txt)    = asChild $ pcdata (T.unpack txt)

addPreProc :: (MonadIO m) =>
              Plugins theme n hook config [TL.Text -> ClckT ClckURL IO TL.Text]
           -> (TL.Text -> ClckT ClckURL IO TL.Text)
           -> m ()
addPreProc plugins p =
    modifyPluginsSt plugins $ \ps -> p : ps

-- * Preprocess

data Segment cmd
    = TextBlock T.Text
    | Cmd cmd
      deriving Show

instance Functor Segment where
    fmap f (TextBlock t) = TextBlock t
    fmap f (Cmd c)       = Cmd (f c)

transform :: (Monad m) =>
             (cmd -> m Builder)
          -> [Segment cmd]
          -> m Builder
transform f segments =
    do bs <- mapM (transformSegment f) segments
       return (mconcat bs)

transformSegment :: (Monad m) =>
                    (cmd -> m Builder)
                 -> Segment cmd
                 -> m Builder
transformSegment f (TextBlock t) = return (B.fromText t)
transformSegment f (Cmd cmd) = f cmd

segments :: T.Text
         -> Parser a
         -> Parser [Segment a]
segments name p =
    many (cmd name p <|> plainText)

cmd :: T.Text -> Parser cmd -> Parser (Segment cmd)
cmd n p =
    do char '{'
       ((try $ do asciiCI n
                  char '|'
                  r <- p
                  char '}'
                  return (Cmd r))
             <|>
             (do t <- takeWhile1 (/= '{')
                 return $ TextBlock (T.cons '{' t)))

plainText :: Parser (Segment cmd)
plainText =
    do t <- takeWhile1 (/= '{')
       return $ TextBlock t

-- * Require Role

requiresRole_ :: (Happstack  m) => (ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text) -> Set Role -> url -> ClckT u m url
requiresRole_ showFn role url =
    ClckT $ RouteT $ \_ -> unRouteT (unClckT (requiresRole role url)) showFn

requiresRole :: (Happstack m) => Set Role -> url -> ClckT ClckURL m url
requiresRole role url =
    do mu <- getUserId
       case mu of
         Nothing -> escape $ seeOtherURL (Auth $ AuthURL A_Login)
         (Just uid) ->
             do r <- query (HasRole uid role)
                if r
                   then return url
                   else escape $ unauthorizedPage ("You do not have permission to view this page." :: T.Text)

getUserRoles :: (Happstack m, MonadIO m) => ClckT u m (Set Role)
getUserRoles =
    do mu <- getUserId
       case mu of
         Nothing -> return (Set.singleton Visitor)
         (Just u) ->
             do mr <- query (GetRoles u)
                case mr of
                  Nothing  -> return Set.empty
                  (Just r) -> return r