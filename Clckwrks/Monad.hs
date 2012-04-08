{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards, ScopedTypeVariables, UndecidableInstances, OverloadedStrings #-}
module Clckwrks.Monad
    ( Clck
    , ClckT(..)
    , evalClckT
    , execClckT
    , runClckT
    , mapClckT
    , withRouteClckT
    , ClckState(..)
    , getUserId
    , Content(..)
    , markupToContent
    , addPreProcessor
    , addAdminMenu
    , addPluginPath
    , setCurrentPage
    , getPrefix
    , getUACCT
    , getUnique
    , setUnique
    , requiresRole
    , requiresRole_
    , query
    , update
    , nestURL
    )
where

import Clckwrks.Admin.URL            (AdminURL(..))
import Clckwrks.Acid                 (Acid(..), GetAcidState(..))
import Clckwrks.Page.Types           (Markup(..), runPreProcessors)
import Clckwrks.Menu.Acid            (MenuState)
import Clckwrks.Page.Acid            (PageState, PageId)
import Clckwrks.ProfileData.Acid     (ProfileDataState, HasRole(..))
import Clckwrks.ProfileData.Types    (Role(..))
import Clckwrks.Types                (Prefix, Trust(Trusted))
import Clckwrks.Unauthorized         (unauthorizedPage)
import Clckwrks.URL                  (ClckURL(..))
import Control.Applicative           (Alternative, Applicative, (<$>), (<|>), many)
import Control.Monad                 (MonadPlus)
import Control.Monad.State           (MonadState, StateT, evalStateT, execStateT, get, mapStateT, modify, put, runStateT)
import Control.Monad.Reader          (MonadReader, ReaderT, mapReaderT)
import Control.Monad.Trans           (MonadIO(liftIO), lift)
import Control.Concurrent.STM        (TVar, readTVar, writeTVar, atomically)
import Data.Aeson                    (Value(..))
import Data.Acid                     (AcidState, EventState, EventResult, QueryEvent, UpdateEvent)
import Data.Acid.Advanced            (query', update')
import Data.Attoparsec.Text          (Parser, parseOnly, char, try, takeWhile, takeWhile1)
import qualified Data.HashMap.Lazy   as HashMap
import qualified Data.List           as List
import qualified Data.Map            as Map
import Data.Monoid                   (mappend, mconcat)
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import Data.ByteString.Lazy          as LB (ByteString)
import Data.ByteString.Lazy.UTF8     as LB (toString)
import Data.Data                     (Data, Typeable)
import Data.Map                      (Map)
import Data.SafeCopy                 (SafeCopy(..))
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.Time.Clock               (UTCTime)
import Data.Time.Format              (formatTime)
import Happstack.Auth                (AuthProfileURL(..), AuthURL(..), AuthState, ProfileState, UserId)
import qualified Happstack.Auth      as Auth

import Happstack.Server              (Happstack, ServerMonad(..), FilterMonad(..), WebMonad(..), Response, HasRqData(..), ServerPartT, UnWebT, mapServerPartT, escape)
import Happstack.Server.HSP.HTML     () -- ToMessage XML instance
import Happstack.Server.Internal.Monads (FilterFun)
import HSP                           hiding (Request, escape)
import HSP.Google.Analytics          (UACCT)
import HSP.ServerPartT               ()
import qualified HSX.XMLGenerator    as HSX
import HSX.JMacro                    (IntegerSupply(..))
import Language.Javascript.JMacro
import Prelude                       hiding (takeWhile)
import System.Locale                 (defaultTimeLocale)
import Text.Blaze                    (Html)
import Text.Blaze.Renderer.String    (renderHtml)
import Web.Routes                    (URL, MonadRoute(askRouteFn), RouteT(RouteT, unRouteT), mapRouteT, showURL, withRouteT)
import qualified Web.Routes          as R
import Web.Routes.Happstack          (seeOtherURL) -- imported so that instances are scope even though we do not use them here
import Web.Routes.XMLGenT            () -- imported so that instances are scope even though we do not use them here

data ClckState
    = ClckState { acidState        :: Acid
                , currentPage      :: PageId
                , themePath        :: FilePath
                , pluginPath       :: Map T.Text FilePath
                , componentPrefix  :: Prefix
                , uniqueId         :: TVar Integer -- only unique for this request
                , preProcessorCmds :: forall m url. (Functor m, MonadIO m, Happstack m) => Map T.Text (T.Text -> ClckT url m Builder) -- TODO: should this be a TVar?
                , adminMenus       :: [(T.Text, [(T.Text, T.Text)])]
                , uacct            :: Maybe UACCT
                }

newtype ClckT url m a = ClckT { unClckT :: RouteT url (StateT ClckState m) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, ServerMonad, HasRqData, FilterMonad r, WebMonad r, MonadState ClckState)

instance (Happstack m) => Happstack (ClckT url m)

evalClckT :: (Monad m) => (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> ClckState -> ClckT url m a -> m a
evalClckT showFn clckState m = evalStateT (unRouteT (unClckT m) showFn) clckState

execClckT :: (Monad m) => (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> ClckState -> ClckT url m a -> m ClckState
execClckT showFn clckState m = execStateT (unRouteT (unClckT m) showFn) clckState


runClckT :: (Monad m) => (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> ClckState -> ClckT url m a -> m (a, ClckState)
runClckT showFn clckState m = runStateT (unRouteT (unClckT m) showFn) clckState

-- | update the 'currentPage' field of 'ClckState'
setCurrentPage :: PageId -> Clck url ()
setCurrentPage pid =
    modify $ \s -> s { currentPage = pid }

getPrefix :: Clck url Prefix
getPrefix = componentPrefix <$> get

setUnique :: Integer -> Clck url ()
setUnique i =
    do u <- uniqueId <$> get
       liftIO $ atomically $ writeTVar u i

-- | get a unique 'Integer'.
--
-- Only unique for the current request
getUnique :: Clck url Integer
getUnique =
    do u <- uniqueId <$> get
       liftIO $ atomically $ do i <- readTVar u
                                writeTVar u (succ i)
                                return i

-- | get the google tracking code ('UACCT') associated with this site
getUACCT :: (Functor m, MonadState ClckState m) => m (Maybe UACCT)
getUACCT = uacct <$> get

addPreProcessor :: (Monad n) => T.Text -> (forall url m. (Functor m, MonadIO m, Happstack m) => T.Text -> ClckT url m Builder) -> ClckT u n ()
addPreProcessor name action =
    modify $ \cs ->
        cs { preProcessorCmds = Map.insert name action (preProcessorCmds cs) }

addAdminMenu :: (Monad m) => (T.Text, [(T.Text, T.Text)]) -> ClckT url m ()
addAdminMenu (category, entries) =
    modify $ \cs ->
        let oldMenus = adminMenus cs
            newMenus = Map.toAscList $ Map.insertWith List.union category entries $ Map.fromList oldMenus
        in cs { adminMenus = newMenus }

addPluginPath :: (Monad m) => T.Text -> FilePath -> ClckT url m ()
addPluginPath plugin fp =
    modify $ \cs  ->
        cs { pluginPath = Map.insert plugin fp (pluginPath cs) }

mapClckT :: (m (a, ClckState) -> n (b, ClckState))
         -> ClckT url m a
         -> ClckT url n b
mapClckT f (ClckT r) = ClckT $ mapRouteT (mapStateT f) r

-- | change the route url
withRouteClckT :: ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> url -> [(T.Text, Maybe T.Text)] -> T.Text)
               -> ClckT url  m a
               -> ClckT url' m a
withRouteClckT f (ClckT routeT) = (ClckT $ withRouteT f routeT)

type Clck url = ClckT url (ServerPartT IO)

instance IntegerSupply (Clck url) where
    nextInteger = getUnique

instance ToJExpr Value where
    toJExpr (Object obj)  = ValExpr $ JHash   $ Map.fromList $ map (\(k,v) -> (T.unpack k, toJExpr v)) (HashMap.toList obj)
    toJExpr (Array vs)    = ValExpr $ JList   $ map toJExpr (Vector.toList vs)
    toJExpr (String s)    = ValExpr $ JStr    $ T.unpack s
    toJExpr (Number n)    = ValExpr $ JDouble $ realToFrac n
    toJExpr (Bool True)   = ValExpr $ JVar    $ StrI "true"
    toJExpr (Bool False)  = ValExpr $ JVar    $ StrI "false"
    toJExpr Null          = ValExpr $ JVar    $ StrI "null"

instance ToJExpr Text.Text where
  toJExpr t = ValExpr $ JStr $ T.unpack t

nestURL :: (url1 -> url2) -> ClckT url1 m a -> ClckT url2 m a
nestURL f (ClckT r) = ClckT $ R.nestURL f r

instance (Monad m) => MonadRoute (ClckT url m) where
    type URL (ClckT url m) = url
    askRouteFn = ClckT $ askRouteFn

query :: forall event m. (QueryEvent event, GetAcidState m (EventState event), Functor m, MonadIO m, MonadState ClckState m) => event -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

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

instance (Functor m, Monad m) => GetAcidState (ClckT url m) (MenuState ClckURL) where
    getAcidState = (acidMenu . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) PageState where
    getAcidState = (acidPage . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) ProfileDataState where
    getAcidState = (acidProfileData . acidState) <$> get

getUserId :: (Happstack m, GetAcidState m AuthState, GetAcidState m ProfileState) => m (Maybe UserId)
getUserId =
    do authState    <- getAcidState
       profileState <- getAcidState
       Auth.getUserId authState profileState

-- * XMLGen / XMLGenerator instances for Clck

instance (Functor m, Monad m) => HSX.XMLGen (ClckT url m) where
    type XML (ClckT url m) = XML
    newtype Child (ClckT url m) = ClckChild { unClckChild :: XML }
    newtype Attribute (ClckT url m) = FAttr { unFAttr :: Attribute }
    genElement n attrs children =
        do attribs <- map unFAttr <$> asAttr attrs
           childer <- flattenCDATA . map (unClckChild) <$> asChild children
           XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = ClckChild
    pcdataToChild = HSX.xmlToChild . pcdata

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

instance (Functor m, Monad m) => HSX.EmbedAsAttr (ClckT url m) Attribute where
    asAttr = return . (:[]) . FAttr

instance (Functor m, Monad m, IsName n) => HSX.EmbedAsAttr (ClckT url m) (Attr n String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Functor m, Monad m, IsName n) => HSX.EmbedAsAttr (ClckT url m) (Attr n Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Functor m, Monad m, IsName n) => HSX.EmbedAsAttr (ClckT url m) (Attr n Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Functor m, Monad m, IsName n) => HSX.EmbedAsAttr (ClckT url m) (Attr n Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Functor m, Monad m, IsName n) => HSX.EmbedAsAttr (ClckT url m) (Attr n Integer) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (IsName n) => HSX.EmbedAsAttr (Clck ClckURL) (Attr n ClckURL) where
    asAttr (n := u) =
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))

instance (IsName n) => HSX.EmbedAsAttr (Clck AdminURL) (Attr n AdminURL) where
    asAttr (n := u) =
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))


{-
instance HSX.EmbedAsAttr Clck (Attr String AuthURL) where
    asAttr (n := u) =
        do url <- showURL (W_Auth u)
           asAttr $ MkAttr (toName n, pAttrVal url)
-}

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

{-
instance EmbedAsChild Clck TextHtml where
    asChild = XMLGenT . return . (:[]) . ClckChild . cdata . T.unpack . unTextHtml
-}
instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) XML where
    asChild = XMLGenT . return . (:[]) . ClckChild

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Html where
    asChild = XMLGenT . return . (:[]) . ClckChild . cdata . renderHtml

instance (Functor m, MonadIO m, Happstack m) => EmbedAsChild (ClckT url m) Markup where
    asChild mrkup = asChild =<< (XMLGenT $ markupToContent mrkup)

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
         Element n as cs -> return $ Element n (foldr (:) as (map unFAttr attrs)) cs

instance (Functor m, Monad m) => XMLGenerator (ClckT url m)

data Content
    = TrustedHtml T.Text
    | PlainText   T.Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) Content where
    asChild (TrustedHtml html) = asChild $ cdata (T.unpack html)
    asChild (PlainText txt)    = asChild $ pcdata (T.unpack txt)

markupToContent :: (Functor m, MonadIO m, Happstack m) => Markup -> ClckT url m Content
markupToContent Markup{..} =
    do clckState <- get
       markup' <- process (preProcessorCmds clckState) markup
       e <- liftIO $ runPreProcessors preProcessors trust markup'
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

-- * Preprocess

data Segment
    = TextBlock T.Text
    | Command T.Text T.Text
      deriving (Show)

segment :: Parser Segment
segment =
    tryCommand <|>
    do t <- takeWhile1 (/= '{')
       return (TextBlock t)

tryCommand :: Parser Segment
tryCommand =
    try $ do char '{'
             cmd <- takeWhile1 (\c -> notElem c "|}")
             char '|'
             arg <- takeWhile (/= '}')
             char '}'
             return (Command cmd arg)

segments :: Parser [Segment]
segments = many segment

processSegment :: (MonadIO m) => Map T.Text (T.Text -> m Builder) -> Segment -> m Builder
processSegment _ (TextBlock txt) = return $ B.fromText txt
processSegment handlers (Command name txt) =
    case Map.lookup name handlers of
      Nothing -> return $ "<span class='preprocessor-error'>Invalid processor name: " `mappend`
                             (B.fromText name) `mappend`
                          "</span>"
      (Just cmd) ->
          cmd txt

processSegments :: (Functor m, MonadIO m) => Map T.Text (T.Text -> m Builder) -> [Segment] -> m Builder
processSegments handlers segments =
    mconcat <$> mapM (processSegment handlers) segments

process :: (Functor m, MonadIO m) => Map T.Text (T.Text -> m Builder) -> T.Text -> m T.Text
process handlers txt =
    case parseOnly segments txt of
      Left e -> error e
      Right segs ->
          (TL.toStrict . B.toLazyText) <$> processSegments handlers segs

requiresRole_ :: (Happstack  m) => (ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text) -> Role -> url -> ClckT u m url
requiresRole_ showFn role url =
    ClckT $ RouteT $ \_ -> unRouteT (unClckT (requiresRole role url)) showFn

requiresRole :: (Happstack m) => Role -> url -> ClckT ClckURL m url
requiresRole role url =
    do mu <- getUserId
       case mu of
         Nothing -> escape $ seeOtherURL (Auth $ AuthURL A_Login)
         (Just uid) ->
             do r <- query (HasRole uid role)
                if r
                   then return url
                   else escape $ unauthorizedPage ("You do not have permission to view this page." :: T.Text)
