{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RecordWildCards, ScopedTypeVariables #-}
module Clckwrks.Monad
    ( Clck
    , ClckT(..)
    , mapClckT
    , ClckState(..)
    , Content(..)
    , markupToContent
    , setCurrentPage
    , getPrefix
    , getUnique
    , setUnique
    , query
    , update
    , nestURL
    ) where

import Clckwrks.Admin.URL            (AdminURL(..))
import Clckwrks.Acid                 (Acid(..), GetAcidState(..))
import Clckwrks.Page.Types           (Markup(..), runPreProcessors)
import Clckwrks.Menu.Acid            (MenuState)
import Clckwrks.Page.Acid            (PageState, PageId)
import Clckwrks.ProfileData.Acid     (ProfileDataState)
import Clckwrks.Types                (Prefix)
import Clckwrks.URL                  (ClckURL(..))
import Control.Applicative           (Alternative, Applicative, (<$>))
import Control.Monad                 (MonadPlus)
import Control.Monad.State           (MonadState, StateT, get, mapStateT, modify, put)
import Control.Monad.Trans           (MonadIO(liftIO))
import Data.Aeson                    (Value(..))
import Data.Acid                     (AcidState, EventState, EventResult, QueryEvent, UpdateEvent)
import Data.Acid.Advanced            (query', update')
import qualified Data.HashMap.Lazy   as HashMap
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import Data.ByteString.Lazy          as LB (ByteString)
import Data.ByteString.Lazy.UTF8     as LB (toString)
import Data.Data                     (Data, Typeable)
import Data.SafeCopy                 (SafeCopy(..))
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Data.Time.Clock               (UTCTime)
import Data.Time.Format              (formatTime)
import HSP                           hiding (Request, escape)
import HSP.ServerPartT               ()
import qualified HSX.XMLGenerator    as HSX
import Happstack.Server              (Happstack, ServerMonad, FilterMonad, WebMonad, Response, HasRqData, ServerPartT, UnWebT, mapServerPartT)
import Happstack.Server.Internal.Monads (FilterFun)
import HSX.JMacro                    (IntegerSupply(..))
import Language.Javascript.JMacro    
import System.Locale                 (defaultTimeLocale)
import Text.Blaze                    (Html)
import Text.Blaze.Renderer.String    (renderHtml)
import Web.Routes                    hiding (nestURL)
import qualified Web.Routes          as R
import Web.Routes.Happstack          () -- imported so that instances are scope even though we do not use them here
import Web.Routes.XMLGenT            () -- imported so that instances are scope even though we do not use them here

data ClckState 
    = ClckState { acidState       :: Acid 
                , currentPage     :: PageId
                , themePath       :: FilePath
                , componentPrefix :: Prefix
                , uniqueId        :: Integer
                }

newtype ClckT url m a = ClckT { unClck :: RouteT url (ServerPartT (StateT ClckState m)) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, Happstack, ServerMonad, HasRqData, FilterMonad Response, WebMonad Response, MonadState ClckState)

mapClckT :: (m (Maybe (Either Response a, FilterFun Response), ClckState) -> n (Maybe (Either Response b, FilterFun Response), ClckState))
         -> ClckT url m a
         -> ClckT url n b
mapClckT f (ClckT r) = ClckT $ mapRouteT (mapServerPartT (mapStateT f)) r

type Clck url = ClckT url IO

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

instance (Functor m, Monad m) => GetAcidState (ClckT url m) (MenuState ClckURL) where
    getAcidState = (acidMenu . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) PageState where
    getAcidState = (acidPage . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) ProfileDataState where
    getAcidState = (acidProfileData . acidState) <$> get


-- | update the 'currentPage' field of 'ClckState'
setCurrentPage :: PageId -> Clck url ()
setCurrentPage pid =
    modify $ \s -> s { currentPage = pid }

getPrefix :: Clck url Prefix
getPrefix = componentPrefix <$> get

setUnique :: Integer -> Clck url ()
setUnique i =
    modify $ \s -> s { uniqueId = i }

getUnique :: Clck url Integer
getUnique = 
    do s <- get
       let u = uniqueId s
       put $ s { uniqueId = succ u }
       return u

-- * XMLGen / XMLGenerator instances for Clck

instance (Functor m, Monad m) => HSX.XMLGen (ClckT url m) where
    type HSX.XML (ClckT url m) = XML
    newtype HSX.Child (ClckT url m) = ClckChild { unClckChild :: XML }
    newtype HSX.Attribute (ClckT url m) = FAttr { unFAttr :: Attribute }
    genElement n attrs children =
        do attribs <- map unFAttr <$> asAttr attrs
           childer <- flattenCDATA . map (unClckChild) <$> asChild children
           HSX.XMLGenT $ return (Element
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
{-
instance EmbedAsChild Clck (Block t) where
  asChild b = asChild $
    <script type="text/javascript">
      <% show b %>
    </script>

instance IsAttrValue Clck (HJScript (Exp t)) where
  toAttrValue script = toAttrValue $ evaluateHJScript script

instance IsAttrValue Clck (Block t) where
  toAttrValue block = return . attrVal $ "javascript:" ++ show block

instance (IsName n) => HSX.EmbedAsAttr Clck (Attr n (HJScript (Exp a))) where
    asAttr (n := script) = return . (:[]) . FAttr $ MkAttr (toName n, attrVal $ show $ evaluateHJScript script)
-}
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

instance (Functor m, MonadIO m) => EmbedAsChild (ClckT url m) Markup where
    asChild mrkup = asChild =<< markupToContent mrkup

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) () where
    asChild () = return []

instance (Functor m, Monad m) => EmbedAsChild (ClckT url m) UTCTime where
    asChild = asChild . formatTime defaultTimeLocale "%a, %F @ %r"

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

markupToContent :: (MonadIO m) => Markup -> m Content
markupToContent Markup{..} =
    do e <- runPreProcessors preProcessors markup
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

