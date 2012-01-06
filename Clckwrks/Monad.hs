{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards, ScopedTypeVariables, UndecidableInstances, OverloadedStrings #-}
module Clckwrks.Monad
    ( Clck
    , ClckT(..)
    , runClckT
    , mapClckT
    , ClckState(..)
    , Content(..)
    , markupToContent
    , addPreProcessor
    , setCurrentPage
    , getPrefix
    , getUnique
    , setUnique
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
import Clckwrks.ProfileData.Acid     (ProfileDataState)
import Clckwrks.Types                (Prefix)
import Clckwrks.URL                  (ClckURL(..))
import Control.Applicative           (Alternative, Applicative, (<$>), (<|>), many)
import Control.Monad                 (MonadPlus)
import Control.Monad.State           (MonadState, StateT, evalStateT, get, mapStateT, modify, put)
import Control.Monad.Reader          (MonadReader, ReaderT, mapReaderT)
import Control.Monad.Trans           (MonadIO(liftIO), lift)
import Control.Concurrent.STM        (TVar, readTVar, writeTVar, atomically)
import Data.Aeson                    (Value(..))
import Data.Acid                     (AcidState, EventState, EventResult, QueryEvent, UpdateEvent)
import Data.Acid.Advanced            (query', update')
import Data.Attoparsec.Text          (Parser, parseOnly, char, try, takeWhile, takeWhile1)
import qualified Data.HashMap.Lazy   as HashMap
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
import HSP                           hiding (Request, escape)
import HSP.ServerPartT               ()
import qualified HSX.XMLGenerator    as HSX
import Happstack.Server              (Happstack, ServerMonad(..), FilterMonad(..), WebMonad(..), Response, HasRqData(..), ServerPartT, UnWebT, mapServerPartT)
import Happstack.Server.Internal.Monads (FilterFun)
import HSX.JMacro                    (IntegerSupply(..))
import Language.Javascript.JMacro    
import Prelude                       hiding (takeWhile)
import System.Locale                 (defaultTimeLocale)
import Text.Blaze                    (Html)
import Text.Blaze.Renderer.String    (renderHtml)
import Web.Routes                    (URL, MonadRoute(askRouteFn), RouteT(unRouteT), mapRouteT, showURL)
import qualified Web.Routes          as R
import Web.Routes.Happstack          () -- imported so that instances are scope even though we do not use them here
import Web.Routes.XMLGenT            () -- imported so that instances are scope even though we do not use them here

data ClckState 
    = ClckState { acidState        :: Acid 
                , currentPage      :: PageId
                , themePath        :: FilePath
                , componentPrefix  :: Prefix
                , uniqueId         :: TVar Integer -- only unique for this request
                , preProcessorCmds :: forall m url. (Functor m, MonadIO m) => Map T.Text (T.Text -> ClckT url m Builder) -- TODO: should this be a TVar?
                }

-- TODO: move into happstack-server
instance (ServerMonad m) => ServerMonad (StateT s m) where
    askRq   = lift askRq
    localRq f = mapStateT (localRq f)

instance (Monad m, HasRqData m) => HasRqData (StateT s m) where
    askRqEnv = lift askRqEnv
    localRqEnv f = mapStateT (localRqEnv f)
    rqDataError e = lift (rqDataError e)

instance (FilterMonad r m) => FilterMonad r (StateT s m) where
    setFilter f = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter   m = mapStateT (\m' -> 
                                   do ((b,s), f) <- getFilter m'
                                      return ((b, f) ,s)) m

instance (WebMonad a m) => WebMonad a (StateT s m) where
    finishWith = lift . finishWith

instance (Happstack m) => Happstack (StateT s m)


instance (ServerMonad m) => ServerMonad (ReaderT s m) where
    askRq   = lift askRq
    localRq f = mapReaderT (localRq f)

instance (Monad m, HasRqData m) => HasRqData (ReaderT s m) where
    askRqEnv = lift askRqEnv
    localRqEnv f = mapReaderT (localRqEnv f)
    rqDataError e = lift (rqDataError e)

instance (FilterMonad r m) => FilterMonad r (ReaderT s m) where
    setFilter f = lift $ setFilter f
    composeFilter = lift . composeFilter
    getFilter     = mapReaderT getFilter

instance (WebMonad a m) => WebMonad a (ReaderT s m) where
    finishWith = lift . finishWith

instance (Happstack m) => Happstack (ReaderT s m)

newtype ClckT url m a = ClckT { unClckT :: RouteT url (StateT ClckState m) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, ServerMonad, HasRqData, FilterMonad r, WebMonad r, MonadState ClckState)

instance (Happstack m) => Happstack (ClckT url m)

runClckT :: (Monad m) => (url -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> ClckState -> ClckT url m a -> m a
runClckT showFn clckState m = evalStateT (unRouteT (unClckT m) showFn) clckState

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

getUnique :: Clck url Integer
getUnique = 
    do u <- uniqueId <$> get
       liftIO $ atomically $ do i <- readTVar u
                                writeTVar u (succ i)
                                return i

addPreProcessor :: (Monad n) => T.Text -> (forall url m. (Functor m, MonadIO m) => T.Text -> ClckT url m Builder) -> ClckT u n ()
addPreProcessor name action =
    modify $ \cs ->
        cs { preProcessorCmds = Map.insert name action (preProcessorCmds cs) }

mapClckT :: (m (a, ClckState) -> n (b, ClckState))
         -> ClckT url m a
         -> ClckT url n b
mapClckT f (ClckT r) = ClckT $ mapRouteT (mapStateT f) r

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

instance (Functor m, Monad m) => GetAcidState (ClckT url m) (MenuState ClckURL) where
    getAcidState = (acidMenu . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) PageState where
    getAcidState = (acidPage . acidState) <$> get

instance (Functor m, Monad m) => GetAcidState (ClckT url m) ProfileDataState where
    getAcidState = (acidProfileData . acidState) <$> get

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
    asChild mrkup = asChild =<< (XMLGenT $ markupToContent mrkup)

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

markupToContent :: (Functor m, MonadIO m) => Markup -> ClckT url m Content
markupToContent Markup{..} =
    do clckState <- get 
       markup' <- process (preProcessorCmds clckState) markup
       e <- liftIO $ runPreProcessors preProcessors markup'
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
