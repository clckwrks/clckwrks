{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RecordWildCards, ScopedTypeVariables #-}
module ClckwrksMonad
    ( Clck(..)
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

import Acid
import Admin.URL (AdminURL(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Aeson
import Data.Acid                     (AcidState, EventState, EventResult, QueryEvent, UpdateEvent)
import Data.Acid.Advanced            (query', update')
import qualified Data.HashMap.Lazy   as HashMap
import qualified Data.Map            as Map
import qualified Data.Vector         as Vector
import Page.Acid
import Page.Types                    (Markup(..))
import Data.ByteString.Lazy          as LB (ByteString)
import Data.ByteString.Lazy.UTF8     as LB (toString)
import Data.Data
import Data.SafeCopy                 (SafeCopy(..))
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Data.Time.Clock               (UTCTime)
import Data.Time.Format              (formatTime)
import Language.Javascript.JMacro    
import HSP hiding (Request, escape)
import HSP.ServerPartT
import qualified HSX.XMLGenerator as HSX
import Happstack.Server
import HSX.JMacro                    (IntegerSupply(..))
import System.Locale                 (defaultTimeLocale)
import Text.Blaze (Html)
import Text.Blaze.Renderer.String (renderHtml)
import Types
import URL                           (ClckURL(..))
import Web.Routes         hiding (nestURL)
import qualified Web.Routes as R
import Web.Routes.Happstack
import Web.Routes.XMLGenT ()

data ClckState 
    = ClckState { acidState       :: Acid 
                , currentPage     :: PageId
                , themePath       :: FilePath
                , componentPrefix :: Prefix
                , uniqueId        :: Integer
                }

newtype Clck url a = Clck { unClck :: RouteT url (ServerPartT (StateT ClckState IO)) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, Happstack, ServerMonad, HasRqData, FilterMonad Response, WebMonad Response, MonadState ClckState)

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


nestURL :: (url1 -> url2) -> Clck url1 a -> Clck url2 a
nestURL f (Clck r) = Clck $ R.nestURL f r

instance MonadRoute (Clck url) where
    type URL (Clck url) = url
    askRouteFn = Clck $ askRouteFn

query :: forall event m. (QueryEvent event, GetAcidState (EventState event), Functor m, MonadIO m, MonadState ClckState m) => event -> m (EventResult event)
query event =
    do as <- (getAcidState . acidState) <$> get
       query' (as :: AcidState (EventState event)) event

update :: forall event m. (UpdateEvent event, GetAcidState (EventState event), Functor m, MonadIO m, MonadState ClckState m) => event -> m (EventResult event)
update event =
    do as <- (getAcidState . acidState) <$> get
       update' (as :: AcidState (EventState event)) event

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

instance HSX.XMLGen (Clck url) where
    type HSX.XML (Clck url) = XML
    newtype HSX.Child (Clck url) = ClckChild { unClckChild :: XML }
    newtype HSX.Attribute (Clck url) = FAttr { unFAttr :: Attribute }
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

instance IsAttrValue (Clck url) T.Text where
    toAttrValue = toAttrValue . T.unpack

instance IsAttrValue (Clck url) TL.Text where
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
instance HSX.EmbedAsAttr (Clck url) Attribute where
    asAttr = return . (:[]) . FAttr 

instance (IsName n) => HSX.EmbedAsAttr (Clck url) (Attr n String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (IsName n) => HSX.EmbedAsAttr (Clck url) (Attr n Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (IsName n) => HSX.EmbedAsAttr (Clck url) (Attr n Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (IsName n) => HSX.EmbedAsAttr (Clck url) (Attr n Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (IsName n) => HSX.EmbedAsAttr (Clck url) (Attr n Integer) where
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

instance (IsName n) => (EmbedAsAttr (Clck url) (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.unpack a)

instance (IsName n) => (EmbedAsAttr (Clck url) (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ T.unpack a)

instance EmbedAsChild (Clck url) Char where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . (:[])

instance EmbedAsChild (Clck url) String where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata

instance EmbedAsChild (Clck url) Int where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance EmbedAsChild (Clck url) Integer where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance EmbedAsChild (Clck url) Double where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance EmbedAsChild (Clck url) Float where
    asChild = XMLGenT . return . (:[]) . ClckChild . pcdata . show

instance EmbedAsChild (Clck url) TL.Text where
    asChild = asChild . TL.unpack

instance EmbedAsChild (Clck url) T.Text where
    asChild = asChild . T.unpack

instance (EmbedAsChild (Clck url1) a, url1 ~ url2) => EmbedAsChild (Clck url1) (Clck url2 a) where
    asChild c = 
        do a <- XMLGenT c
           asChild a

instance (EmbedAsChild (Clck url) a) => EmbedAsChild (Clck url) (IO a) where
    asChild c = 
        do a <- XMLGenT (liftIO c)
           asChild a

{-
instance EmbedAsChild Clck TextHtml where
    asChild = XMLGenT . return . (:[]) . ClckChild . cdata . T.unpack . unTextHtml
-}
instance EmbedAsChild (Clck url) XML where
    asChild = XMLGenT . return . (:[]) . ClckChild

instance EmbedAsChild (Clck url) Html where
    asChild = XMLGenT . return . (:[]) . ClckChild . cdata . renderHtml

instance EmbedAsChild (Clck url) Markup where
    asChild mrkup = asChild =<< markupToContent mrkup

instance EmbedAsChild (Clck url) () where
    asChild () = return []

instance EmbedAsChild (Clck url) UTCTime where
    asChild = asChild . formatTime defaultTimeLocale "%a, %F @ %r"

instance AppendChild (Clck url) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unClckChild chs))

instance SetAttr (Clck url) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unFAttr attrs)) cs

instance XMLGenerator (Clck url)

data Content 
    = TrustedHtml T.Text
    | PlainText   T.Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance EmbedAsChild (Clck url) Content where
    asChild (TrustedHtml html) = asChild $ cdata (T.unpack html)
    asChild (PlainText txt)    = asChild $ pcdata (T.unpack txt)

markupToContent :: (MonadIO m) => Markup -> m Content
markupToContent Markup{..} =
    do e <- runPreProcessors preProcessors markup
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

