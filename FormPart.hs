{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Wwarn -F -pgmFtrhsx #-}
module FormPart
    ( FormDF
    , formPart
    , multiFormPart
    , notEmpty
    , fieldset
    , ol
    , li
    , minLengthString
    , minLengthText
    , nullToNothing
    , rowsToList
    , rowsToColumn
    , submitOnChange
    , inputTextArea
    ) where

import Control.Applicative        (Alternative, Applicative(pure), (*>))
import Control.Monad              (MonadPlus(mplus), msum)
import Data.List                  (intersperse)
import Data.Maybe                 (fromMaybe)
import Data.Monoid                (Monoid(mempty), mconcat)
import qualified Data.Text        as Text
import           Data.Text        (Text)
import Data.Traversable           (sequenceA)
import Happstack.Server           (Happstack, Request(rqMethod), ToMessage(..)
                                  , Method(..), Input, escape, method, getDataFn, look, localRq)
import HSP                        (XMLGenerator, XMLGenT(..), Attr(..), EmbedAsAttr(..), EmbedAsChild(..), SetAttr, GenXML, (<@), genElement, genEElement, unXMLGenT)
import Happstack.Server.HSX       () -- instance (ServerMonad XMLGenT)
import qualified HSX.XMLGenerator as HSX
import Text.Digestive             ((++>), Validator, Transformer, Form(..), FormRange(..), Result(..)
                                  , View(unView), check, mapView, runForm, transformEither, validate, view, viewForm)
import Text.Digestive.Forms.Happstack (happstackEnvironment)
import Text.Digestive.HSP.Html4       (errors)
import qualified Text.Digestive.Forms  as Forms

type FormDF m a = Form m [Input] String [XMLGenT m (HSX.XML m)] a

-- | turn a formlet into XML+ServerPartT which can be embedded in a larger document
formPart ::
  (EmbedAsChild m xml, EmbedAsAttr m (Attr String String), EmbedAsAttr m (Attr String Text), ToMessage b, Happstack m, Alternative m)
  => String     -- ^ prefix
  -> Text       -- ^ url to POST form results to
  -> (a -> m b) -- ^ handler used when form validates
  -> Maybe ([(FormRange, e)] -> [XMLGenT m (HSX.XML m)] -> m b) -- ^ handler used when form does not validate
  -> Form m [Input] e xml a      -- ^ the formlet
  -> XMLGenT m (HSX.XML m)
formPart prefix action handleSuccess mHandleFailure form =
  XMLGenT $ 
    msum [ do method [GET, HEAD]
              v <- viewForm form prefix
              unXMLGenT $ 
                <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                  <% v %>
                 </form>
         , do method POST
              (v,r') <- runForm form prefix $ happstackEnvironment
              r <- r'
              case r of
                (Ok a)    -> (escape . fmap toResponse) $ handleSuccess a
                (Error e) ->
                    case mHandleFailure of
                      (Just handleFailure) ->
                          (escape . fmap toResponse) $
                             handleFailure e [ <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                                                 <% unView v e %>
                                               </form>
                                             ]
                      Nothing ->
                          unXMLGenT $ 
                            <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                             <% unView v e %>
                            </form>
         ]


multiFormPart ::
  (EmbedAsChild m xml, EmbedAsAttr m (Attr String String), EmbedAsAttr m (Attr String Text), ToMessage b, Happstack m, Alternative m) 
  => String -- ^ unique name for the formlet
  -> Text -- ^ url to POST form results to
  -> (a -> m b) -- ^ handler used when form validates
   -> Maybe ([(FormRange, e)] -> [XMLGenT m (HSX.XML m)] -> m b) -- ^ handler used when form does not validate
  -> Form m [Input] e xml a      -- ^ the formlet
  -> XMLGenT m (HSX.XML m)
multiFormPart name action success failure form = guard name (formPart name (action `Text.append` (Text.pack $ "?form=" ++ name)) success failure form)
    where
      guard :: (Happstack m) => String -> m a -> m a
      guard formName part =
          (do method POST
              submittedName <- getDataFn (look "form")
              if (submittedName == (Right formName))
               then part
               else localRq (\req -> req { rqMethod = GET }) part
          ) `mplus` part


notEmpty :: (Monad m) => Validator m String Text
notEmpty = (check "field can not be empty") (not . Text.null)

fieldset :: (Functor m, XMLGenerator m) => FormDF m a -> FormDF m a
fieldset = mapView $ \xml -> [<fieldset><% xml %></fieldset>]

ol :: (Functor m, XMLGenerator m) => FormDF m a -> FormDF m a
ol = mapView $ \xml -> [<ol><% xml %></ol>]

li :: (Functor m, XMLGenerator m) => FormDF m a -> FormDF m a
li = mapView $ \xml -> [<li><% xml %></li>]

minLengthText :: (Functor m, XMLGenerator m) => Int -> FormDF m Text -> FormDF m Text
minLengthText 0 f = f
minLengthText 1 f = errors ++> (f `validate` (check "This field can not be empty." (not . Text.null)))
minLengthText n f = errors ++> (f `validate` (check ("This field must be at least " ++ show n ++ " characters.") (\t -> Text.length t >= n)))

minLengthString :: (Functor m, XMLGenerator m) => Int -> FormDF m String -> FormDF m String
minLengthString 0 f = f
minLengthString 1 f = errors ++> (f `validate` (check "This field can not be empty." (not . null)))
minLengthString n f = errors ++> (f `validate` (check ("This field must be at least " ++ show n ++ " characters.") (\t -> length t >= n)))

nullToNothing :: (Monad m) => Transformer m e Text (Maybe Text)
nullToNothing = 
    transformEither $ \t ->
        Right $ if Text.null t
                 then Nothing
                 else (Just t)
          
seqA :: (Functor m, Monad m) => [Form m i e v a] -> Form m i e [v] [a]
seqA xs = sequenceA (map ((: []) `mapView`) xs)
                
rowsToList :: (XMLGenerator x, EmbedAsChild x v, Functor m, Monad m, Monoid a) => String -> [Form m i e v a] -> Form m i e [XMLGenT x (HSX.XML x)] a
rowsToList class' [] = view [<div class=class'>(none)</div>] *> pure mempty
rowsToList class' rows 
    = (\xs -> [<ul class=class'>
                  <% map (\x -> <li><% x %></li>) xs %>
               </ul>])
      `mapView` (fmap mconcat $ seqA rows)

rowsToColumn :: (XMLGenerator x, Applicative m, Monad m, Monoid a) => [Form m i e [XMLGenT x (HSX.XML x)] a] -> Form m i e [XMLGenT x (HSX.XML x)] a
rowsToColumn [] = view [<span>(none)</span>] *> pure mempty
rowsToColumn rows = (mconcat . intersperse [<hr/>]) `mapView` (fmap mconcat $ seqA rows)

submitOnChange :: (EmbedAsAttr m (Attr String String), SetAttr m elem) => elem -> GenXML m
submitOnChange elm = elm <@ ("onChange" := "submit()")

-- FIXME: update digestive-functors-hsp
inputTextArea :: (Monad m, Functor m, XMLGenerator x, Forms.FormInput i f) =>
                 Maybe Int -- ^ cols
              -> Maybe Int -- ^ rows
              -> Maybe Text
              -> Form m i e [XMLGenT x (HSX.XML x)] Text
inputTextArea c r = 
    Forms.inputText $ \id' inp ->
        [<textarea name=(show id') id=(show id') (rows r ++ cols c)><% Text.unpack $ fromMaybe Text.empty inp %></textarea>]
    where
      rows Nothing  = []
      rows (Just n) = [("rows" := n)]
      cols Nothing  = []
      cols (Just n) = [("cols" := n)]
