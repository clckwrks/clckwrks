{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.Page.Types where

import Clckwrks.Markup.HsColour (hscolour)
import Clckwrks.Markup.Markdown (markdown)
import Control.Applicative      ((<$>))
import Control.Monad.Trans      (MonadIO(liftIO))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Data                (Data, Typeable)
import Data.IxSet               (Indexable(..), IxSet, ixFun, ixSet)
import Data.SafeCopy            (base, deriveSafeCopy)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Web.Routes               (PathInfo(..))

instance PathInfo PageId where
    toPathSegments (PageId i) = toPathSegments i
    fromPathSegments = PageId <$> fromPathSegments

newtype PageId = PageId { unPageId :: Integer }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageId)

instance ToJSON PageId where
    toJSON (PageId i) = toJSON i
instance FromJSON PageId where
    parseJSON n = PageId <$> parseJSON n

data PreProcessor 
    = HsColour
    | Markdown
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PreProcessor)

-- $(deriveJSON id ''PreProcessor)

runPreProcessors :: (MonadIO m) => [PreProcessor] -> Text -> m (Either Text Text)
runPreProcessors [] txt = return (Right txt)
runPreProcessors (p:ps) txt = 
    do e <- runPreProcessor p txt
       case e of
         (Left e) -> return (Left e)
         (Right txt') -> runPreProcessors ps txt'

runPreProcessor :: (MonadIO m) => PreProcessor -> Text -> m (Either Text Text)
runPreProcessor pproc txt =
    do let f = case pproc of
                 Markdown -> markdown Nothing
                 HsColour -> hscolour Nothing
       f txt


data Markup
    = Markup { preProcessors :: [PreProcessor]
             , markup :: Text 
             }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Markup)

data PublishStatus
    = Draft
    | Revoked
    | Published
    | Scheduled
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PublishStatus)

data Page 
    = Page { pageId        :: PageId
           , pageTitle     :: Text 
           , pageSrc       :: Markup
           , pageExcerpt   :: Maybe Markup
           , pageDate      :: Maybe UTCTime
           , pageStatus    :: PublishStatus
           }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Page)

instance Indexable Page where
    empty = ixSet [ ixFun ((:[]) . pageId) 
                  ]

type Pages = IxSet Page
