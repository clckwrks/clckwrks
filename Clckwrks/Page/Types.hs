{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Page.Types where

import Clckwrks.Markup.HsColour (hscolour)
import Clckwrks.Markup.Markdown (markdown)
import Clckwrks.Types           (Trust(..))
import Control.Applicative      ((<$>))
import Control.Monad.Trans      (MonadIO(liftIO))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Data                (Data, Typeable)
import Data.IxSet               (Indexable(..), IxSet, ixFun, ixSet)
import Data.SafeCopy            (Migrate(..), base, deriveSafeCopy, extension)
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

runPreProcessors :: (MonadIO m) => [PreProcessor] -> Trust -> Text -> m (Either Text Text)
runPreProcessors [] _ txt = return (Right txt)
runPreProcessors (p:ps) trust txt =
    do e <- runPreProcessor p trust txt
       case e of
         (Left e) -> return (Left e)
         (Right txt') -> runPreProcessors ps trust txt'

runPreProcessor :: (MonadIO m) => PreProcessor -> Trust -> Text -> m (Either Text Text)
runPreProcessor pproc trust txt =
    do let f = case pproc of
                 Markdown -> markdown Nothing trust
                 HsColour -> hscolour Nothing
       f txt

data Markup_001
    = Markup_001 { preProcessors_001 :: [PreProcessor]
                 , markup_001 :: Text
                 }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Markup_001)

data Markup
    = Markup { preProcessors :: [PreProcessor]
             , markup        :: Text
             , trust         :: Trust
             }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Markup)

instance Migrate Markup where
    type MigrateFrom Markup = Markup_001
    migrate (Markup_001 pp mu) = Markup pp mu Trusted

data PublishStatus
    = Draft
    | Revoked
    | Published
    | Scheduled
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PublishStatus)

data PageKind
    = PlainPage
    | Post
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageKind)

data Page
    = Page { pageId        :: PageId
           , pageTitle     :: Text
           , pageSrc       :: Markup
           , pageExcerpt   :: Maybe Markup
           , pageDate      :: Maybe UTCTime
           , pageStatus    :: PublishStatus
           , pageKind      :: PageKind
           }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Page)

instance Indexable Page where
    empty = ixSet [ ixFun ((:[]) . pageId)
                  , ixFun ((:[]) . pageDate)
                  , ixFun ((:[]) . pageKind)
                  ]

type Pages = IxSet Page
