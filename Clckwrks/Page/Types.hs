{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Page.Types where

import Clckwrks.Markup.HsColour (hscolour)
import Clckwrks.Markup.Markdown (markdown)
import Clckwrks.Types           (Trust(..))
import Control.Applicative      ((<$>))
import Control.Monad.Trans      (MonadIO(liftIO))
import Data.Aeson               (ToJSON(..), FromJSON(..))
import Data.Char                (ord)
import Data.Data                (Data, Typeable)
import Data.Maybe               (fromMaybe)
import Data.IxSet               (Indexable(..), IxSet, ixFun, ixSet)
import Data.SafeCopy            (Migrate(..), base, deriveSafeCopy, extension)
import Data.String              (fromString)
import Data.Text                (Text)
import Data.Time                (UTCTime)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import Data.UUID                (UUID)
import Data.UUID.V5             (generateNamed, namespaceOID)
import Happstack.Auth           (UserId(..))
import Web.Routes               (PathInfo(..))
import System.Random            (randomIO)

$(deriveSafeCopy 0 'base ''UUID)

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

data Page_001
    = Page_001 { pageId_001        :: PageId
               , pageTitle_001     :: Text
               , pageSrc_001       :: Markup
               , pageExcerpt_001   :: Maybe Markup
               , pageDate_001      :: Maybe UTCTime
               , pageStatus_001    :: PublishStatus
               , pageKind_001      :: PageKind
               }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Page_001)


data Page
    = Page { pageId        :: PageId
           , pageAuthor    :: UserId
           , pageTitle     :: Text
           , pageSrc       :: Markup
           , pageExcerpt   :: Maybe Markup
           , pageDate      :: UTCTime
           , pageUpdated   :: UTCTime
           , pageStatus    :: PublishStatus
           , pageKind      :: PageKind
           , pageUUID      :: UUID
           }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''Page)

instance Migrate Page where
    type MigrateFrom Page = Page_001
    migrate (Page_001 pi pt ps pe pd pst pk) =
        (Page pi (UserId 1) pt ps pe (fromMaybe epoch pd) (fromMaybe epoch pd) pst pk $ generateNamed namespaceOID (map (fromIntegral . ord) (show pi ++ show ps)))
            where
              epoch = posixSecondsToUTCTime 0

instance Indexable Page where
    empty = ixSet [ ixFun ((:[]) . pageId)
                  , ixFun ((:[]) . pageDate)
                  , ixFun ((:[]) . pageKind)
                  , ixFun ((:[]) . pageDate)
                  , ixFun ((:[]) . pageStatus)
                  ]

type Pages = IxSet Page

data FeedConfig = FeedConfig
    { feedUUID       :: UUID -- ^ UUID which identifies this feed. Should probably never change
--    , feedCategory :: Set Text
    , feedTitle      :: Text
    , feedLink       :: Text
    , feedAuthorName :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeedConfig)

initialFeedConfig :: IO FeedConfig
initialFeedConfig =
    do uuid <- randomIO
       return $ FeedConfig { feedUUID       = uuid
                           , feedTitle      = fromString "Untitled Feed"
                           , feedLink       = fromString ""
                           , feedAuthorName = fromString "Anonymous"
                           }
