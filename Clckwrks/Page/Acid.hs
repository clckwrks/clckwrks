{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings #-}
module Clckwrks.Page.Acid
    ( module Clckwrks.Page.Types
      -- * state
    , PageState
    , initialPageState
      -- * events
    , NewPage(..)
    , PageById(..)
    , PagesSummary(..)
    , UpdatePage(..)
    , AllPosts(..)
    , GetFeedConfig(..)
    , SetFeedConfig(..)
    ) where

import Clckwrks.Page.Types  (Markup(..), PublishStatus(..), PreProcessor(..), PageId(..), PageKind(..), Page(..), Pages(..), FeedConfig(..), initialFeedConfig)
import Clckwrks.Types       (Trust(..))
import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState, Query, Update, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable, IxSet, (@=), Proxy(..), empty, fromList, getOne, ixSet, ixFun, insert, toList, toDescList, updateIx)
import Data.Maybe           (fromJust)
import Data.SafeCopy        (Migrate(..), base, deriveSafeCopy, extension)
import Data.String          (fromString)
import Data.Text            (Text)
import Data.Time.Clock      (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import qualified Data.Text  as Text
import           Data.UUID  (UUID)
import qualified Data.UUID  as UUID
import Data.UUID.V1         (nextUUID)
import Happstack.Auth       (UserId(..))

data PageState_001  = PageState_001
    { nextPageId_001 :: PageId
    , pages_001      :: IxSet Page
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageState_001)

data PageState  = PageState
    { nextPageId :: PageId
    , pages      :: IxSet Page
    , feedConfig :: FeedConfig
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'extension ''PageState)

instance Migrate PageState where
    type MigrateFrom PageState = PageState_001
    migrate (PageState_001 npi pgs) =
        PageState npi pgs (FeedConfig { feedUUID       = fromJust $ UUID.fromString "fa6cf090-84d7-11e1-8001-0021cc712949"
                                      , feedTitle      = fromString "Untitled Feed"
                                      , feedLink       = fromString ""
                                      , feedAuthorName = fromString "Anonymous"
                                      })

initialPageState :: IO PageState
initialPageState =
    do fc <- initialFeedConfig
       return $ PageState { nextPageId = PageId 2
                          , pages = fromList [ Page { pageId        = PageId 1
                                                    , pageAuthor    = UserId 1
                                                    , pageTitle     = "This title rocks!"
                                                    , pageSrc       = Markup { preProcessors = [ Markdown ]
                                                                             , trust         = Trusted
                                                                             , markup        = "This is the body!"
                                                                             }
                                                    , pageExcerpt   = Nothing
                                                    , pageDate      = posixSecondsToUTCTime 1334089928
                                                    , pageUpdated   = posixSecondsToUTCTime 1334089928
                                                    , pageStatus    = Published
                                                    , pageKind      = PlainPage
                                                    , pageUUID      = fromJust $ UUID.fromString "c306fe3a-8346-11e1-8001-0021cc712949"
                                                    }
                                             ]
                          , feedConfig = fc
                          }

pageById :: PageId -> Query PageState (Maybe Page)
pageById pid =
    do pgs <- pages <$> ask
       return $ getOne $ pgs @= pid

pagesSummary :: Query PageState [(PageId, Text)]
pagesSummary =
    do pgs <- pages <$> ask
       return $ map (\page -> (pageId page, pageTitle page)) (toList pgs)

updatePage :: Page -> Update PageState (Maybe String)
updatePage page =
    do ps@PageState{..} <- get
       case getOne $ pages @= (pageId page) of
         Nothing  -> return $ Just $ "updatePage: Invalid PageId " ++ show (unPageId $ pageId page)
         (Just _) ->
             do put $ ps { pages = updateIx (pageId page) page pages }
                return Nothing

newPage :: PageKind -> UserId -> UUID -> UTCTime -> Update PageState Page
newPage pk uid uuid now =
    do ps@PageState{..} <- get
       let page = Page { pageId      = nextPageId
                       , pageAuthor  = uid
                       , pageTitle   = "Untitled"
                       , pageSrc     = Markup { preProcessors = [ Markdown ]
                                              , trust         = Trusted
                                              , markup        = Text.empty
                                              }
                       , pageExcerpt = Nothing
                       , pageDate    = now
                       , pageUpdated = now
                       , pageStatus  = Draft
                       , pageKind    = pk
                       , pageUUID    = uuid
                       }
       put $ ps { nextPageId = PageId $ succ $ unPageId nextPageId
                , pages      = insert page pages
                }
       return page

getFeedConfig :: Query PageState FeedConfig
getFeedConfig =
    do PageState{..} <- ask
       return feedConfig

setFeedConfig :: FeedConfig -> Update PageState ()
setFeedConfig fc =
    do ps <- get
       put $ ps { feedConfig = fc }

-- | get all posts, sorted reverse cronological
allPosts :: Query PageState [Page]
allPosts =
    do pgs <- pages <$> ask
       return $ toDescList (Proxy :: Proxy UTCTime) (pgs @= Post)

$(makeAcidic ''PageState
  [ 'newPage
  , 'pageById
  , 'pagesSummary
  , 'updatePage
  , 'allPosts
  , 'getFeedConfig
  , 'setFeedConfig
  ])
