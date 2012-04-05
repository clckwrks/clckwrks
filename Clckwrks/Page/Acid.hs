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
    ) where

import Clckwrks.Page.Types  (Markup(..), PublishStatus(..), PreProcessor(..), PageId(..), PageKind(..), Page(..), Pages(..))
import Clckwrks.Types       (Trust(..))
import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState, Query, Update, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable, IxSet, (@=), Proxy(..), empty, fromList, getOne, ixSet, ixFun, insert, toList, toDescList, updateIx)
import Data.SafeCopy        (base, deriveSafeCopy)
import Data.Text            (Text)
import Data.Time.Clock      (UTCTime, getCurrentTime)
import qualified Data.Text  as Text

data PageState  = PageState
    { nextPageId :: PageId
    , pages      :: IxSet Page
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageState)

initialPageState :: PageState
initialPageState =
    PageState { nextPageId = PageId 2
              , pages = fromList [ Page { pageId        = PageId 1
                                        , pageTitle     = "This title rocks!"
                                        , pageSrc       = Markup { preProcessors = [ Markdown ]
                                                                 , trust         = Trusted
                                                                 , markup        = "This is the body!"
                                                                 }
                                        , pageExcerpt   = Nothing
                                        , pageDate      = Nothing
                                        , pageStatus    = Published
                                        , pageKind      = PlainPage
                                        }
                                 ]
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

newPage :: PageKind -> Update PageState Page
newPage pk =
    do ps@PageState{..} <- get
       let page = Page { pageId      = nextPageId
                       , pageTitle   = "Untitled"
                       , pageSrc     = Markup { preProcessors = [ Markdown ]
                                              , trust         = Trusted
                                              , markup        = Text.empty
                                              }
                       , pageExcerpt = Nothing
                       , pageDate    = Nothing
                       , pageStatus  = Draft
                       , pageKind    = pk
                       }
       put $ ps { nextPageId = PageId $ succ $ unPageId nextPageId
                , pages      = insert page pages
                }
       return page

-- | get all posts, sorted reverse cronological
allPosts :: Query PageState [Page]
allPosts =
    do pgs <- pages <$> ask
       return $ toDescList (Proxy :: Proxy (Maybe UTCTime)) (pgs @= Post)

$(makeAcidic ''PageState
  [ 'newPage
  , 'pageById
  , 'pagesSummary
  , 'updatePage
  , 'allPosts
  ])
