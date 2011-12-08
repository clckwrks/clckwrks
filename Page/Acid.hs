{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings #-}
module Page.Acid 
    ( module Page.Types
      -- * state
    , PageState
    , initialPageState
      -- * events
    , NewPage(..)
    , PageById(..)
    , PagesSummary(..)
    , UpdatePage(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Trans (liftIO)
import Data.Acid (AcidState, Query, Update, makeAcidic)
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable, IxSet, (@=), empty, fromList, getOne, ixSet, ixFun, insert, toList, updateIx)
import Data.SafeCopy
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Text as Text
import Page.Types

data PageState  = PageState 
    { nextPageId :: PageId
    , pages      :: Pages 
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageState)

initialPageState :: PageState
initialPageState = 
    PageState { nextPageId = PageId 2
              , pages = fromList [ Page { pageId        = PageId 1
                                        , pageTitle     = "This title rocks!"
                                        , pageSrc       = Markup { preProcessors = [ Markdown ]
                                                                 , markup        = "This is the body!"
                                                                 }
                                        , pageExcerpt   = Nothing
                                        , pageDate      = Nothing
                                        , pageStatus    = Published
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

newPage :: Update PageState Page
newPage =
    do ps@PageState{..} <- get
       let page = Page { pageId      = nextPageId
                       , pageTitle   = "Untitled"
                       , pageSrc     = Markup { preProcessors = [ Markdown ]
                                              , markup        = Text.empty
                                              }
                       , pageExcerpt = Nothing
                       , pageDate    = Nothing
                       , pageStatus  = Draft
                       }
       put $ PageState { nextPageId = PageId $ succ $ unPageId nextPageId
                       , pages = insert page pages
                       }
       return page

$(makeAcidic ''PageState 
  [ 'newPage
  , 'pageById
  , 'pagesSummary
  , 'updatePage
  ])
