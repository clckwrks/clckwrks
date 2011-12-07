module PageMapper where

import CMS
import Page
import qualified Home as Home

pageMapper :: XMLGenT (CMS ClckURL) XML
pageMapper =
    do pid <- XMLGenT $ getPageId
       case pid of
         (PageId 1) -> Home.page
         _          -> page
