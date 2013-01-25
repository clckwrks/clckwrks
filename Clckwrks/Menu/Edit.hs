{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Menu.Edit where

import Clckwrks.Admin.Template (template)
import Clckwrks.Menu.Types     (Menu(..), MenuItem(..), MenuLink(..), MenuName(..))
import Clckwrks.Menu.Acid      (SetMenu(..))
import Clckwrks.Monad          (Clck, query, update)
--import Clckwrks.Page.Acid      (PageId(..), PagesSummary(..))
-- import Clckwrks.Page.Types     (Slug(..), slugify)
import Clckwrks.Types          (Prefix(..))
import Clckwrks.URL            (ClckURL(..), AdminURL(..))
import Control.Applicative     ((<$>), (<|>), optional, pure)
import Data.Aeson              (FromJSON(..), ToJSON(..), Value(..), (.:), (.=), decode, object)
import Data.String             (fromString)
import Data.Tree               (Tree(..))
import           Data.Text     (Text)
import qualified Data.Text     as Text
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Vector   as Vector
import Happstack.Server        (Response, internalServerError, lookBS, ok, toResponse)
import HSP
import Language.Javascript.JMacro
import Web.Routes              (PathInfo, showURL, toPathInfo, fromPathInfo)


-- MenURL ?

editMenu :: (PathInfo url) => Menu url -> Clck ClckURL Response
editMenu menu =
    do -- summaries <- query PagesSummary
       let summaries = undefined
       let clckLinks = [ -- (toPathInfo Blog, fromString "Blog")
                       ]
       template "edit menu" (headers summaries clckLinks) $
         <%>
          <button id="add-page">Add Page</button>
          <select id="page-list"></select><br />
          <button id="add-clckwrks-link">Add Clckwrks Link</button>
          <select id="clckwrks-link"></select><br />
          <button id="add-sub-menu">Add Sub-Menu</button><br />
          <button id="remove-item">Remove</button><br />
          <button id="saveChanges">Save Changes</button><br />
          <div id="menu">
          </div>
         </%>
    where
      headers summaries clckLinks
           = do menuUpdate <- showURL (Admin MenuPOST)
                <%>
                 <script type="text/javascript" src="/jstree/jquery.jstree.js" ></script>
                 <% [$jmacro|
                      $(document).ready(function () {
                        $("#menu").jstree(`(jstree menu)`);
                        var !menu = $.jstree._reference("#menu");
                        // click elsewhere in document to unselect nodes
                        $(document).bind("click", function (e) {
                         if(!$(e.target).parents(".jstree:eq(0)").length) {
                                 $.jstree._focused().deselect_all();
                         }
                        });
                        `(saveChanges menuUpdate)`;
 /* -- FIXME                        `(addPageMenu summaries)`;*/
                        `(addClckwrksMenu clckLinks)`;
                        `(addSubMenu)`;
                        `(removeItem)`;
//                        `(menuEvents)`;
                      });
                    |]
                  %>
                 </%>

addClckwrksMenu :: [(Text, Text)] -> JStat
addClckwrksMenu linkInfos =
    [$jmacro|
      var select = $("#clckwrks-link");
      var links  = `(data_)`;

      for (var i = 0; i < links.length; i++) {
       var option = $("<option>");
       option.attr('value', i);
       option.text(links[i].data.title);
       option.data( 'menu', links[i]);
       select.append(option);
      }

      $("#add-clckwrks-link").click(function () {
        var i = select.val();
        menu.create(null, 0, links[i], false, true);
      });

     |]
    where
      data_ = map summaryData linkInfos

      summaryData (link, ttl)  =
          object [ fromString "data" .=
                     object [ fromString "title" .= ttl
                            ]
                 , fromString "attr" .=
                     object [ fromString "rel" .= "target"
                            ]
                 , fromString "metadata"  .= object [ fromString "link" .=
                                                                 object [ fromString "linkType" .= "url"
                                                                        , fromString "linkDest" .= link
                                                                        ]
                                                    ]
                 ]

{-
addPageMenu :: [(PageId, Text, Maybe Slug)] -> JStat
addPageMenu pageSummaries =
    [$jmacro|
      var select = $("#page-list");
      var pages = `(data_)`;

      for (var i = 0; i < pages.length; i++) {
       var option = $("<option>");
       option.attr('value', i);
       option.text(pages[i].data.title);
       option.data( 'menu', pages[i]);
       select.append(option);
      }

      $("#add-page").click(function () {
        var i = select.val();
        menu.create(null, 0, pages[i], false, true);
      });
    |]
    where
      root =
          object [ fromString "data" .=
                     object [ fromString "title" .= "menu"
                            ]
                 , fromString "attr" .=
                     object [ fromString "rel" .= "root"
                            ]
                 ]
      summaryData (PageId pid, ttl, slug)  =
          object [ fromString "data" .=
                     object [ fromString "title" .= ttl
                            ]
                 , fromString "attr" .=
                     object [ fromString "rel" .= "target"
                            ]
                 , fromString "metadata"  .= object [ fromString "pid" .= pid ]
                 ]
      data_ = map summaryData pageSummaries
-}
addSubMenu :: JStat
addSubMenu =
    [$jmacro|
      $("#add-sub-menu").click(function () {
        var item = { 'attr' : { 'rel' : 'menu' } }
        menu.create(null, 0, item, false, false);
      });
    |]

removeItem :: JStat
removeItem =
  [$jmacro|
   $("#remove-item").click(function() {
       menu.remove(menu.get_selected());
   });
  |]

{-
menuEvents :: JStat
menuEvents =
   [$jmacro|
     $("#menu").bind("select_node.jstree", function (event, d) {
                        if (d.inst
                        alert($(d.args[0]).text());
     });
    |]
-}
saveChanges :: Text -> JStat
saveChanges menuUpdateURL =
    [$jmacro|
     $("#saveChanges").click(function () {
       var tree = $("#menu").jstree("get_json", -1);
       var json = JSON.stringify(tree);
       console.log(json);
       $.post(`(menuUpdateURL)`, { tree : json });
     });
    |]

jstree :: (PathInfo url) => Menu url -> Value
jstree menu =
    object [ fromString "types" .=
               object [ fromString "types" .=
                         object [ fromString "root" .=
                                    object [ fromString "max_children" .= (-1 :: Int)
                                           ]
                                , fromString "menu" .=
                                    object [ fromString "max_children" .= (-1 :: Int)
                                           ]
                                , fromString "target" .=
                                    object [ fromString "max_children" .= (0 :: Int)
                                           ]
                                ]
                      ]
           , fromString "dnd" .=
               object [ fromString "drop_target"  .= False
                      , fromString "drag_target"  .= False
                      ]

           , fromString "ui" .=
               object [ fromString "initially_select" .= [ "tree-root" ]
                      ]

           , fromString "json_data" .= menuToJSTree menu
           , fromString "plugins"   .= toJSON [ "themes", "ui", "crrm", "types", "json_data", "dnd" ]
           ]

rootNode :: Value -> Value
rootNode children =
    object  [ fromString "data" .=
                object [ fromString "data" .=
                           object [ fromString "title" .= "menu"
                                  ]

                , fromString "attr" .=
                    object [ fromString "id" .= "tree-root"
                           ]

                , fromString "children" .= children

                ]
            ]

menuToJSTree :: (PathInfo url) => Menu url -> Value
menuToJSTree (Menu items) =
    object  [ fromString "data" .= (toJSON $ map menuTreeToJSTree items)
            ]

menuTreeToJSTree :: (PathInfo url) => Tree (MenuItem url) -> Value
menuTreeToJSTree (Node item children) =
    object [ fromString "data" .=
               object [ fromString "title" .= menuTitle item ]
           , fromString "metadata" .=
               object [ fromString "menuName" .=
                          object [ fromString "prefix" .= prefixText (menuPrefix (menuName item))
                                 , fromString "tag"    .= menuTag (menuName item)
                                 , fromString "unique" .= menuUnique (menuName item)
                                 ]
                      , fromString "link" .=
                                   case (menuLink item) of
                                     (LinkText txt) ->
                                         object [ fromString "linkType" .= "text"
                                                , fromString "linkDest"  .= txt
                                                ]
                                     (LinkURL url) ->
                                         object [ fromString "linkType" .= "url"
                                                , fromString "linkDest" .= toPathInfo url
                                                ]

                      ]
           , fromString "children" .=
               map menuTreeToJSTree children
           ]

newtype MenuUpdate url = MenuUpdate ([Tree (MenuUpdateItem url)]) deriving (Show)
newtype MenuUpdateItem url = MenuUpdateItem (String, Maybe MenuName, Maybe Integer, Maybe (MenuLink url)) deriving (Show)

instance (PathInfo url) => FromJSON (MenuUpdate url) where
  parseJSON (Array a) = MenuUpdate <$> mapM parseJSON (Vector.toList a)

instance (PathInfo url) => FromJSON (Tree (MenuUpdateItem url)) where
  parseJSON (Object o) =
    do ttl      <- o .: (fromString "data")
       meta     <- o .: (fromString "metadata")
       pid      <- optional $ meta .: (fromString "pid")
       link     <- do mLinkObj <- optional $ meta .: (fromString "link")
                      case mLinkObj of
                        Nothing ->  return Nothing
                        (Just linkObj) ->
                            do linkType <- linkObj .: (fromString "linkType")
                               case () of
                                 () | linkType == "text" ->
                                       do linkDest <- linkObj .: (fromString "linkDest")
                                          return (Just $ LinkText linkDest)
                                    | linkType == "url" ->
                                        do linkDest <- linkObj .: (fromString "linkDest")
                                           case fromPathInfo linkDest of
                                             (Left _) -> return Nothing
                                             (Right u) -> return (Just $ LinkURL u)

       menuName <- do mmno <- optional $ meta .: (fromString "menuName")
                      case mmno of
                        Nothing -> return Nothing
                        (Just mno) ->
                            do prefix <- mno .: fromString "prefix"
                               tag    <- mno .: fromString "tag"
                               unique <- mno .: fromString "unique"
                               return (Just $ MenuName (Prefix prefix) tag unique)
       children <- (o .: (fromString "children")) <|> pure Vector.empty
       return (Node (MenuUpdateItem (ttl, menuName, pid, link)) (Vector.toList children))

menuPost :: Clck ClckURL Response
menuPost =
  do t <- lookBS "tree"
     let mu = decode t :: Maybe (MenuUpdate ClckURL)
     case mu of
       Nothing ->
           internalServerError $ toResponse "menuPost: failed to decode JSON data"
       (Just u) ->
           do update (SetMenu (updateToMenu u))
              ok $ toResponse ()

updateToMenu :: (MenuUpdate ClckURL) -> Menu ClckURL
updateToMenu (MenuUpdate t) =
    Menu $ map convertItem t
    where
      convertItem :: Tree (MenuUpdateItem ClckURL) -> Tree (MenuItem ClckURL)
      convertItem (Node (MenuUpdateItem (ttl, mmn, mPageId, mLink)) children) =
          let menuName = case mmn of
                           Just mn -> mn
                           Nothing -> MenuName (Prefix (fromString "clckwrks")) (fromString "tag") 1
              menuItem = MenuItem { menuName  = menuName
                                  , menuTitle = Text.pack ttl
                                  , menuLink =
                                      case mPageId of
--                                        (Just pid) -> LinkURL (ViewPage (PageId pid))
                                        Nothing ->
                                            case mLink of
                                              Nothing -> LinkText (fromString "updateToMenu failed") -- Text.empty -- FIXME: this is really an error..
                                              (Just link) -> link
                                  }
          in Node menuItem (map convertItem children)

