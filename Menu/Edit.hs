{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Menu.Edit where

import Admin.Template
import ClckwrksMonad
import Data.Aeson
import Data.String
import Data.Tree
import           Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Server
import HSP
import Language.Javascript.JMacro
import Menu.API
import Menu.Types
import Menu.Acid
import Page.Acid
import Types
import URL

editMenu :: Menu url -> Clck url Response
editMenu menu =
    do summaries <- query PagesSummary
       template "edit menu" (headers summaries) $
         <%>
          <button id="add-page">Add Page</button>
          <select id="page-list"></select><br />
          <button id="add-sub-menu">Add Sub-Menu</button><br />
          <button id="remove-item">Remove</button><br />
          <button id="serialize">Serialize</button><br />
          <div id="menu">
          </div>
         </%>
    where
      headers summaries
              = <%>
                 <script type="text/javascript" src="/jstree/jquery.jstree.js" ></script>
                 <% [$jmacro|
                      $(document).ready(function () {
                        $("#menu").jstree(`(jstree menu)`);
                        `(serialize)`;
                        `(addPageMenu summaries)`;
                        `(addSubMenu)`;
                      });
                    |]
                  %>
                </%>

addPageMenu :: [(PageId, Text)] -> JStat
addPageMenu pageSummaries =
    [$jmacro|
      var menu = $.jstree._reference("#menu");
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
      summaryData (PageId pid, ttl)  =
          object [ fromString "data" .=
                     object [ fromString "title" .= ttl
                            ]
                 , fromString "attr" .= 
                     object [ fromString "rel" .= "target" 
                            ]
                 , fromString "metadata"  .= object [ fromString "pid" .= pid ]
                 ]
      data_ = map summaryData pageSummaries

addSubMenu =
    [$jmacro|
      var menu = $.jstree._reference("#menu");
      $("#add-sub-menu").click(function () {
        var item = { 'attr' : { 'rel' : 'menu' } }
        menu.create(null, 0, item, false, false);
      });
    |]


serialize :: JStat
serialize =
    [$jmacro|
     $("#serialize").click(function () {
       console.log($("#menu").jstree("get_json", -1));
       console.log(JSON.stringify($("#menu").jstree("get_json", -1)));
     });
    |]

jstree :: Menu url -> Value
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
           , fromString "json_data" .= menuToJSTree menu
           , fromString "plugins"   .= toJSON [ "themes", "ui", "crrm", "types", "json_data", "dnd" ]
           ]

rootNode :: Value -> Value
rootNode children =
    object  [ fromString "data" .= 
                object [ fromString "data" .= 
                         [ object [ fromString "title" .= "menu"
                                  ]
                         ]
                       ]
            , fromString "children" .= children
            ]

    

menuToJSTree :: Menu url -> Value
menuToJSTree (Menu items) =
    object  [ fromString "data" .= (toJSON $ map menuTreeToJSTree items) 
            ]

menuTreeToJSTree :: Tree (MenuItem url) -> Value
menuTreeToJSTree (Node item children) =
    object [ fromString "data" .= 
               object [ fromString "title" .= menuTitle item ]
           , fromString "children" .=
               map menuTreeToJSTree children
           ]
