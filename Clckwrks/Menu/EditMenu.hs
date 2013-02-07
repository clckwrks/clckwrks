{-# LANGUAGE FlexibleInstances, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Menu.EditMenu where

import Clckwrks.Admin.Template (template)
import Control.Applicative     ((<$>))
import Clckwrks                (query, update)
import Clckwrks.Menu.Acid      (GetMenu(..), SetMenu(..))
import Clckwrks.Menu.Types     (Menu(..), MenuLink(..), MenuLinks(..), MenuItem(..))
import Clckwrks.Monad          (Clck(..), ClckState(..), getMenuLinks, mapClckT)
import Clckwrks.URL            (ClckURL(..), AdminURL(..))
import Control.Monad.State     (get)
import Control.Monad.Trans     (lift, liftIO)
import Data.Aeson              (FromJSON(..), ToJSON(..), Value(..), (.:), (.=), decode, object)
import Data.Text               (Text)
import Data.Tree               (Tree(..))
import qualified Data.Vector   as Vector
import Happstack.Server        (Response, internalServerError, lookBS, ok, toResponse)
import HSP
import Language.Javascript.JMacro
import Web.Routes              (showURL)

editMenu :: Clck ClckURL Response
editMenu =
    do p     <- plugins <$> get
       links <- getMenuLinks p
--       liftIO $ print (toJSON links)
       currentMenu <- query GetMenu
       template "Edit Menu" (headers currentMenu links) $
                <%>
                 <h1>Menu Editor</h1>
                   <div id="alert">
                   </div>
                   <div class="form-horizontal">
                    <fieldset>
                     <legend>Add Link</legend>
                     <div class="control-group">
                      <label class="control-label" for="plugin-list">Select a plugin...</label>
                      <div class="controls">
                       <select id="plugin-list"></select><br />
                      </div>
                     </div>
                     <div class="control-group">
                      <label class="control-label" for="menu-item-list">then select a link</label>
                      <div class="controls">
                       <div class="input-append">
                        <select id="menu-item-list"></select>
                        <button class="btn" id="add-link">Add Link</button>
                       </div>
                      </div>
                     </div>
                    </fieldset>
                    -- <button id="add-sub-menu">Add Sub-Menu</button><br />
                    <fieldset>
                     <legend>Other Actions</legend>
                     <div class="control-group">
                      <label class="control-label" for="remove-item"></label>
                      <div class="controls">
                       <button class="btn btn-danger" id="remove-item"><i class="icon-remove icon-white"></i> Remove Selected Item</button>
                      </div>
                     </div>
                     <div class="control-group">
                      <label class="control-label" for="saveChanges"></label>
                      <div class="controls">
                       <button class="btn btn-success" id="saveChanges"><i class="icon-ok icon-white"></i> Save Changes</button>
                      </div>
                     </div>
                    </fieldset>

                    <fieldset>
                     <legend>Menu</legend>
                     <p><i>Drag and Drop to rearrange. Click to rename.</i></p>
                     <div id="menu"></div>
                    </fieldset>
                   </div>
                </%>
    where
      headers currentMenu menuLinks
           = do menuUpdate <- showURL (Admin MenuPost)
                <%>
                 <script type="text/javascript" src="/jstree/jquery.jstree.js" ></script>
                 <% [$jmacro|
                      function !doubleClickCB(e, dt) {
                        alert(dt.rslt.o);
//                        menu.rename(d.rslt);
                      };

                      $(document).ready(function ()
                                         { $("#menu").jstree(`(jstree currentMenu)`);
                                            var !menu = $.jstree._reference("#menu");
                                            $("#menu").bind("select_node.jstree",  function(e, d) { $("#menu").jstree("rename", d.inst.o); } );

                                            // click elsewhere in document to unselect nodes
                                            $(document).bind("click", function (e) {
                                              if(!$(e.target).parents(".jstree:eq(0)").length) {
                                                $.jstree._focused().deselect_all();
                                              }
                                             });

                                            `(initializeDropDowns menuLinks)`;
                                            `(removeItem)`;
                                            `(saveChanges menuUpdate)`;
                                         });

                    |]
                  %>
                 </%>

initializeDropDowns :: MenuLinks -> JStat
initializeDropDowns menuLinks' =
    [jmacro|
             var !menuLinks = `(toJSON menuLinks')`;
             var pluginList = $('#plugin-list');
             var menuItemList = $('#menu-item-list');

             function populateLinks (pluginIndex) {
               menuItemList.empty();
               for (var i = 0; i < menuLinks[pluginIndex].pluginLinks.length; i++) {
                 var option = $('<option></option>', { value : [pluginIndex,i] }).text(menuLinks[pluginIndex].pluginLinks[i].menuItemName);
                 option.data("menuItem", menuLinks[pluginIndex].pluginLinks[i].menuItemName);
                 menuItemList.append(option);
               }
             }

             for (var p = 0; p < menuLinks.length; p++) {
               var option = $('<option></option>', { 'value' : p }).text(menuLinks[p].pluginName);
               option.text(menuLinks[p].pluginName);
               pluginList.append(option);
             }
             populateLinks(0);
             pluginList.change(function() { populateLinks($(this).val()); });

             // add handler to add-link button
             $("#add-link").click(function () {
               var indexes = menuItemList.val().split(',');
               var menuItem = menuLinks[indexes[0]].pluginLinks[indexes[1]];
               var entry = { 'state' : 'open'
                           , 'data'  : { 'title' : menuItem.menuItemName }
                           , 'attr'  : { 'rel' : 'target' }
                           , 'metadata'
                                     : { 'link' : { 'menuName' : menuItem.menuItemName
                                                  , 'menuLink' : menuItem.menuItemLink
                                                  }
                                       }
                           };
               menu.create(null, 0,  entry , false, true);
             });

           |]

saveChanges :: Text -> JStat
saveChanges menuUpdateURL =
    [$jmacro|
     $("#saveChanges").click(function () {
       var tree = $("#menu").jstree("get_json", -1);
       var json = JSON.stringify(tree);
       console.log(json);
       $.post(`(menuUpdateURL)`, { tree : json });
       var alert = $('<div class="alert alert-success"><button type="button" class="close" data-dismiss="alert">×</button><span>Changes Saved!</span></div>');
//       alert.alert();
       $("#alert").append(alert);
     });
    |]


removeItem :: JStat
removeItem =
  [$jmacro|
   $("#remove-item").click(function() {
       menu.remove(menu.get_selected());
   });
  |]

jstree :: Menu -> Value
jstree menu =
    object [ "types" .=
             object [ "types" .=
                      object [ "root" .=
                               object [ "max_children" .= (-1 :: Int)
                                      ]
                             , "menu" .=
                               object [ "max_children" .= (-1 :: Int)
                                      ]
                             , "target" .=
                               object [ "max_children" .= (0 :: Int)
                                      , "icon"         .= False
                                      ]
                             ]
                    ]
           , "dnd" .=
               object [ "drop_target" .= False
                      , "drag_target" .= False
                      ]
           , "ui" .=
                 object [ "initially_select" .= ([ "tree-root" ] :: [String])
                        ]
           , "json_data" .= menuToJSTree menu
           , "plugins"   .= ([ "themes", "ui", "crrm", "types", "json_data", "dnd" ] :: [String])
           ]

menuToJSTree :: Menu -> Value
menuToJSTree (Menu items) =
    object  [ "data" .= (toJSON $ map menuItemToJSTree items)
            ]

menuItemToJSTree :: MenuItem -> Value
menuItemToJSTree (MILink MenuLink{..}) =
    object [ "data" .=
               object [ "title" .= menuItemName
                      ]
           , "attr" .=
               object [ "rel" .= ("target" :: String)
                      ]
           , "metadata" .=
               object [ "link" .=
                          object [ "menuLink" .= menuItemLink
                                 ]
                      ]
           ]

menuPost :: Clck ClckURL Response
menuPost =
  do t <- lookBS "tree"
     let mu = decode t :: Maybe MenuUpdate
--     liftIO $ print t
--     liftIO $ print mu
     case mu of
       Nothing ->
           do internalServerError $ toResponse ("menuPost: failed to decode JSON data" :: Text)
       (Just (MenuUpdate u)) ->
           do update (SetMenu u)
              ok $ toResponse ()

newtype MenuUpdate     = MenuUpdate Menu deriving (Show)

instance FromJSON MenuUpdate where
  parseJSON (Array a) = (MenuUpdate . Menu) <$> mapM parseJSON (Vector.toList a)

instance FromJSON MenuItem where
  parseJSON (Object o) =
      do ttl      <- o .: "data"
         meta     <- o .: "metadata"
         link     <- meta .: "link"
         menuLink <- link .: "menuLink"
         let ml = MenuLink ttl menuLink
         return (MILink ml)
