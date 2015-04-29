{-# LANGUAGE FlexibleInstances, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.NavBar.EditNavBar where

import Clckwrks.Admin.Template (template)
import Control.Applicative     ((<$>))
import Clckwrks                (query, update)
import Clckwrks.NavBar.Acid    (GetNavBar(..), SetNavBar(..))
import Clckwrks.NavBar.Types   (NavBar(..), NavBarLinks(..), NavBarItem(..))
import Clckwrks.Monad          (Clck(..), ClckState(..), getNavBarLinks, mapClckT)
import Clckwrks.Types          (NamedLink(..))
import Clckwrks.URL            (ClckURL(..), AdminURL(..))
import Control.Monad.State     (get)
import Control.Monad.Trans     (lift, liftIO)
import Data.Aeson              (FromJSON(..), ToJSON(..), Value(..), (.:), (.=), decode, object)
import Data.Aeson.QQ           (aesonQQ)
import qualified Data.Text     as T
import Data.Text.Lazy (Text)
import Data.Tree               (Tree(..))
import qualified Data.Vector   as Vector
import Happstack.Server        (Response, internalServerError, lookBS, ok, toResponse)
import HSP.XML                 (XML, fromStringLit)
import HSP.XMLGenerator
import Language.Javascript.JMacro
import Web.Routes              (showURL)

editNavBar :: Clck ClckURL Response
editNavBar =
    do p     <- plugins <$> get
       links <- getNavBarLinks p
--       liftIO $ print (toJSON links)
       currentNavBar <- query GetNavBar
       template "Edit NavBar" (headers currentNavBar links) $
                <%>
                 <h1>NavBar Editor</h1>
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
                      <label class="control-label" for="navBar-item-list">then select a link</label>
                      <div class="controls">
                       <div class="input-append">
                        <select id="navBar-item-list"></select>
                        <button class="btn" id="add-link">Add Link</button>
                       </div>
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
                     <legend>NavBar</legend>
                     <p><i>Drag and Drop to rearrange. Right-click to rename or remove.</i></p>
                     <div id="navBar"></div>
                    </fieldset>
                   </div>
                </%>
    where
      headers :: NavBar -> NavBarLinks -> GenChildList (Clck ClckURL)
      headers currentNavBar navBarLinks
           = do navBarUpdate <- showURL (Admin NavBarPost)
                <%>
                 <link rel="stylesheet" href="/jstree/themes/default/style.min.css" />
                 <script type="text/javascript" src="/jstree/jstree.min.js" ></script>
                 <% [$jmacro|
//                      function !doubleClickCB(e, dt) {
//                        alert(dt.rslt.o);
//                        navBar.rename(d.rslt);
//                      };

                      $(document).ready(function ()
                        { $("#navBar").jstree({ 'core' :
                                                 { 'data'           : `(navBarToJSTree currentNavBar)`
                                                 , 'themes'         : { 'variant' : 'large' }
                                                 , 'check_callback' : true
                                                 }
                                              , 'contextmenu' :
                                                 { 'items' : function (o, cb) {
                                                                var items    = $.jstree.defaults.contextmenu.items(o, cb);
                                                                return { 'rename' : items.rename
                                                                       , 'remove' : items.remove
                                                                       };
                                                              }
                                                 }
                                              , 'types' :
                                                 { 'target' :
                                                   { 'icon'         : 'icon-black icon-bookmark'
                                                   , 'max_children' : 0
                                                   }
                                                 }
                                              , 'plugins'          : [ 'contextmenu', 'dnd', 'types' ]
                                              });

                          var !navBar = $.jstree.reference("#navBar");
                          `(initializeDropDowns navBarLinks)`;
                          `(removeItem)`;
                          `(saveChanges navBarUpdate)`;
                        });
                    |]
                  %>
                 </%>

initializeDropDowns :: NavBarLinks -> JStat
initializeDropDowns navBarLinks' =
    [jmacro|
             var !navBarLinks = `(toJSON navBarLinks')`;
             var pluginList = $('#plugin-list');
             var navBarItemList = $('#navBar-item-list');

             function populateLinks (pluginIndex) {
               navBarItemList.empty();
               for (var i = 0; i < navBarLinks[pluginIndex].pluginLinks.length; i++) {
                 var option = $('<option></option>', { value : [pluginIndex,i] }).text(navBarLinks[pluginIndex].pluginLinks[i].navBarItemName);
                 option.data("navBarItem", navBarLinks[pluginIndex].pluginLinks[i].navBarItemName);
                 navBarItemList.append(option);
               }
             }

             for (var p = 0; p < navBarLinks.length; p++) {
               var option = $('<option></option>', { 'value' : p }).text(navBarLinks[p].pluginName);
               option.text(navBarLinks[p].pluginName);
               pluginList.append(option);
             }

             populateLinks(0);
             pluginList.change(function() { populateLinks($(this).val()); });

             // add handler to add-link button
             $("#add-link").click(function () {
               var indexes = navBarItemList.val().split(',');
               var navBarItem = navBarLinks[indexes[0]].pluginLinks[indexes[1]];

               var entry = { 'state'    : { 'opened' : true }
                           , 'text'     : navBarItem.navBarItemName
                           , 'type'     : 'target'
                           , 'a_attr'   : { 'rel' : 'target' }
                           , 'data' :
                                { 'link' :
                                     { 'navBarName' : navBarItem.navBarItemName
                                     , 'navBarLink' : navBarItem.navBarItemLink
                                     }
                                }
                           };
               navBar.create_node(null, entry, "last",  null, false);
             });

           |]

saveChanges :: T.Text -> JStat
saveChanges navBarUpdateURL =
    [$jmacro|
     $("#saveChanges").click(function () {
       var tree = navBar.get_json('#'); // $("#navBar").jstree("get_json", -1);
       var json = JSON.stringify(tree);
       console.log(json);
       $.post(`(navBarUpdateURL)`, { tree : json });
       var alert = $('<div class="alert alert-success"><button type="button" class="close" data-dismiss="alert">×</button><span>Changes Saved!</span></div>');
//       alert.alert();
       $("#alert").append(alert);
     });
    |]


removeItem :: JStat
removeItem =
  [$jmacro|
   $("#remove-item").click(function() {
       navBar.remove(navBar.get_selected());
   });
  |]

  {-
    object [ "core" .=
             object [ "data" .= navBarToJSTree navBar
                    ]
           , "types" .=
             object [ "#" .=
                      object [ "max_children" .= (-1 :: Int)
                             ]
                    , "navBar" .=
                      object [ "max_children" .= (-1 :: Int)
                             ]
                    , "target" .=
                      object [ "max_children" .= (0 :: Int)
                             , "icon"         .= False
                             ]
                    ]
           , "dnd" .=
               object [ "drop_target" .= False
                      , "drag_target" .= False
                      ]
           , "ui" .=
                 object [ "initially_select" .= ([ "tree-root" ] :: [String])
                        ]
           , "plugins"   .= ([ "themes", "ui", "crrm", "types", "json_data", "dnd" ] :: [String])
           ]
-}
navBarToJSTree :: NavBar -> [Value]
navBarToJSTree (NavBar items) =
  map navBarItemToJSTree items

navBarItemToJSTree :: NavBarItem -> Value
navBarItemToJSTree (NBLink NamedLink{..}) =
  [aesonQQ|
     { text : #{namedLinkTitle}
     , type : "target"
     , a_attr : { rel : "target" }
     , data : { link :
                 { navBarLink : #{namedLinkURL}
                 }
              }

     } |]
--  toJSON namedLinkTitle
  {-
    object [ "data" .=
               object [ "title" .= namedLinkTitle
                      ]
           , "attr" .=
               object [ "rel" .= ("target" :: String)
                      ]
           , "metadata" .=
               object [ "link" .=
                          object [ "navBarLink" .= namedLinkURL
                                 ]
                      ]
           ]
-}
{-
jstree :: NavBar -> Value
jstree navBar = -- #{navBarToJSTree navBar}
  [aesonQQ|
   { core :
       { data           : []
       , themes         : { variant : "large" }
       , check_callback : true
       }
   , types :
       { target :
           { icon         : "glyphicon glyphicon-flash"
           , max_children : 0
           }
       }
   , plugins          : ["contextmenu", "dnd", "types"]
   } |]
-}
navBarPost :: Clck ClckURL Response
navBarPost =
  do t <- lookBS "tree"
     let mu = decode t :: Maybe NavBarUpdate
--     liftIO $ print t
--     liftIO $ print mu
     case mu of
       Nothing ->
           do internalServerError $ toResponse ("navBarPost: failed to decode JSON data" :: T.Text)
       (Just (NavBarUpdate u)) ->
           do update (SetNavBar u)
              ok $ toResponse ()

newtype NavBarUpdate     = NavBarUpdate NavBar deriving (Show)

instance FromJSON NavBarUpdate where
  parseJSON (Array a) = (NavBarUpdate . NavBar) <$> mapM parseJSON (Vector.toList a)

instance FromJSON NavBarItem where
  parseJSON (Object o) =
      do ttl        <- o    .: "text"
         meta       <- o    .: "data"
         link       <- meta .: "link"
         navBarLink <- link .: "navBarLink"
         let nl = NamedLink ttl navBarLink
         return (NBLink nl)
