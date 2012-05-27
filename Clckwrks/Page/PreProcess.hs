{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.PreProcess where
import Control.Monad.Trans (MonadIO)
import Control.Applicative ((<*>), (*>), (<$>), (<|>), optional)
import Clckwrks.Monad (ClckT, ClckState, query)
import Clckwrks.Page.Acid (GetPageTitle(..))
import Clckwrks.URL   (ClckURL(ViewPage))
import Clckwrks.Page.Types (PageId(..))
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, parseOnly, space, stringCI, try)
import Data.Attoparsec.Combinator (many1, manyTill, skipMany)
import           Data.Text (Text, pack)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HSP
import HSP.HTML (renderAsHTML)
import Web.Routes (showURL)

-- TODO: move to reusable module
parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       stringCI name
       skipMany space
       char '='
       skipMany space

qchar :: Parser Char
qchar = (char '\\' *> anyChar) <|> anyChar

text :: Parser Text
text = pack <$> many1 qchar

qtext :: Parser Text
qtext = pack <$> (char '"' *> manyTill qchar (try $ char '"'))

data PageCmd
    = LinkPage PageId (Maybe Text)
      deriving (Eq, Ord, Show)

pageId :: Parser PageCmd
pageId = LinkPage <$> (parseAttr "id" *> (PageId <$> decimal)) <*> (optional $ parseAttr "title" *> qtext)

parseCmd :: Parser PageCmd
parseCmd = pageId

pageCmd :: (Functor m, MonadIO m) => (ClckURL -> [(Text, Maybe Text)] -> Text) -> Text -> ClckT url m Builder
pageCmd showURLFn txt =
    do let mi = parseOnly parseCmd txt
       case mi of
         (Left e) ->
               return $ B.fromString e -- FIXME: format the error more nicely or something?

         (Right cmd) ->
             case cmd of
               (LinkPage pid mTitle) ->
                   do ttl <- case mTitle of
                               (Just t) -> return t
                               Nothing  -> do mttl <- query (GetPageTitle pid)
                                              case mttl of
                                                Nothing -> return $ pack "Untitled"
                                                (Just ttl) -> return ttl
                      html <- unXMLGenT $ <a href=(showURLFn (ViewPage pid) [])><% ttl %></a>
                      return $ B.fromString $ concat $ lines $ renderAsHTML html
