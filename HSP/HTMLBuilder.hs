-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.HTML
-- Copyright   :  (c) Niklas Broberg, Jeremy Shaw 2008
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Attempt to render XHTML as well-formed HTML 4.01:
--
--  1. no short tags are used, e.g., \<script\>\<\/script\> instead of \<script \/\>
--
--  2. the end tag is forbidden for some elements, for these we:
--
--    * render only the open tag, e.g., \<br\>
--
--    * throw an error if the tag contains children
--
--  3. optional end tags are always rendered
--
-- Currently no validation is performed.
-----------------------------------------------------------------------------
module HSP.HTMLBuilder (
                  Style(..)
                 -- * Functions
                , renderAsHTML
                , htmlEscapeChars
                ) where

import Data.List
import Data.Monoid (mappend, mconcat, mempty)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (Builder, fromString, singleton, toLazyText)
import HSP.XML
import HSP.XML.PCDATA(escaper)

-- | Pretty-prints HTML values.
--
-- Error Handling:
--
-- Some tags (such as img) can not contain children in HTML. However,
-- there is nothing to stop the caller from passing in XML which
-- contains an img tag with children. There are three basic ways to
-- handle this:
--
--  1. drop the bogus children silently
--
--  2. call 'error' \/ raise an exception
--
--  3. render the img tag with children -- even though it is invalid
--
-- Currently we are taking approach #3, since no other attempts to
-- validate the data are made in this function. Instead, you can run
-- the output through a full HTML validator to detect the errors.
--
-- #1 seems like a poor choice, since it makes is easy to overlook the
-- fact that data went missing.
--
-- We could raising errors, but you have to be in the IO monad to
-- catch them. Also, you have to use evaluate if you want to check for
-- errors. This means you can not start sending the page until the
-- whole page has been rendered. And you have to store the whole page
-- in RAM at once. Similar problems occur if we return Either
-- instead. We mostly care about catching errors and showing them in
-- the browser during testing, so perhaps this can be configurable.
--
-- Another solution would be a compile time error if an empty-only
-- tag contained children.
--
-- FIXME: also verify that the domain is correct
--
-- FIXME: what to do if a namespace is encountered

renderAsHTML :: Style -> XML -> Builder
renderAsHTML style xml = renderAsHTML' style 0 xml

data TagType = Open | Close

data Style = Compact | Expanded

renderAsHTML' :: Style -> Int -> XML -> Builder
renderAsHTML' _ _ (CDATA needsEscape cd) = fromString (if needsEscape then (escaper htmlEscapeChars cd) else cd)

renderAsHTML' style n elm@(Element name@(Nothing,nm) attrs children)
    | nm == "area"	= renderTagEmpty children
    | nm == "base"	= renderTagEmpty children
    | nm == "br"        = renderTagEmpty children
    | nm == "col"       = renderTagEmpty children
    | nm == "hr"        = renderTagEmpty children
    | nm == "img"       = renderTagEmpty children
    | nm == "input"     = renderTagEmpty children
    | nm == "link"      = renderTagEmpty children
    | nm == "meta"      = renderTagEmpty children
    | nm == "param"     = renderTagEmpty children
    | nm == "script"    = renderElement style n (Element name attrs (map asCDATA children))
    | nm == "style"     = renderElement style n (Element name attrs (map asCDATA children))
    where
      renderTagEmpty [] = renderTag style Open n name attrs
      renderTagEmpty _ = renderElement style n elm -- this case should not happen in valid HTML
      -- for and script\/style, render text in element as CDATA not PCDATA
      asCDATA :: XML -> XML
      asCDATA (CDATA _ cd) = (CDATA False cd)
      asCDATA o = o -- this case should not happen in valid HTML
renderAsHTML' style n e = renderElement style n e

renderElement :: Style -> Int -> XML -> Builder
renderElement style n (Element name attrs children) =
        let open  = renderTag style Open n name attrs
            cs    = renderChildren n children
            close = renderTag style Close n name []
         in mconcat [open, cs, close]
  where renderChildren :: Int -> Children -> Builder
        renderChildren n' cs = mconcat $ map (renderAsHTML' style (n'+2)) cs
renderElement _ _ _ = error "internal error: renderElement only suports the Element constructor."


renderTag :: Style -> TagType -> Int -> Name -> Attributes -> Builder
renderTag style typ n name attrs =
        let (start,end) = case typ of
                           Open   -> (singleton  '<' , singleton '>')
                           Close  -> (fromString "</", singleton '>')
            nam = showName name
            as  = renderAttrs attrs
         in mconcat [start, nam, as, end]

  where renderAttrs :: Attributes -> Builder
        renderAttrs [] = nl
        renderAttrs attrs' = mconcat [singleton ' ', ats, nl]
          where ats = mconcat $ intersperse (singleton ' ') $ fmap renderAttr attrs'

        renderAttr :: Attribute -> Builder
        renderAttr (MkAttr (nam, (Value needsEscape val))) =
            mconcat [ showName nam, singleton '=', renderAttrVal (if needsEscape then (escaper htmlEscapeChars val) else val)]

        renderAttrVal :: String -> Builder
        renderAttrVal s =
            mconcat [singleton '\"', fromString s, singleton '\"']

        showName (Nothing, s) = fromString s
        showName (Just d, s)  = mconcat [fromString d, singleton ':', fromString s]

        nl =
            case style of
              Compact  -> mempty
              Expanded -> singleton '\n' `mappend` (fromString $ replicate n ' ')

-- This list should be extended.
htmlEscapeChars :: [(Char, String)]
htmlEscapeChars = [
	('&',	"amp"	),
	('\"',	"quot"	),
	('<',	"lt"	),
	('>',	"gt"	)
	]
