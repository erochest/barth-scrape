{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.XML where


import           Control.Arrow
import qualified Data.Text       as T
import           Prelude         hiding (div)
import           Text.XML.Cursor


isContent :: T.Text -> Cursor -> Bool
isContent c e = (== c) . T.concat $ e $// content

tocEntries :: Axis
tocEntries = laxElement "i"

-- | This takes the link text to find and a cursor positioned on the <i>
-- elements surrounding the titles, and it returns a list of titles and
-- links.
tocPair :: T.Text -> Cursor -> [(T.Text, T.Text)]
tocPair link =
    pure . (T.concat *** T.concat) . (childContent &&& (take 1 . tocLinks link))

childContent ::Cursor -> [T.Text]
childContent = child >=> content

tocLinks :: T.Text -> Cursor -> [T.Text]
tocLinks c = followingSibling
             >=> laxElement "a"
             >=> check (isContent c)
             >=> laxAttribute "href"

spanHead :: Axis
spanHead = laxElement "span" >=> attributeIs "class" "head"

excursus :: Axis
excursus = laxElement "span" >=> attributeIs "class" "excursus"

div :: Axis
div = laxElement "div"

abstract :: Axis
abstract = div >=> attributeIs "type" "abstract"

p :: Axis
p = laxElement "p"

tinyurl :: Axis
tinyurl = div >=> attributeIs "id" "tinyurl"
