{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.XML where


import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Maybe
import qualified Data.Text             as T
import           Prelude               hiding (div)
import           Text.XML
import           Text.XML.Cursor
import           Text.XML.Lens         (nodes)

import           BarthPar.Scrape.Utils


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

filterEl :: MonadPlus m => (Element -> Bool) -> Element -> m Element
filterEl f e
    | f e       = return $ e & over nodes (mapMaybe (filterNode f))
    | otherwise = mzero

filterNode :: MonadPlus m => (Element -> Bool) -> Node -> m Node
filterNode f   (NodeElement     e) = NodeElement <$> filterEl f e
filterNode _ n@(NodeInstruction _) = return n
filterNode _ n@(NodeContent     _) = return n
filterNode _ n@(NodeComment     _) = return n
