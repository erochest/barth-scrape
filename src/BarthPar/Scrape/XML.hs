{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.XML where


import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Data.Bitraversable
import           Data.Char
import qualified Data.Text             as T
import           Data.Tuple
import           Text.XML.Cursor
import           Text.XML.Lens         hiding (attributeIs)

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils


parseVolumeID :: VolumeTitle -> Script VolumeID
parseVolumeID vtitle
    -- CD Volume I,1 (§§ 1-12)
    | "CD Volume " `T.isPrefixOf` vtitle =
        fmap (uncurry Volume)
        . bisequenceA
        . (fromRomanS `bimap` (fmap fst . decimalS . T.drop 1))
        . T.break (==',')
        . T.takeWhile (not . isSpace)
        $ T.drop 10 vtitle

    -- I. General Index
    | otherwise =
        fmap Appendix . fromRomanS $ T.takeWhile (not . (=='.')) vtitle


makePage :: VolumeTitle -> [Cursor] -> Script Page
makePage vtitle cs =
    Page <$> parseVolumeID vtitle
             <*> forceS "Unable to find VolumeTitle"
                     (concatMap ($// laxElement "span"
                                >=> attributeIs "class" "head"
                                >=> child
                                >=> content
                                ) cs)
             <*> forceS "Unable to find abstract"
                 (concatMap (toListOf _Element . node) abstract)
             <*> mapM (fmap cleanSection . readSection)
                     (concatMap
                      ($| followingSibling &/ laxElement "div") abstract)
    where
      abstract = concatMap
                 ($| laxElement "div" >=> attributeIs "type" "abstract")
                 cs

readSectionHead :: Cursor -> Script SectionHeader
readSectionHead c = fmap (swap . fmap fst)
                    . sequenceA
                    . (T.drop 2 *** decimalS)
                    . swap
                    . T.break (=='.')
                    . T.concat
                    $ c $// content

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

readSection :: Cursor -> Script Section
readSection c =
    Section <$> (listToMaybe
                <$> mapM readSectionHead
                        (c $/ laxElement "span" >=> attributeIs "class" "head"))
            <*> pure (c $/ laxElement "p" >=> toListOf _Element . node)
            <*> (   pure
                .   listToMaybe
                $   c
                $/  laxElement "span"
                >=> attributeIs "class" "excursus"
                >=> toListOf _Element . node
                )

-- TODO:
cleanSection :: Section -> Section
cleanSection = id
