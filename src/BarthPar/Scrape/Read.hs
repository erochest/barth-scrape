{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Read where


import           Control.Error
import           Data.Bitraversable
import           Data.Char
import qualified Data.Text             as T
import           Data.Tuple
import           Prelude               hiding (div)
import           Text.XML.Cursor
import           Text.XML.Lens         hiding (attributeIs)

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


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
                     (concatMap ($// spanHead >=> child >=> content) cs)
             <*> forceS "Unable to find abstract"
                     (concatMap (toListOf _Element . node) abstract')
             <*> mapM (fmap cleanSection . readSection)
                     (concatMap ($| followingSibling &/ div) abstract')
    where
      abstract' = concatMap ($| abstract) cs

readSectionHead :: Cursor -> Script SectionHeader
readSectionHead c = fmap (swap . fmap fst)
                    . sequenceA
                    . (T.drop 2 `bimap` decimalS)
                    . swap
                    . T.break (=='.')
                    . T.concat
                    $ c $// content

readSection :: Cursor -> Script Section
readSection c =
    Section <$> (listToMaybe <$> mapM readSectionHead (c $/ spanHead))
            <*> pure (c $/ p >=> toListOf _Element . node)
            <*> (   pure
                .   listToMaybe
                $   c
                $/  excursus
                >=> toListOf _Element . node
                )

-- TODO:
cleanSection :: Section -> Section
cleanSection = id
