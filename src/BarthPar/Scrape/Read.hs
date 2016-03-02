{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Read where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Bitraversable
import           Data.Char
import           Data.Foldable
import qualified Data.List             as L
import qualified Data.Text             as T
import           Data.Tuple
import           Prelude               hiding (div)
import           Text.XML.Cursor
import           Text.XML.Lens         hiding (attributeIs)

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


parseVolumeID :: VolumeTitle -> T.Text -> Script VolumeID
parseVolumeID vtitle pageName
    -- CD Volume I,1 (§§ 1-12)
    | "CD Volume " `T.isPrefixOf` vtitle =
        volume <$> ( bisequenceA
                   . (fromRomanS `bimap` (fmap fst . decimalS . T.drop 1))
                   . T.break (== ',')
                   . T.takeWhile (not . isSpace)
                   $ T.drop 10 vtitle
                   )
                   <*> parsePageName pageName

    -- I. General Index
    | otherwise =
        fmap Appendix . fromRomanS $ T.takeWhile (not . (=='.')) vtitle

    where
      volume (v, s) = Volume v s

-- § 1 The Task of Dogmatics.
parsePageName :: T.Text -> Script Int
parsePageName page = fst <$> decimalS (T.drop 2 page)

readChildSections :: [Cursor] -> Script [(Int, Section)]
readChildSections = fmap (zip [1..] . L.reverse . snd)
                    . foldlM step (Nothing, [])
    where
      step :: (Maybe SectionHeader, [Section]) -> Cursor
           -> Script (Maybe SectionHeader, [Section])
      step (sh, sections) c = do
        s <- cleanSection <$> readSection c
        let s' = s & over sectionHead (<|> sh)
        return ( _sectionHead s'
               , s' : sections
               )

makePage :: VolumeTitle -> T.Text -> [Cursor] -> Script Page
makePage vtitle pageName cs =
    (Page <$> vId
    <*> forceS "Unable to find VolumeTitle"
            (concatMap ($// spanHead >=> child >=> content) cs)
    <*> forceS "Unable to find abstract"
            (concatMap (toListOf _Element . node) abstract')
    <*> readChildSections (concatMap ($| followingSibling) abstract'))
    <|>
    (Page <$> vId
    <*> forceS "Unable to find VolumeTitle"
            (concatMap ($// spanHead >=> child >=> content) cs)
    <*> forceS "Unable to find excursus/abstract"
            (concatMap ($// spanHead
                       >=> followingSibling
                       >=> excursus
                       >=> toListOf _Element
                       . node
                       )
             cs)
    <*> readChildSections
            (concatMap ($| spanHead >=> parent >=> followingSibling &/ div) cs))
    where
      vId = parseVolumeID vtitle pageName
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

-- TODO: Remove "[note]"
cleanSection :: Section -> Section
cleanSection =
    over sectionContent (>>= filterEl notCenter)
             . over sectionExcursus (>>= filterEl notCenter)
    where
      notCenter :: Element -> Bool
      notCenter = not . (== "center") . elementName
