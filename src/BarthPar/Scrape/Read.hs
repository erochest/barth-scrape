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
import           Data.Text.Read
import           Data.Tuple
import           Prelude               hiding (div)
import           Text.XML.Cursor
import           Text.XML.Lens         hiding (attributeIs)

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


parseVolumeID :: VolumeTitle -> T.Text -> PureScript VolumeID
parseVolumeID vtitle pageName
    -- CD Volume I,1 (§§ 1-12)
    | "CD Volume " `T.isPrefixOf` vtitle =
        volume <$> ( bisequenceA
                   . (fromRomanPS `bimap` (fmap fst . decimal . T.drop 1))
                   . T.break (== ',')
                   . T.takeWhile (not . isSpace)
                   $ T.drop 10 vtitle
                   )
                   <*> parsePageName pageName

    -- I. General Index
    | otherwise =
        fmap Appendix . fromRomanPS $ T.takeWhile (not . (=='.')) vtitle

    where
      volume (v, s) = Volume v s

-- § 1 The Task of Dogmatics.
parsePageName :: T.Text -> PureScript Int
parsePageName = fmap fst . decimal . T.drop 2

readChildSections :: [Cursor] -> PureScript [(Int, Section)]
readChildSections = fmap (zip [1..] . L.reverse . snd)
                    . foldlM step (Nothing, [])
    where
      step :: (Maybe SectionHeader, [Section]) -> Cursor
           -> PureScript (Maybe SectionHeader, [Section])
      step (sh, sections) c = do
        s <- cleanSection <$> readSection c
        let s' = s & over sectionHead (<|> sh)
        return ( _sectionHead s'
               , s' : sections
               )

makePage :: VolumeTitle -> T.Text -> [Cursor] -> PureScript Page
makePage vtitle pageName cs =
    (Page <$> vId
    <*> forcePS "Unable to find VolumeTitle"
            (concatMap ($// spanHead >=> child >=> content) cs)
    <*> forcePS "Unable to find abstract"
            (concatMap (toListOf _Element . node) abstract')
    <*> readChildSections (concatMap ($| followingSibling) abstract'))
    <|>
    (Page <$> vId
    <*> forcePS "Unable to find VolumeTitle"
            (concatMap ($// spanHead >=> child >=> content) cs)
    <*> forcePS "Unable to find excursus/abstract"
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

readSectionHead :: Cursor -> PureScript SectionHeader
readSectionHead c = fmap (swap . fmap fst)
                    . sequenceA
                    . (T.drop 2 `bimap` decimal)
                    . swap
                    . T.break (=='.')
                    . T.concat
                    $ c $// content

readSection :: Cursor -> PureScript Section
readSection c =
    Section <$> (listToMaybe <$> mapM readSectionHead (c $/ spanHead))
            <*> pure (c $/ p >=> toListOf _Element . node)
            <*> (   pure
                .   listToMaybe
                $   c
                $/  excursus
                >=> toListOf _Element . node
                )

-- TODO: Think I can use Control.Lens.Plated here
-- TODO: Replace Cursor navigation with lenses
cleanSection :: Section -> Section
cleanSection =
    over sectionContent (>>= filterEl f)
             . over sectionExcursus (>>= filterEl f)
    where
      center :: Element -> Bool
      center = (== "center") . view name

      noteLink :: Element -> Bool
      noteLink e = e ^. name == "a" && e ^. text == "[note]"

      f :: Element -> Bool
      f e = not $ center e || noteLink e
