{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module BarthPar.Scrape.Read where


import           Control.Applicative
import           Control.Error
import           Control.Lens           hiding ((<|))
import           Control.Monad
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Char
import           Data.Foldable
import qualified Data.Map.Lazy          as M
import           Data.Sequence          ((<|))
import qualified Data.Sequence          as S
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Read
import           Data.Tuple
import           Prelude                hiding (div, span)
import           Text.XML.Cursor        hiding (forceM)
import           Text.XML.Lens          hiding (attributeIs, (<|))

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML
import           BarthPar.Utils


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
        fmap Appendix . fromRomanPS $ T.takeWhile (/='.') vtitle

    where
      volume (v, s) = Volume v s

-- § 1 The Task of Dogmatics.
parsePageName :: T.Text -> PureScript Int
parsePageName = fmap fst . decimal . T.drop 2

readChildSections :: [Cursor] -> PureScript [Section]
readChildSections = fmap (toList . thd) . foldlM step (0, Nothing, S.empty)
    where
        thd :: (a, b, c) -> c
        thd (_, _, x) = x

        step :: (Int, Maybe SectionHeader, S.Seq Section) -> Cursor
             -> PureScript (Int, Maybe SectionHeader, S.Seq Section)
        step (n, sh, sections) c = do
            s <- cleanSection <$> readSection n c
            let s' = s & over sectionHead (<|> sh)
            return ( succ n
                   , _sectionHead s'
                   , s' <| sections
                   )

makePage :: VolumeTitle -> T.Text -> [Cursor] -> PureScript Page
makePage vtitle pageName (h:a:cs) =
    Page <$> first ("Error parsing volume ID: " ++)
         (   parseVolumeID vtitle pageName)
         <*> forceM "Missing VolumeTitle"
         (  h $// (spanHead >=> child      >=> content))
         <*> forceM "Missing Abstract" a'
         <*> readChildSections cs'
    where
        (a', cs') = if length (h $/ span) == 1
                    then (buildNodes $ a $|  abstract, cs)
                    else (buildNodes $ h $// excursus, a:cs)

        buildNodes :: [Cursor] -> [T.Text]
        buildNodes = wrapL
                   . normalize
                   . TL.toStrict
                   . toLazyText
                   . foldMap (buildText . node)

        wrapL :: T.Text -> [T.Text]
        wrapL t | T.null t  = []
                | otherwise = [t]

makePage vtitle pageName _ =
    Left $ "Not enough nodes on " ++ T.unpack pageName ++ show vtitle

readSectionHead :: Cursor -> PureScript SectionHeader
readSectionHead c = fmap swap
                    . sequenceA
                    . (T.drop 2 `bimap` anyNumberPS)
                    . swap
                    . T.break (=='.')
                    . T.concat
                    $ c $// content

readSection :: Int -> Cursor -> PureScript Section
readSection n c =
    Section n
            <$> (listToMaybe <$> mapM readSectionHead (c $/ spanHead))
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
             . over sectionExcursus (>>= filterEl fEx)
    where
      center :: Element -> Bool
      center = (== "center") . view name

      noteLink :: Element -> Bool
      noteLink e = e ^. name == "a" && e ^. text == "[note]"

      f :: Element -> Bool
      f e = not $ center e || noteLink e

      fEx :: Element -> Bool
      fEx e = not $  center e
                  || noteLink e
                  || Just "hiddennote" == M.lookup "class" (elementAttributes e)
