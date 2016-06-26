{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module BarthPar.Scrape.Read where


import           Control.Applicative
import           Control.Arrow          ((&&&))
import           Control.Error
import           Control.Lens           hiding ((|>))
import           Control.Monad
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Char
import           Data.Foldable
import qualified Data.List              as L
import           Data.Sequence          ((|>))
import qualified Data.Sequence          as S
import qualified Data.Text              as T
import           Data.Text.Lazy.Builder
import           Data.Text.Read
import           Data.Tuple
import           Prelude                hiding (div, span)
import           Text.Groom
import           Text.XML
import           Text.XML.Cursor        hiding (forceM)
import           Text.XML.Lens          (_Element)

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML
import           BarthPar.Utils         hiding (paragraphs)


makeChapter :: Title -> Title -> [Cursor] -> PureScript (Chapter ContentBlock)
makeChapter vTitle title (h:a:cs) = do
    cloc <- first ("Error parsing volume ID: " ++)
         $  parseChapterLoc vTitle title
    let v  = Header (_locVolume cloc) vTitle
        p' = _locPart cloc
        c  = _locChapter cloc
        c' = Header c title
    abst <-  pure
         .   flip (ContentBlock v p' c' (Header 0 "Abstract") 0) ""
         .   normalizeWrap
         .   render
         .   foldMap cleanText
         <$> forceList "Missing abstract"
         (   a' ^.. traverse . to node . _Element
         )
    paragraphs <- readParagraphs v p' c' cs'
    return $ Chapter v p' abst c title paragraphs
    where
        (a', cs') = if length (h $/ span) == 1
                    then (a $|  abstract, cs)
                    else (h $// excursus, a:cs)

makeChapter vTitle cTitle _ =
    Left $ "Not enough nodes on " ++ T.unpack cTitle ++ show vTitle

type RPState = ( Int
               , Maybe (Int, Paragraph ContentBlock, S.Seq ContentBlock)
               , S.Seq (Paragraph ContentBlock)
               )

readParagraphs :: Header -> Int -> Header -> [Cursor]
               -> PureScript [Paragraph ContentBlock]
readParagraphs v p' ch = fmap finis . foldM step (0, Nothing, S.empty)
    where
        step :: RPState -> Cursor -> PureScript RPState

        step (n, Nothing, ps) c
            | L.null (c $/ spanHead) = do
                let par = Paragraph v p' ch 0 "" []
                return ( succ n
                       , Just (0, par, S.empty)
                       , ps
                       )
            | otherwise = do
                accum <- readParagraph v p' ch c
                return ( succ n
                       , Just accum
                       , ps
                       )

        step (n, Just (n', par, cbs), ps) c
            | L.null (c $/ spanHead) = do
                let ph = uncurry Header $ (_paragraphN &&& _paragraphTitle) par
                cb <- readContent v p' ch ph n' c
                return ( n
                       , Just (succ n', par, cbs |> cb)
                       , ps
                       )

            | otherwise = do
                accum <- readParagraph v p' ch c
                return ( succ n
                       , Just accum
                       , ps |> (  par
                               & paragraphContent
                               .~ toList (S.unstableSort cbs)
                               )
                       )

        finis :: RPState -> [Paragraph ContentBlock]
        finis (_, Nothing, _) = []
        finis (_, Just (_, par, cbs), accum) =
            toList $ accum |> (par & paragraphContent .~ toList cbs)

readParagraph :: Header -> Int -> Header -> Cursor
              -> PureScript (Int, Paragraph ContentBlock, S.Seq ContentBlock)
readParagraph v p' ch c = do
    h  <-  headErr (  "Unable to find paragraph header: "
                   ++ groom (node c)
                   )
       =<< mapM readHeader (c $/ spanHead)
    cb <- readContent v p' ch h 0 c
    return ( 0
           , Paragraph v p' ch (_headerN h) (_headerTitle h) []
           , S.singleton cb
           )

readContent :: Header -> Int -> Header -> Header -> Int -> Cursor
            -> PureScript ContentBlock
readContent v p' ch ph n c = do
    let txt   = normalizeWrap . render . foldMap cleanText
              $ (c $/ p       ) ^.. traverse . to node . _Element
        excur = normalizeWrap . render . foldMap cleanExcursus
              $ (c $/ excursus) ^.. traverse . to node . _Element
    return $ ContentBlock v p' ch ph n txt excur

-- | CD Volume I,1 (§§ 1-12)
parseChapterLoc :: Title -> T.Text -> PureScript ChapterLoc
parseChapterLoc vtitle pageName =
    cloc <$> ( bisequenceA
             . (fromRomanPS `bimap` (fmap fst . decimal . T.drop 1))
             . T.break (== ',')
             . T.takeWhile (not . isSpace)
             $ T.drop 10 vtitle
             )
         <*> parsePageName pageName
    where
        cloc (v, s) = ChapterLoc v s

-- § 1 The Task of Dogmatics.
parsePageName :: T.Text -> PureScript Int
parsePageName = fmap fst . decimal . T.drop 2

readHeader :: Cursor -> PureScript Header
readHeader c = fmap (uncurry Header . swap)
             . sequenceA
             . ((normalize . T.drop 2) `bimap` anyNumberPS)
             . swap
             $ T.break (=='.') title
    where
        title = T.concat $ c $// content

cleanText :: Element -> Builder
cleanText =
    cleanWith $ \e -> not $ isCenter e || isNoteLink e

cleanExcursus :: Element -> Builder
cleanExcursus =
    cleanWith $ \e -> not $ isCenter e || isNoteLink e || isHiddenNote e

cleanWith :: (Element -> Bool) -> Element -> Builder
cleanWith f e =
    foldMap (buildText . NodeElement) (filterEl f e :: Maybe Element)
