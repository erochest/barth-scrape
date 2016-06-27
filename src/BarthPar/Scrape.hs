{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module BarthPar.Scrape where


import           Control.Arrow           ((&&&))
import           Control.Error
import           Control.Monad
import           Data.Function
import qualified Data.List               as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Network.URI
import           Prelude                 hiding (div)
import           System.FilePath
import           System.IO
import           Text.XML.Cursor

import qualified BarthPar.Scrape.Chunks  as Chunks
import           BarthPar.Scrape.Network
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Read
import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


scrape :: Bool                      -- ^ Print debugging output?
       -> Bool                      -- ^ Clean out the output directory first?
       -> MetadataTarget            -- ^ How to output the metadata.
       -> Chunking                  -- ^ How to chunk the output.
       -> FilePath                  -- ^ The output directory.
       -> Either FilePath String    -- ^ The input.
       -> Script ()
scrape debug clean mdata chunkings outputDir inputRoot = toScript debug mdata $ do
    when clean $
        cleanOutputs outputDir
    input <- case fmap parseURI inputRoot of
                Right (Just uri) -> return $ Right uri
                Left filePath    -> return $ Left filePath
                Right Nothing    -> throwS "Invalid root URL."
    outputs <- Chunks.chunk chunkings mdata <$> (dumpHtml "CORPUS" =<< scrapeTOC input)
    mapM_ (writeOutput outputDir) outputs
    when (mdata == TargetCSV) $
        writeCsv (outputDir </> "corpus.csv") $ mapMaybe _outputCsv outputs

scrapeTOC :: InputSource -> Scrape (Corpus ContentBlock)
scrapeTOC input =
    fmap (Corpus . wrapVolume . concat)
        .   smapConcurrently (uncurry scrapePartPage)
        =<< scrapeTOCPage input "Table of Contents" (T.isPrefixOf "CD ")

scrapeTOCPage :: InputSource
              -- ^ The ToC's URI to download
              -> T.Text
              -- ^ The content of A tags to look for.
              -> (T.Text -> Bool)
              -- ^ A predicate to find which links to move to next.
              -> Scrape [(T.Text, InputSource)]
                 -- ^ A list of A tag contents and @hrefs.
scrapeTOCPage input title f =
    mapMaybe (sequenceA . fmap (appendInput input))
                 . filter (f . fst)
                 . ($// tocEntries >=> tocPair title)
                 . fromDocument
                 <$> dl (Just $ "TOC: " <> title) input

scrapePartPage :: T.Text -> InputSource -> Scrape [Part ContentBlock]
scrapePartPage volName input =
    fmap groupParts . smapConcurrently (uncurry (scrapePage volName))
        =<< scrapeTOCPage input "View Text" (T.isPrefixOf "§ ")

scrapePage :: Title -> T.Text -> InputSource -> Scrape (Chapter ContentBlock)
scrapePage volName pageName input = do
    doc <- dl (Just $ "PAGE: " <> volName <> " | " <> pageName) input
    dumpPage "page" doc
        >>= scrapeIO . maybe (return ()) (hPutStrLn stderr . (">>> " ++)) . fst
    let nds = fromDocument doc $// tinyurl >=> followingSibling >=> div
    io $ makeChapter volName pageName nds

wrapVolume :: Eq a => [Part a] -> [Volume a]
wrapVolume = mapMaybe volume
           . L.groupBy ((==) `on` (_headerN . _partVolume))
           . L.sortBy (comparing (_headerN . _partVolume))
    where
        volume :: Eq a => [Part a] -> Maybe (Volume a)
        volume ps@(Part{_partVolume=(Header vn vt)}:_) =
            Just . Volume vn vt $ L.sort ps
        volume _ = Nothing

groupParts :: Eq a => [Chapter a] -> [Part a]
groupParts = mapMaybe part
           . L.groupBy ((==) `on` _chapterPart)
           . L.sortBy (comparing (_chapterVolume &&& _chapterPart))
    where
        part :: Eq a => [Chapter a] -> Maybe (Part a)
        part cs@(Chapter{_chapterVolume=v,_chapterPart=pt}:_) =
            Just . Part v pt $ L.sort cs
        part _ = Nothing
