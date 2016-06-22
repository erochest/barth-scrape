{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module BarthPar.Scrape where


import           Control.Error
import           Control.Monad
import           Data.Monoid
import qualified Data.Text               as T
import           Network.URI
import           Prelude                 hiding (div)
import           System.FilePath
import           Text.XML.Cursor

import qualified BarthPar.Scrape.Chunks  as Chunks
import           BarthPar.Scrape.Network
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Types
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
  outputs <- Chunks.chunk chunkings mdata <$> scrapeTOC input
  mapM_ (writeOutput outputDir) outputs
  when (mdata == TargetCSV) $
    writeCsv (outputDir </> "corpus.csv") $ mapMaybe _outputCsv outputs

scrapeTOC :: InputSource -> Scrape (Corpus ContentBlock)
scrapeTOC _input = undefined
    {-
     - fmap concat . smapConcurrently (uncurry scrapeVolumePage)
     -     =<< scrapeTOCPage input "Table of Contents" (T.isPrefixOf "CD ")
     -}

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

scrapeVolumePage :: T.Text -> InputSource -> Scrape [Volume ContentBlock]
scrapeVolumePage _volName _input = undefined
    {-
     - smapConcurrently (uncurry (scrapePage volName))
     -     =<< scrapeTOCPage input "View Text" (T.isPrefixOf "§ ")
     -}

scrapePage :: Title -> T.Text -> InputSource -> Scrape (Chapter ContentBlock)
scrapePage _volName _pageName _input = undefined
    {-
     - doc <- dl (Just $ "PAGE: " <> volName <> " | " <> pageName) input
     - dumpPage "page" doc
     -     >>= scrapeIO . maybe (return ()) (hPutStrLn stderr . (">>> " ++)) . fst
     - let nds = fromDocument doc $// tinyurl >=> followingSibling >=> div
     - io $ makePage volName pageName nds
     -}
