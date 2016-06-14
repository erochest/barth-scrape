{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module BarthPar.Scrape where


import           Control.Error
import           Control.Monad
import qualified Data.ByteString.Lazy    as BL
import           Data.Csv
import qualified Data.List               as L
import           Data.Monoid
import qualified Data.Text               as T
import           Network.URI
import           Prelude                 hiding (div)
import           System.FilePath
import           System.IO
import           Text.XML.Cursor

import           BarthPar.Scrape.Network
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Read
import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


scrape :: Bool -> Bool -> MetadataTarget -> FilePath -> Either FilePath String
       -> Script ()
scrape debug clean mdata outputDir inputRoot = toScript debug mdata $ do
  when clean $
       cleanOutputs outputDir
  input <- case fmap parseURI inputRoot of
             Right (Just uri) -> return $ Right uri
             Left filePath    -> return $ Left filePath
             Right Nothing    -> throwS "Invalid root URL."
  pages <- mapM (writePage outputDir) =<< scrapeTOC input
  -- TODO: refactor this and share with Single.hs
  when (mdata == TargetCSV) $
    let csvFile = outputDir </> "corpus.csv"
    in  scrapeIO . BL.writeFile csvFile
                 . encodeDefaultOrderedByName
                 . L.sort
                 $ concat pages

scrapeTOC :: InputSource -> Scrape [Page]
scrapeTOC input =
    fmap concat . smapConcurrently (uncurry scrapeVolumePage)
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

scrapeVolumePage :: T.Text -> InputSource -> Scrape [Page]
scrapeVolumePage volName input =
    smapConcurrently (uncurry (scrapePage volName))
        =<< scrapeTOCPage input "View Text" (T.isPrefixOf "§ ")

scrapePage :: VolumeTitle -> T.Text -> InputSource -> Scrape Page
scrapePage volName pageName input = do
    doc <- dl (Just $ "PAGE: " <> volName <> " | " <> pageName) input
    (dumpFile, _) <- dumpPage "page" doc
    scrapeIO . hPutStrLn stderr $ ">>> " ++ dumpFile
    let nds = fromDocument doc $// tinyurl >=> followingSibling >=> div
    io $ makePage volName pageName nds
