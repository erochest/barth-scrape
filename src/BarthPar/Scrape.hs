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
import           Text.XML.Cursor

import           BarthPar.Scrape.Network
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Read
import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


scrape :: Bool -> FilePath -> Either FilePath String -> Script ()
scrape clean outputDir inputRoot = do
  when clean $
       cleanOutputs outputDir
  input <- case fmap parseURI inputRoot of
             Right (Just uri) -> return $ Right uri
             Left filePath    -> return $ Left filePath
             Right Nothing    -> throwE "Invalid root URL."
  scrapeTOC input >>= mapM_ (writePage outputDir)

scrapeTOC :: InputSource -> Script [Page]
scrapeTOC input =
    fmap concat . smapConcurrently (uncurry scrapeVolumePage)
        =<< scrapeTOCPage input "Table of Contents" (T.isPrefixOf "CD ")

scrapeTOCPage :: InputSource
              -- ^ The ToC's URI to download
              -> T.Text
              -- ^ The content of A tags to look for.
              -> (T.Text -> Bool)
              -- ^ A predicate to find which links to move to next.
              -> Script [(T.Text, InputSource)]
                 -- ^ A list of A tag contents and @hrefs.
scrapeTOCPage input title f =
    mapMaybe (sequenceA . fmap (appendInput input))
                 . filter (f . fst)
                 . ($// tocEntries >=> tocPair title)
                 . fromDocument
                 <$> dl (Just title) input

scrapeVolumePage :: T.Text -> InputSource -> Script [Page]
scrapeVolumePage volName input =
    smapConcurrently (uncurry (scrapePage volName))
        =<< scrapeTOCPage input "View Text" (T.isPrefixOf "§ ")

scrapePage :: VolumeTitle -> T.Text -> InputSource -> Script Page
scrapePage volName pageName input = do
    doc <- dl (Just $ volName <> " | " <> pageName) input
    writeDoc "output/doc.html" doc
    let nds = fromDocument doc $// tinyurl >=> followingSibling >=> div
    void . writeNodes "output/page.html" $ map node nds
    makePage volName nds
