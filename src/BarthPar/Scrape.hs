{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module BarthPar.Scrape where


import           Control.Error
import           Control.Monad
import           Data.Monoid
import qualified Data.Text               as T
import           Network.URI
import           Text.XML.Cursor

import           BarthPar.Scrape.Network
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.Scrape.XML


-- TODO: write the metadata to a json file alongside the content file,
-- or a yaml block


scrape :: Bool -> FilePath -> String -> Script ()
scrape clean outputDir rootUrl = do
  when clean $
       cleanOutputs outputDir
  case parseURI rootUrl of
    Just uri -> scrapeTOC uri >>= mapM_ (writePage outputDir)
    Nothing  -> return ()

scrapeTOC :: URI -> Script [Page]
scrapeTOC uri =
    fmap concat . smapConcurrently (uncurry scrapeVolumePage)
        =<< scrapeTOCPage uri "Table of Contents" (T.isPrefixOf "CD ")

scrapeTOCPage :: URI
              -- ^ The ToC's URI to download
              -> T.Text
              -- ^ The content of A tags to look for.
              -> (T.Text -> Bool)
              -- ^ A predicate to find which links to move to next.
              -> Script [(T.Text, URI)]
                 -- ^ A list of A tag contents and @hrefs.
scrapeTOCPage uri title p = do
    doc <- dl (Just title) uri
    mapMaybe (sequenceA . fmap (appendUri uri)) . filter (p . fst)
                 <$> dumpPrint ("scrapeTOCPage \"" ++ T.unpack title ++ "\"")
                         (fromDocument doc $// tocEntries >=> tocPair title)

scrapeVolumePage :: T.Text -> URI -> Script [Page]
scrapeVolumePage volName uri =
    smapConcurrently (uncurry (scrapePage volName))
        =<< scrapeTOCPage uri "View Text" (T.isPrefixOf "§ ")

scrapePage :: VolumeTitle -> T.Text -> URI -> Script Page
scrapePage volName pageName uri = do
    doc <- dl (Just $ volName <> " | " <> pageName) uri
    writeDoc "output/doc.html" doc
    let nds = fromDocument doc
              $// laxElement "div"
              >=> attributeIs "id" "tinyurl"
              >=> followingSibling
              >=> laxElement "div"
    void . writeNodes "output/page.html" $ map node nds
    makePage volName nds
    undefined
