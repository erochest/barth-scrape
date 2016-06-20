module BarthPar.Scrape.Single where


import           Control.Error
import           Control.Monad
import qualified Data.Text              as T
import           System.FilePath

import           BarthPar.Scrape        (scrapePage)
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Types


scrapeSingle :: FilePath        -- ^ The input file.
             -> VolumeTitle     -- ^ The Volume title for this file.
             -> T.Text          -- ^ The page title for this file.
             -> MetadataTarget  -- ^ How to output the metadata.
             -> Chunking        -- ^ How to chunk the output.
             -> FilePath        -- ^ The output directory.
             -> Script ()
scrapeSingle inputFile vTitle pageTitle meta _chunking outputDir =
    toScript True meta $
        when (meta == TargetCSV) . writeCsv (outputDir </> "corpus.csv")
            =<< writePage outputDir
            =<< scrapePage vTitle pageTitle (Left inputFile)
