module BarthPar.Scrape.Single where


import           Control.Error
import           Control.Monad          (void)
import qualified Data.Text              as T

import           BarthPar.Scrape        (scrapePage)
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Types


scrapeSingle :: FilePath -> VolumeTitle -> T.Text -> MetadataTarget -> FilePath
             -> Script ()
scrapeSingle inputFile vTitle pageTitle meta outputDir =
    toScript True meta $
        void . writePage outputDir =<< scrapePage vTitle pageTitle (Left inputFile)
