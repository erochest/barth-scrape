module BarthPar.Scrape.Single where


import           Control.Error
import           Control.Monad
import qualified Data.Text              as T
import           System.FilePath

import           BarthPar.Scrape        (scrapePage)
import qualified BarthPar.Scrape.Chunks as Chunks
import           BarthPar.Scrape.Output
import           BarthPar.Scrape.Types


scrapeSingle :: FilePath        -- ^ The input file.
             -> Title           -- ^ The Volume title for this file.
             -> T.Text          -- ^ The page title for this file.
             -> MetadataTarget  -- ^ How to output the metadata.
             -> Chunking        -- ^ How to chunk the output.
             -> FilePath        -- ^ The output directory.
             -> Script ()
scrapeSingle inputFile vTitle pageTitle meta chunkings outputDir =
    toScript True meta $ do
        outputs <-  Chunks.chunk chunkings meta . wrapCorpus vTitle
                <$> scrapePage vTitle pageTitle (Left inputFile)
        mapM_ (writeOutput outputDir) outputs
        when (meta == TargetCSV) $
            writeCsv (outputDir </> "corpus.csv") $ mapMaybe _outputCsv outputs

wrapCorpus :: Title -> Chapter a -> Corpus a
wrapCorpus vTitle =
    Corpus . pure . Volume 1 vTitle . pure . Part (Header 1  vTitle) 1 . pure
