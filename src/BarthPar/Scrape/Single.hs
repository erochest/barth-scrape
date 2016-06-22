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
        outputs <-  Chunks.chunk chunkings meta . wrapCorpus
                <$> scrapePage vTitle pageTitle (Left inputFile)
        mapM_ (writeOutput outputDir) outputs
        when (meta == TargetCSV) $
            writeCsv (outputDir </> "corpus.csv") $ mapMaybe _outputCsv outputs

wrapCorpus :: Chapter a -> Corpus a
wrapCorpus = undefined
