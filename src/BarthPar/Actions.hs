{-# LANGUAGE RecordWildCards #-}


module BarthPar.Actions where


import           Control.Error
import qualified Data.ByteString.Lazy as BL
import           Data.Function
import qualified Data.List            as L
import           Data.Ord
import qualified Data.Vector          as V

import           BarthPar.Data
import           BarthPar.Graph
import           BarthPar.Scrape
import           BarthPar.Scrape.Single
import           BarthPar.Types


action :: Actions -> Script ()

action CsvToJson{..} = do
    byWord <-  ExceptT
           $   fmap ( L.groupBy ((==) `on` getWord)
                    . L.sortBy (comparing getWord)
                    . V.toList
                    )
           .   splitTabs
           <$> BL.readFile jsonInputFile
    let (nodes, tokenIndex) = makeNodes byWord
    lazyWriteNetwork jsonChunkSize jsonOutputFile
        . Network nodes
        $ makeLinks tokenIndex byWord

action Scrape{..} = scrape scrapeDebug scrapeClean scrapeMetadata scrapeChunks
                    scrapeOutputDir scrapeInput

action ScrapePage{..} = scrapeSingle pageFile pageVolume pageTitle pageMetadata
                        pageChunks pageOutputDir
