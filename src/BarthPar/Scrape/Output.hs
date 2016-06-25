{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Scrape.Output where


import           Control.Lens
import           Control.Monad
import qualified Data.Aeson              as A
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as BL
import           Data.Csv                hiding (Only, encode)
import qualified Data.Text               as T
import           Data.Text.Format        hiding (build)
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Yaml.Aeson
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Groom
import           Text.XML
import qualified Text.XML                as XML

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils


findFileName :: Format -> Int -> Scrape FilePath
findFileName template n = do
  exists <- scrapeIO $ doesFileExist filename
  if exists
  then findFileName template $ succ n
  else return filename
  where
    filename = TL.unpack . format template . Only $ left 3 '0' n

cleanOutputs :: FilePath -> Scrape ()
cleanOutputs outputDir = do
  ensureClean outputDir
  debugging $
       ensureClean "dump"
  where
    ensureClean :: FilePath -> Scrape ()
    ensureClean dirname = scrapeIO $ do
                            exists <- doesDirectoryExist dirname
                            when exists $
                                 removeDirectoryRecursive dirname
                            createDirectoryIfMissing True dirname

dumpPage :: String -> XML.Document -> Scrape (Maybe String, XML.Document)
dumpPage source doc = debugging' (Nothing, doc) $ do
  filename <- findFileName "dump/{}-page.html" 0
  scrapeIO $ withFile filename WriteMode $ \f -> do
                     TIO.hPutStrLn f "<!--"
                     hprint f "source: {}\n" $ Only source
                     TIO.hPutStrLn f "-->\n"
                     TIO.hPutStr f . TL.toStrict
                            $ XML.renderText (XML.def
                                             { rsPretty = True
                                             }) doc
  return (Just filename, doc)

dumpPrint :: Show a => String -> a -> Scrape a
dumpPrint source x = debugging' x $ do
  filename <- findFileName "dump/{}-show.html" 0
  scrapeIO $ withFile filename WriteMode $ \f -> do
                       TIO.hPutStrLn f "---"
                       hprint f "source: {}\n" $ Only source
                       TIO.hPutStrLn f "---\n"
                       TIO.hPutStrLn f . T.pack $ groom x
  return x

dumpText :: String -> T.Text -> Scrape T.Text
dumpText source x = debugging' x $ do
  filename <- findFileName "dump/{}-text.html" 0
  scrapeIO . TIO.writeFile filename $ mconcat
               [ "---\n"
               , T.pack source, "\n"
               , "===\n"
               , x
               ]
  return x

dumpEl :: String -> XML.Element -> Scrape XML.Element
dumpEl source e = debugging' e $ do
    void . dumpPage source $ XML.Document (XML.Prologue [] Nothing []) e []
    return e

writeOutput :: FilePath -> Output -> Scrape ()
writeOutput outputDir Output{..} = do
  mdTarget <- view scrapeMetadata
  scrapeIO . withFile (outputDir </> _outputFilePath) WriteMode $
               \h -> do
                 case mdTarget of
                   TargetNone -> return ()
                   TargetYamlHeader -> B8.hPut h (encode _outputMetadata)
                                       >> B8.hPutStrLn h "\n---\n\n"
                   TargetJSON -> BL.writeFile (_outputFilePath -<.> "json")
                                 $ A.encode _outputMetadata
                   TargetCSV  -> return ()
                 B8.hPutStr h . BL.toStrict . encodeUtf8 $ toLazyText _outputContent

wrapNodes :: [XML.Node] -> XML.Document
wrapNodes nds =
    XML.Document
       (XML.Prologue [] Nothing [])
       (XML.Element "div" mempty nds)
       []

writeNodes :: FilePath -> [XML.Node] -> Scrape XML.Document
writeNodes filename nds = do
    writeDoc filename doc
    return doc
    where
      doc = wrapNodes nds

writeDoc :: FilePath -> XML.Document -> Scrape ()
writeDoc filename doc =
    scrapeIO $ XML.writeFile (XML.def { XML.rsPretty = True }) filename doc

writePairs :: FilePath -> [(T.Text, T.Text)] -> Scrape ()
writePairs filename = scrapeIO
                      . TIO.writeFile filename
                      . T.unlines
                      . map (\(a, b) -> T.concat [a, "\t", b])

writeCsv :: FilePath -> [CsvOutput] -> Scrape ()
writeCsv csvFilePath cs@(c:_) =
    scrapeIO
        . BL.writeFile csvFilePath
        . encodeByName (_csvHeader c)
        $ map _csvRecord cs
writeCsv _ [] = return ()
