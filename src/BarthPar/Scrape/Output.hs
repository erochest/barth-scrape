{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Scrape.Output where


import           Control.Lens
import           Control.Monad
import qualified Data.Aeson             as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               hiding (Only, encode)
import qualified Data.HashMap.Strict    as M
import qualified Data.List              as L
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Format       hiding (build)
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO      as TLIO
import           Data.Yaml.Aeson
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Groom
import           Text.XML
import qualified Text.XML               as XML

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

dumpPage :: String -> XML.Document -> Scrape (String, XML.Document)
dumpPage source doc = debugging' ("", doc) $ do
  filename <- findFileName "dump/{}-page.html" 0
  scrapeIO $ withFile filename WriteMode $ \f -> do
                     TIO.hPutStrLn f "<!--"
                     hprint f "source: {}\n" $ Only source
                     TIO.hPutStrLn f "-->\n"
                     TIO.hPutStr f . TL.toStrict
                            $ XML.renderText (XML.def
                                             { rsPretty = True
                                             }) doc
  return (filename, doc)

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

writeOutput :: Output -> Scrape ()
writeOutput Output{..} = do
  mdTarget <- view scrapeMetadata
  scrapeIO . withFile _outputFilePath WriteMode $
               \h -> do
                 case mdTarget of
                   TargetNone -> return ()
                   TargetYamlHeader -> B.hPut h (encode _outputMetadata)
                                       >> hPutStrLn h "\n---\n\n"
                   TargetJSON -> BL.writeFile (_outputFilePath -<.> "json")
                                 $ A.encode _outputMetadata
                   TargetCSV  -> return ()
                 TLIO.hPutStr h (toLazyText _outputContent)

writePage :: FilePath -> Page -> Scrape [SectionPage]
writePage dirname page@Page{..} = do
  let pageMeta = M.singleton "page" . Object $ asMetadata page
      pageFile = dirname </> makeFileName _pageVolumeId (0 :: Int) 0
      sp       = SectionPage page Nothing pageFile
  writeOutput $ Output
                  pageFile
                  pageMeta
                  (build page)
  fmap (sp:) . forM _pageContent $ \s@Section{_sectionN, _sectionHead} -> do
      let n        = _sectionN
          filename = dirname
                     </> makeFileName _pageVolumeId (maybe 0 fst _sectionHead) n
          metadata = M.insert "paragraph" (toJSON n)
                     . M.insert "section" (Object $ asMetadata s)
                     $ pageMeta
      writeOutput . Output filename metadata $ build s
      return $ SectionPage page (Just s) filename
    where
      makeFileName :: Buildable s => VolumeID -> s -> Int -> FilePath
      makeFileName (Volume a b p) section n =
          T.unpack
               . TL.toStrict
               $ format "barth-{}-{}-{}-{}-{}.md"
                     ( left 2 '0' a
                     , left 2 '0' b
                     , left 2 '0' p
                     , left 2 '0' section
                     , left 3 '0' n
                     )
      makeFileName (Appendix a) section n =
          T.unpack
               . TL.toStrict
               $ format "barth-{}-{}-{}-{}-{}.md"
                     ( "XX" :: T.Text
                     , left 2 '0' a
                     , left 2 '0' (0 :: Int)
                     , left 2 '0' section
                     , left 3 '0' n
                     )

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

writeCsv :: FilePath -> [SectionPage] -> Scrape ()
writeCsv csvFilePath sPages =
    scrapeIO . BL.writeFile csvFilePath
             . encodeDefaultOrderedByName
             $ L.sort sPages
