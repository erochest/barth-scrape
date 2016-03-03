{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Scrape.Output where


import           Control.Monad
-- import qualified Data.ByteString        as B
import qualified Data.HashMap.Strict    as M
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Format       hiding (build)
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO      as TLIO
import           Data.Yaml.Aeson
import           Debug.Trace
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
    ensureClean :: FilePath -> Scrape()
    ensureClean dirname =
        scrapeIO (createDirectoryIfMissing True dirname) >>
                 scrapeIO (removeDirectoryRecursive dirname)

dumpPage :: String -> XML.Document -> Scrape XML.Document
dumpPage source doc = debugging' doc $ do
  filename <- findFileName "dump/{}-page.html" 0
  scrapeIO $ withFile filename WriteMode $ \f -> do
                     TIO.hPutStrLn f "<!--"
                     hprint f "source: {}\n" $ Only source
                     TIO.hPutStrLn f "-->\n"
                     TIO.hPutStr f . TL.toStrict
                            $ XML.renderText (XML.def
                                             { rsPretty = True
                                             }) doc
  return doc

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
writeOutput Output{..} = traceM ("WRITE: " ++ _outputFilePath)
                         >> scrapeIO
                         . withFile _outputFilePath WriteMode $ \h ->
                         -- B.hPut h (encode _outputMetadata)
                         -- >> hPutStrLn h "\n---\n\n"
                         -- >>
                         TLIO.hPutStr h (toLazyText _outputContent)

writePage :: FilePath -> Page -> Scrape ()
writePage dirname page@Page{..} = do
  let pageMeta = M.singleton "page" . Object $ asMetadata page
  writeOutput $ Output
                  (dirname </> makeFileName _pageVolumeId (0 :: Int) 0)
                  pageMeta
                  (build page)
  forM_ _pageContent $ \(n, s@Section{_sectionHead}) ->
      let filename = dirname
                     </> makeFileName _pageVolumeId (maybe 0 fst _sectionHead) n
          metadata = M.insert "paragraph" (toJSON n)
                     . M.insert "section" (Object $ asMetadata s)
                     $ pageMeta
      in  writeOutput . Output filename metadata $ build s
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
