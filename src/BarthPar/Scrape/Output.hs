{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Scrape.Output where


import           Control.Error
import           Control.Monad
import qualified Data.ByteString        as B
import qualified Data.HashMap.Strict    as M
import qualified Data.List              as L
import           Data.Monoid
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


findFileName :: Format -> Int -> Script FilePath
findFileName template n = do
  exists <- scriptIO $ doesFileExist filename
  if exists
  then findFileName template $ succ n
  else return filename
  where
    filename = TL.unpack . format template . Only $ left 3 '0' n

cleanOutputs :: FilePath -> Script ()
cleanOutputs outputDir = do
  rmFiles outputDir
  rmFiles "dump"
  where
    rmFiles :: FilePath -> Script ()
    rmFiles dirname = mapM_ (scriptIO . removeFile . (dirname </>))
                      . filter (not . ("." `L.isPrefixOf`))
                    =<< scriptIO (getDirectoryContents dirname)

dumpPage :: String -> XML.Document -> Script XML.Document
dumpPage source doc = do
  filename <- findFileName "dump/{}-page.html" 0
  scriptIO $ withFile filename WriteMode $ \f -> do
                     TIO.hPutStrLn f "<!--"
                     hprint f "source: {}\n" $ Only source
                     TIO.hPutStrLn f "-->\n"
                     TIO.hPutStr f . TL.toStrict
                            $ XML.renderText (XML.def
                                             { rsPretty = True
                                             }) doc
  return doc

dumpPrint :: Show a => String -> a -> Script a
dumpPrint source x = do
  filename <- findFileName "dump/{}-show.html" 0
  scriptIO $ withFile filename WriteMode $ \f -> do
                       TIO.hPutStrLn f "---"
                       hprint f "source: {}\n" $ Only source
                       TIO.hPutStrLn f "---\n"
                       TIO.hPutStrLn f . T.pack $ groom x
  return x

dumpText :: String -> T.Text -> Script T.Text
dumpText source x = do
  filename <- findFileName "dump/{}-text.html" 0
  scriptIO . TIO.writeFile filename $ mconcat
               [ "---\n"
               , T.pack source, "\n"
               , "===\n"
               , x
               ]
  return x

dumpEl :: String -> XML.Element -> Script XML.Element
dumpEl source e = do
    void . dumpPage source $ XML.Document (XML.Prologue [] Nothing []) e []
    return e

writeOutput :: Output -> Script ()
writeOutput Output{..} = traceM ("WRITE: " ++ outputFilePath)
                         >> scriptIO
                         . withFile outputFilePath WriteMode $ \h ->
                         B.hPut h (encode outputMetadata)
                         >> hPutStrLn h "\n---\n\n"
                         >> TLIO.hPutStr h (toLazyText outputContent)

writePage :: FilePath -> Page -> Script ()
writePage dirname page@Page{..} = do
  let pageMeta = asMetadata page
  writeOutput $ Output
                  (dirname </> makeFileName pageVolumeId 'a' 0)
                  pageMeta
                  (build page)
  forM_ (zip [0..] pageContent) $ \(n, s@Section{sectionHead}) ->
      let filename = dirname
                     </> makeFileName pageVolumeId (maybe 0 fst sectionHead) n
          metadata = M.singleton "page" (Object pageMeta)
                     <> asMetadata s
                     <> M.singleton "paragraph" (toJSON n)
      in  writeOutput . Output filename metadata $ build s
    where
      makeFileName :: Buildable s => VolumeID -> s -> Int -> FilePath
      makeFileName (Volume a b) section n =
          T.unpack
               . TL.toStrict
               $ format "barth-{}-{}-{}-{}.md" ( left 2 '0' a
                                               , left 2 '0' b
                                               , left 2 '0' section
                                               , left 2 '0' n
                                               )
      makeFileName (Appendix a) section n =
          T.unpack
               . TL.toStrict
               $ format "barth-{}-{}-{}-{}.md" ( "XX" :: T.Text
                                               , left 2 '0' a
                                               , left 2 '0' section
                                               , left 2 '0' n
                                               )

wrapNodes :: [XML.Node] -> XML.Document
wrapNodes nds =
    XML.Document
       (XML.Prologue [] Nothing [])
       (XML.Element "div" mempty nds)
       []

writeNodes :: FilePath -> [XML.Node] -> Script XML.Document
writeNodes filename nds = do
    writeDoc filename doc
    return doc
    where
      doc = wrapNodes nds

writeDoc :: FilePath -> XML.Document -> Script ()
writeDoc filename doc =
    scriptIO $ XML.writeFile (XML.def { XML.rsPretty = True }) filename doc

writePairs :: FilePath -> [(T.Text, T.Text)] -> Script ()
writePairs filename = scriptIO
                      . TIO.writeFile filename
                      . T.unlines
                      . map (\(a, b) -> T.concat [a, "\t", b])
