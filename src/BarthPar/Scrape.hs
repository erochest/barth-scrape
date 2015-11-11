{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape where


import qualified C18.Balance          as C18
import           C18.Types
import           Control.Arrow        hiding (left)
import           Control.Error
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy as B
-- import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Data.Text.Format
import qualified Data.Text.IO         as TIO
import qualified Data.Text.Lazy       as TL
import           Data.Traversable
import           Network.URI
import           Network.Wreq
import           System.Directory
import           System.IO
import           Text.Groom
import           Text.HTML.TagSoup
import           Text.XML
import qualified Text.XML             as XML
import           Text.XML.Cursor
-- import           System.FilePath
-- import           Control.Concurrent.Async

import           BarthPar.XML

import           Debug.Trace


type VolumeTitle  = T.Text
type SectionTitle = T.Text
type Content      = T.Text

data Paragraph
    = PlainP   !XML.Element
    | Excursus !XML.Element
    deriving (Show, Eq)

data Section
    = Section
    { sectionHead    :: !T.Text
    , sectionContent :: ![Paragraph]
    } deriving (Show, Eq)

data Page
    = Page
    { pageVolumeTitle  :: !T.Text
    , pageSectionTitle :: !T.Text
    , pageHead         :: !T.Text
    , pageAbstract     :: !Paragraph
    , pageContent      :: ![Section]
    } deriving (Show, Eq)


-- TODO: The tag sets of these should be bundled into one, easy-to-use
-- function in `c18sgml`. Also need to get a real list from the HTML
-- spec.

unnest :: [Tag T.Text] -> XML.Document
unnest = fromMaybe (XML.parseText_ XML.def "<div/>")
         . tagsToDocument
         . concat
         . snd
         . mapAccumL (C18.denestTag trans) []
    where
      trans = DeNestTrans
              ["p", "li", "td", "tr", "table"]
              ["meta", "br", "hr", "area", "img"]

findFileName :: Format -> Int -> Script FilePath
findFileName template n = do
  exists <- scriptIO $ doesFileExist filename
  if exists
  then findFileName template $ succ n
  else return filename
  where
    filename = TL.unpack . format template . Only $ left 3 '0' n

dumpPage :: String -> XML.Document -> Script XML.Document
dumpPage source doc = do
  filename <- findFileName "dump/page-{}.html" 0
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
  filename <- findFileName "dump/show-{}.html" 0
  scriptIO $ withFile filename WriteMode $ \f -> do
                       TIO.hPutStrLn f "---"
                       hprint f "source: {}\n" $ Only source
                       TIO.hPutStrLn f "---\n"
                       TIO.hPutStrLn f . T.pack $ groom x
  return x

dumpText :: String -> T.Text -> Script T.Text
dumpText source x = do
  filename <- findFileName "dump/text-{}.html" 0
  scriptIO . TIO.writeFile filename $ mconcat
               [ "---\n"
               , T.pack source, "\n"
               , "===\n"
               , x
               ]
  return x

dumpEl :: String -> XML.Element -> Script XML.Element
dumpEl source el = do
    void . dumpPage source $ XML.Document (XML.Prologue [] Nothing []) el []
    return el

dl :: URI -> Script XML.Document
dl rootUrl = do
    scriptIO . putStrLn $ "DOWNLOAD: " ++ uri
    dumpPage uri . unnest . parseTags
        =<< dumpText uri . decodeUtf8 . B.toStrict . view responseBody
        =<< scriptIO (get uri)
    where
      uri = show rootUrl

watch :: Show a => String -> a -> a
watch msg x = trace (msg ++ ": " ++ show x) x

scrape :: FilePath -> String -> Script ()
scrape outputDir rootUrl =
    case parseURI rootUrl of
      Just uri -> scrapeTOC uri >>= mapM_ (writePage outputDir)
      Nothing  -> return ()

scrapeTOC :: URI -> Script [Page]
scrapeTOC uri =
    fmap concat . smapConcurrently (uncurry scrapeVolumePage)
        =<< scrapeTOCPage uri "Table of Contents" (T.isPrefixOf "CD ")

smapConcurrently :: Traversable t => (a -> Script b) -> t a -> Script (t b)
-- smapConcurrently f = scriptIO . mapConcurrently (runScript . f)
smapConcurrently = mapM

scrapeTOCPage :: URI -> T.Text -> (T.Text -> Bool) -> Script [(T.Text, URI)]
scrapeTOCPage uri title p = do
    doc <- dl uri
    mapMaybe (sequenceA . fmap (appendUri uri)) . filter (p . fst)
                 <$> dumpPrint ("scrapeTOCPage \"" ++ T.unpack title ++ "\"")
                         (fromDocument doc $// tocEntries >=> tocPair title)

appendUri :: URI -> T.Text -> Maybe URI
appendUri base = fmap (`nonStrictRelativeTo` base)
                 . parseRelativeReference
                 . T.unpack

scrapeVolumePage :: T.Text -> URI -> Script [Page]
scrapeVolumePage volName uri =
    smapConcurrently (uncurry (scrapePage volName))
        =<< scrapeTOCPage uri "View Text" (T.isPrefixOf "§ ")

scrapePage :: T.Text -> T.Text -> URI -> Script Page
scrapePage _volName _pageName uri = do
    doc <- dl uri
    _   <- writeNodes "output/page.html" . map node
           $ fromDocument doc
                 $// laxElement "div"
                 >=> attributeIs "type" "abstract"
                 >=> parent
    undefined

isContent :: T.Text -> Cursor -> Bool
isContent c el = (== c) . T.concat $ el $// content

tocEntries :: Cursor -> [Cursor]
tocEntries c = c $// laxElement "i"

-- | This takes the link text to find and a cursor positioned on the <i>
-- elements surrounding the titles, and it returns a list of titles and
-- links.
tocPair :: T.Text -> Cursor -> [(T.Text, T.Text)]
tocPair c = pure . (T.concat *** T.concat) . (childContent &&& tocLinks c)

childContent ::Cursor -> [T.Text]
childContent = child >=> content

tocLinks :: T.Text -> Cursor -> [T.Text]
tocLinks c = followingSibling
             >=> laxElement "a"
             >=> check (isContent c)
             >=> laxAttribute "href"

writePage :: FilePath -> Page -> Script ()
writePage = undefined

wrapNodes :: [XML.Node] -> XML.Document
wrapNodes nodes =
    XML.Document
       (XML.Prologue [] Nothing [])
       (XML.Element "div" mempty nodes)
       []

writeNodes :: FilePath -> [XML.Node] -> Script XML.Document
writeNodes filename nodes = do
    writeDoc filename doc
    return doc
    where
      doc = wrapNodes nodes

writeDoc :: FilePath -> XML.Document -> Script ()
writeDoc filename doc =
    scriptIO $ XML.writeFile (XML.def { XML.rsPretty = True }) filename doc

writePairs :: FilePath -> [(T.Text, T.Text)] -> Script ()
writePairs filename = scriptIO
                      . TIO.writeFile filename
                      . T.unlines
                      . map (\(a, b) -> T.concat [a, "\t", b])
