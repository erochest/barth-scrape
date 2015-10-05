{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape where


import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Control.Monad
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Network.URI
import           Network.Wreq
-- import           System.FilePath
-- import           Control.Concurrent.Async
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.List       as L
import           Text.HTML.DOM
import qualified Text.XML        as XML
import           Text.XML.Cursor

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


unnest :: XML.Name -> XML.Node -> (Maybe XML.Node, [XML.Node])
unnest name (XML.NodeElement el)
    | XML.elementName el == name = (Nothing , el' : named)
    | otherwise                  = (Just el', named)
    where
      (children, named) = (catMaybes `bimap` concat)
                          . L.unzip
                          . map (unnest name)
                          $ XML.elementNodes el
      el' = XML.NodeElement $ el { XML.elementNodes = children }
unnest _ n = (Just n, [])

unnestSplice :: XML.Name -> XML.Node -> XML.Node
unnestSplice name (XML.NodeElement parent) =
    XML.NodeElement . setChildren parent . foldr' (step name) []
           $ XML.elementNodes parent
    where
      step :: XML.Name -> XML.Node -> [XML.Node] -> [XML.Node]
      step nm n accum = let (n', tail) = unnest nm n
                        in  maybe id (:) n' $ tail ++ accum

      setChildren :: XML.Element -> [XML.Node] -> XML.Element
      setChildren el ns = el { XML.elementNodes = ns }

unnestSplice _ n = n

hasName :: XML.Node -> XML.Name -> Bool
hasName (XML.NodeElement el) n = n == XML.elementName el
hasName _                    _ = False

dl :: URI -> Script XML.Document
dl rootUrl = do
    scriptIO . putStrLn $ "DOWNLOAD: " ++ uri
    scriptIO $ parseLBS . view responseBody <$> get uri
    where
      uri = show rootUrl

watch :: Show a => String -> a -> a
watch msg x = trace (msg ++ ": " ++ show x) x

scrape :: FilePath -> String -> Script ()
scrape outputDir rootUrl = do
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
    -- TODO: remove `take 1`
    return . take 1 . mapMaybe (sequenceA . fmap (appendUri uri))
           $ filter (p . fst)
           ( fromDocument doc $// tocEntries >=> tocPair title)

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
tocEntries c = c $// laxElement "table" &// laxElement "td" &// laxElement "i"

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
