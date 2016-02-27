{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module BarthPar.Scrape where


import qualified C18.Balance            as C18
import           C18.Types
import           Control.Arrow          hiding (left)
import           Control.Error
import           Control.Lens           hiding ((??))
import           Control.Monad
import qualified Data.ByteString.Lazy   as B
import           Data.Char
-- import qualified Data.Set             as S
import qualified Data.List              as L
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Encoding
import           Data.Text.Format       hiding (build)
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO      as TLIO
import           Data.Text.Read
import           Data.Traversable
import           Data.Tuple
import           Network.URI
import           Network.Wreq
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Groom
import           Text.HTML.TagSoup
import           Text.XML
import qualified Text.XML               as XML
import           Text.XML.Cursor
import           Text.XML.Lens          hiding (attributeIs, (??))
-- import           Control.Concurrent.Async
import           BarthPar.XML
import           Data.Bitraversable
import qualified Data.HashMap.Strict    as M
import           Text.Numeral.Roman

import           Debug.Trace


type VolumeTitle   = T.Text
type SectionTitle  = T.Text
type SectionHeader = (Int, SectionTitle)
type Content       = T.Text
type MetaMap       = M.HashMap T.Text T.Text

tshow :: Show a => a -> T.Text
tshow = T.pack . show

class Metadata a where
    asMetadata :: a -> M.HashMap T.Text T.Text

data VolumeID
    = Volume !Int !Int
    | Appendix !Int
    deriving (Show, Eq)

instance Buildable VolumeID where
    build (Volume v s) = mconcat ["Volume ", toRoman v, ",", build s]
    build (Appendix a) = "Appendix " <> toRoman a

instance Metadata VolumeID where
    asMetadata (Volume v s) = M.fromList [ ("volume",  tshow v)
                                         , ("section", tshow s)
                                         ]
    asMetadata (Appendix a) = M.singleton "appendix" $ tshow a

data Section
    = Section
    { sectionHead     :: !(Maybe SectionHeader)
    , sectionContent  :: ![XML.Element]
    , sectionExcursus :: !(Maybe XML.Element)
    } deriving (Show, Eq)

instance Buildable Section where
    build Section{..} =
        mconcat [ foldMap buildElement sectionContent
                , "\n\n---\n\n"
                , foldMap buildElement sectionExcursus
                ]
        where
          buildElement = foldMap build . toListOf text

instance Metadata Section where
    asMetadata = foldMap ( M.fromList
                         . zip ["number", "title"]
                         . toListOf both
                         . first tshow
                         )
                 . sectionHead

data Page
    = Page
    { pageVolumeId    :: !VolumeID
    , pageVolumeTitle :: !VolumeTitle
    , pageAbstract    :: !XML.Element
    , pageContent     :: ![Section]
    } deriving (Show, Eq)

instance Buildable Page where
    build = foldMap build . toListOf text . pageAbstract

instance Metadata Page where
    asMetadata Page{..} =
        M.insert "title" pageVolumeTitle $ asMetadata pageVolumeId

data Output
    = Output
    { outputMetadata :: !MetaMap
    , outputContent  :: !Builder
    } deriving (Show, Eq)


-- TODO: write the metadata to a json file alongside the content file,
-- or a yaml block

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
              ["p", "li", "td", "tr", "table", "div"]
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

dl :: Maybe T.Text -> URI -> Script XML.Document
dl title rootUrl = do
    scriptIO
       . putStrLn
       . ("DOWNLOAD: " ++)
       $ maybe ("<" ++ uri ++ ">") ( (++ ")")
                                   . (++ uri)
                                   . ("[" ++)
                                   . (++ "](")
                                   . T.unpack
                                   ) title
    dumpPage uri . unnest . parseTags
        =<< dumpText uri . decodeUtf8 . B.toStrict . view responseBody
        =<< scriptIO (get uri)
    where
      uri = show rootUrl

cleanOutputs :: FilePath -> Script ()
cleanOutputs outputDir = do
  rmFiles outputDir
  rmFiles "dump"
  where
    rmFiles :: FilePath -> Script ()
    rmFiles dirname = mapM_ (scriptIO . removeFile . (dirname </>))
                      . filter (not . ("." `L.isPrefixOf`))
                    =<< scriptIO (getDirectoryContents dirname)

watch :: Show a => String -> a -> a
watch msg x = trace (msg ++ ": " ++ groom x) x

watchM :: (Monad m, Show a) => String -> a -> m a
watchM msg x = traceM (msg ++ ": " ++ groom x) >> return x

forceS :: String -> [a] -> Script a
forceS _ (x:_) = return x
forceS e []    = throwE e
forceS _ _     = throwE "Unable to force."

scrape :: Bool -> FilePath -> String -> Script ()
scrape clean outputDir rootUrl = do
  when clean $
       cleanOutputs outputDir
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

scrapeTOCPage :: URI
              -- ^ The ToC's URI to download
              -> T.Text
              -- ^ The content of A tags to look for.
              -> (T.Text -> Bool)
              -- ^ A predicate to find which links to move to next.
              -> Script [(T.Text, URI)]
                 -- ^ A list of A tag contents and @hrefs.
scrapeTOCPage uri title p = do
    doc <- dl (Just title) uri
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

scrapePage :: VolumeTitle -> T.Text -> URI -> Script Page
scrapePage volName pageName uri = do
    doc <- dl (Just $ volName <> " | " <> pageName) uri
    writeDoc "output/doc.html" doc
    let nds = fromDocument doc
              $// laxElement "div"
              >=> attributeIs "id" "tinyurl"
              >=> followingSibling
              >=> laxElement "div"
    void . writeNodes "output/page.html" $ map node nds
    makePage volName nds
    undefined

makePage :: VolumeTitle -> [Cursor] -> Script Page
makePage vtitle cs =
    Page <$> parseVolumeID vtitle
             <*> forceS "Unable to find VolumeTitle"
                     (concatMap ($// laxElement "span"
                                >=> attributeIs "class" "head"
                                >=> child
                                >=> content
                                ) cs)
             <*> forceS "Unable to find abstract"
                 (concatMap (toListOf _Element . node) abstract)
             <*> mapM (fmap cleanSection . readSection)
                     (concatMap
                      ($| followingSibling &/ laxElement "div") abstract)
    where
      abstract = concatMap
                 ($| laxElement "div" >=> attributeIs "type" "abstract")
                 cs

parseVolumeID :: VolumeTitle -> Script VolumeID
parseVolumeID vtitle
    -- CD Volume I,1 (§§ 1-12)
    | "CD Volume " `T.isPrefixOf` vtitle =
        fmap (uncurry Volume)
        . bisequenceA
        . (fromRomanS `bimap` (fmap fst . decimalS . T.drop 1))
        . T.break (==',')
        . T.takeWhile (not . isSpace)
        $ T.drop 10 vtitle

    -- I. General Index
    | otherwise =
        fmap Appendix . fromRomanS $ T.takeWhile (not . (=='.')) vtitle

readSection :: Cursor -> Script Section
readSection c =
    Section <$> (listToMaybe
                <$> mapM readSectionHead
                        (c $/ laxElement "span" >=> attributeIs "class" "head"))
            <*> pure (c $/ laxElement "p" >=> toListOf _Element . node)
            <*> (   pure
                .   listToMaybe
                $   c
                $/  laxElement "span"
                >=> attributeIs "class" "excursus"
                >=> toListOf _Element . node
                )

decimalS :: Integral a => T.Text -> Script (a, T.Text)
decimalS = hoistEither . decimal

fromRomanS :: T.Text -> Script Int
fromRomanS s = fromRoman s ?? ("Unable to parse " ++ show s)

readSectionHead :: Cursor -> Script SectionHeader
readSectionHead c = fmap (swap . fmap fst)
                    . sequenceA
                    . (T.drop 2 *** decimalS)
                    . swap
                    . T.break (=='.')
                    . T.concat
                    $ c $// content

-- TODO:
cleanSection :: Section -> Section
cleanSection = id

isContent :: T.Text -> Cursor -> Bool
isContent c e = (== c) . T.concat $ e $// content

tocEntries :: Axis
tocEntries = laxElement "i"

-- | This takes the link text to find and a cursor positioned on the <i>
-- elements surrounding the titles, and it returns a list of titles and
-- links.
tocPair :: T.Text -> Cursor -> [(T.Text, T.Text)]
tocPair link =
    pure . (T.concat *** T.concat) . (childContent &&& (take 1 . tocLinks link))

childContent ::Cursor -> [T.Text]
childContent = child >=> content

tocLinks :: T.Text -> Cursor -> [T.Text]
tocLinks c = followingSibling
             >=> laxElement "a"
             >=> check (isContent c)
             >=> laxAttribute "href"

writePage :: FilePath -> Page -> Script ()
writePage dirname page@Page{..} = do
  -- TODO: abstract
  forM_ (zip [1..] pageContent) $ \(n, s@Section{..}) ->
      let filename = dirname
                     </> makeFileName pageVolumeId
                             (maybe 0 fst sectionHead) n
      in  scriptIO
              . TLIO.writeFile filename
              . toLazyText
              $ mconcat [ "# "
                        , build pageVolumeId
                        , " -- "
                        , build pageVolumeTitle
                        , "\n"
                        , build s
                        ]
    where
      makeFileName :: VolumeID -> Int -> Int -> FilePath
      makeFileName (Volume a b) section n =
          T.unpack
               . TL.toStrict
               $ format "barth-{}-{}-{}-{}" ( left 2 '0' a
                                            , left 2 '0' b
                                            , left 2 '0' section
                                            , left 2 '0' n
                                            )
      makeFileName (Appendix a) section n =
          T.unpack
               . TL.toStrict
               $ format "barth-{}-{}-{}-{}" ( "XX" :: T.Text
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
