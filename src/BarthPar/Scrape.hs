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
import           System.FilePath
import           Text.HTML.DOM
import qualified Text.XML        as XML
import           Text.XML.Cursor


data Hole = Hole


dl :: String -> Script XML.Document
dl rootUrl = scriptIO $ parseLBS . view responseBody <$> get rootUrl

scrape :: String -> FilePath -> Script ()
scrape rootUrl outputDir = do
    doc <- dl rootUrl
    let uri   = parseURI rootUrl
        table = fmap ( maybe "" (T.pack .show)
                     . ap (nonStrictRelativeTo <$> uri)
                     . parseRelativeReference
                     . T.unpack)
                <$> filter (T.isPrefixOf "CD " . fst)
                (   fromDocument doc $// tocEntries >=> tocPair)

    scriptIO . TIO.writeFile (outputDir </> "root.txt")
                 . T.unlines
                 $ map (\(a, b) -> T.concat [a, "\t", b]) table

isTOC :: Cursor -> Bool
isTOC a = (== "Table of Contents") . T.concat $ a $// content

tocEntries :: Cursor -> [Cursor]
tocEntries c = c $// laxElement "table" &// laxElement "td" &// laxElement "i"

tocPair :: Cursor -> [(T.Text, T.Text)]
tocPair = pure . (T.concat *** T.concat) . (childContent &&& tocLinks)

childContent ::Cursor -> [T.Text]
childContent = child >=> content

tocLinks :: Cursor -> [T.Text]
tocLinks =     followingSibling
           >=> laxElement "a"
           >=> check isTOC
           >=> laxAttribute "href"

wrapNodes :: [XML.Node] -> XML.Document
wrapNodes nodes =
    XML.Document
       (XML.Prologue [] Nothing [])
       (XML.Element "div" mempty nodes)
       []

writeNodes :: FilePath -> [XML.Node] -> Script XML.Document
writeNodes filename nodes = do
    scriptIO $ XML.writeFile (XML.def { XML.rsPretty = True }) filename doc
    return doc
    where
      doc = wrapNodes nodes
