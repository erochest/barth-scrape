{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Network where


import qualified C18.Balance           as C18
import           C18.Types
import           Control.Error
import           Control.Lens
import qualified Data.ByteString.Lazy  as B
import qualified Data.Text             as T
import           Data.Text.Encoding
import qualified Data.Text.IO          as TIO
import           Data.Traversable
import           Network.URI
import           Network.Wreq
import           System.FilePath
import           System.IO
import           Text.HTML.TagSoup
import qualified Text.XML              as XML

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils
import           BarthPar.XML


dl :: Maybe T.Text -> InputSource -> Scrape XML.Document
dl title (Right rootUrl) = do
    scrapeIO . hPutStrLn stderr
             . ("DOWNLOAD: " ++)
             $ maybe ("<" ++ uri ++ ">") ( (++ ")")
                                         . (++ uri)
                                         . ("[" ++)
                                         . (++ "](")
                                         . T.unpack
                                         ) title
    unnest . parseTags . decodeUtf8 . B.toStrict . view responseBody
           <$> scrapeIO (get uri)
    where
        uri = show rootUrl

dl title (Left filePath) = do
    scrapeIO . hPutStrLn stderr
             . ("READ: " ++)
             $ maybe ("<" ++ filePath ++ ">") ( (++ ")")
                                              . (++ filePath)
                                              . ("[" ++)
                                              . (++ "](")
                                              . T.unpack
                                              ) title
    unnest . parseTags <$> scrapeIO (TIO.readFile filePath)

appendInput :: InputSource -> T.Text -> Maybe InputSource
appendInput (Right uri)     app = Right <$> appendUri uri app
appendInput (Left filePath) app =
    Just . Left . replaceFileName filePath . unEscapeString $ T.unpack app

appendUri :: URI -> T.Text -> Maybe URI
appendUri base = fmap (`nonStrictRelativeTo` base)
                 . parseRelativeReference
                 . T.unpack

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
