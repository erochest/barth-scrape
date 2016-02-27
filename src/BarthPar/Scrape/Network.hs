{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Network where


import qualified C18.Balance            as C18
import           C18.Types
import           Control.Error
import           Control.Lens
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Data.Traversable
import           Network.URI
import           Network.Wreq
import           Text.HTML.TagSoup
import qualified Text.XML               as XML

import           BarthPar.Scrape.Output
import           BarthPar.XML


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
