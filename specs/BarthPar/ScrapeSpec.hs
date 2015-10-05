{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module BarthPar.ScrapeSpec where


import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import           Text.HTML.DOM        (parseLBS)
import           Text.XML             hiding (parseLBS)

import           Test.Hspec

import           BarthPar.Scrape      (unnest, unnestSplice)


parseEl :: LBS.ByteString -> Element
parseEl = documentRoot . parseLBS

parseN :: LBS.ByteString -> Node
parseN = NodeElement . parseEl

renderEl :: [Element] -> LBS.ByteString
renderEl [el] = renderLBS def (Document (Prologue [] Nothing []) el [])
renderEl els  =
    renderLBS def (Document (Prologue [] Nothing []) el [])
    where
      el = Element "div" [] $ map NodeElement els

renderN :: Node -> LBS.ByteString
renderN (NodeElement el) = renderEl [el]
renderN _                = LBS.empty

spec :: Spec
spec = describe "unnestSplice" $ do
         let expected = "<div>\
                        \<p><i>hello</i> <b>good-bye</b></p>\
                        \<p><b>good-bye</b> <i>hello</i></p>\
                        \<p><em>oops</em> <em>wrong!</em></p>\
                        \</div>"
             expected' = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         <> expected
             base = NodeElement $ parseEl expected

         it "should leave unnested structures alone." $
            renderN (unnestSplice "p" base) `shouldBe` expected'

         it "should move nested elements to the first instances' level." $
            let el = parseN "<div>\
                            \<p><i>hello</i> <b>good-bye</b>\
                            \<p><b>good-bye</b> <i>hello</i></p>\
                            \<p><em>oops</em> <em>wrong!</em></p>\
                            \</p></div>"
            in  renderN (unnestSplice "p" el) `shouldBe` expected'

         it "should move deeply nested elements to the first instance's." $
            let el = parseN "<div>\
                            \<p><i>hello</i> <b>good-bye</b>\
                            \<p><b>good-bye</b> <i>hello</i>\
                            \<p><em>oops</em> <em>wrong!</em></p>\
                            \</p></p></div>"
            in  renderN (unnestSplice "p" el) `shouldBe` expected'
