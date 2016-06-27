{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module BarthPar.Scrape.XMLSpec where


import           Control.Lens
import           Data.Maybe
import           Data.MonoTraversable hiding (Element)
import qualified Data.MonoTraversable as MT
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Text.XML
import           Text.XML.Lens

import           Test.Hspec

import           BarthPar.Scrape.XML  (allText, filterEl)


class FromDocument a where
    fromDoc :: Document -> Maybe a
    toDoc   :: a -> Maybe Document

newtype DocNode = DocNode { getNode :: Node    }
                deriving (Show, Eq)
newtype DocEl   = DocEl   { getEl :: Element }
                deriving (Show, Eq)

instance FromDocument DocNode where
    fromDoc = Just . DocNode . NodeElement . documentRoot
    toDoc (DocNode (NodeElement e)) =
        Just $ Document (Prologue [] Nothing []) e []
    toDoc (DocNode _) = Nothing

type instance MT.Element DocNode = Node

instance MonoFunctor DocNode where
    omap f = DocNode . f . getNode

instance FromDocument DocEl where
    fromDoc = Just . DocEl . documentRoot
    toDoc (DocEl e) = Just $ Document (Prologue [] Nothing []) e []

type instance MT.Element DocEl = Element

instance MonoFunctor DocEl where
    omap f = DocEl . f . getEl

shouldFilter :: (FromDocument a, Show a, Eq a)
             => TL.Text -> (a -> Maybe a) -> Maybe TL.Text -> Expectation
shouldFilter input f expected =
    (toDoc =<< f =<< fromDoc (parseText_ def input)) `shouldBe`
        (Just . parseText_ def =<< expected)

shouldFilterEl :: TL.Text -> (Element -> Bool) -> Maybe TL.Text -> Expectation
shouldFilterEl input p =
    shouldFilter input (fmap DocEl . filterEl p . getEl)

shouldBuild :: TL.Text -> T.Text -> Expectation
shouldBuild input expected =
    norm (allText . NodeElement . documentRoot $ parseText_ def input)
        `shouldBe` expected
    where
        norm :: T.Text -> T.Text
        norm = T.unwords . T.words

spec :: Spec
spec = do
    describe "filterEl" $ do
        it "should filter out the root node that matches the predicate." $
            ("<span></span>" `shouldFilterEl`
                (isNothing . preview (named "span"))
                ) Nothing

        it "should leave the root node that does not match the predicate." $
            ("<span></span>" `shouldFilterEl`
                (isNothing . preview (named "div"))
                ) (Just "<span></span>")

        it "should filter out children that match the predicate." $
            ("<div><span></span><a></a><span></span></div>" `shouldFilterEl`
                (isNothing . preview (named "span"))
                ) (Just "<div><a></a></div>")

        it "should leave children that do not match the predicate." $
            ("<div><span><a></a></span><a></a><span><a></a><a></a></span></div>"
                `shouldFilterEl`
                (isNothing . preview (named "br"))
                ) (Just "<div><span><a></a></span><a></a>\
                        \<span><a></a><a></a></span></div>")

        it "should filter out descendants that match the predicate." $
            ("<div><span><a></a></span><a></a><span><a></a><a></a></span></div>"
                `shouldFilterEl`
                (isNothing . preview (named "a"))
                ) (Just "<div><span></span><span></span></div>")

        it "should leave descendants that don't match the predicate." $
            ("<div><span><a></a></span><a></a><span><a></a><a></a></span></div>"
                `shouldFilterEl`
                (isNothing . preview (named "oops"))
                ) (Just "<div><span><a></a></span><a></a>\
                        \<span><a></a><a></a></span></div>")

    describe "allText" $ do
        it "should pull text from nodes." $
            "<a>link</a>" `shouldBuild` "link"
        it "should walk multiple nodes." $
            "<span><a>link 1</a><a> link 2</a></span>" `shouldBuild`
                "link 1 link 2"
        it "should walk depth first." $
            "<span>a<span>b</span>c<span><span>d</span>e\
            \<span>f</span></span></span>" `shouldBuild` "ab cd ef"
        it "should property pull text from heavily noted and quoted text." $
            let input = "<span class=\"excursus\">\
                        \ <p>\
                        \ Theology is \
                        \ <span class=\"foreign\">\
                        \ <span class=\"hiitalic\">\
                        \ de divinitate ratio sive sermo\
                        \ </span> \
                        \ </span>\
                        \ <a href=\"javascript:displayNote('2:0:-1:0', '/cgi-bin/asp/philo/dkbl/getnote.pl?c.830:2:0:-1:0.barth')\">\
                        \ [note]\
                        \ </a>\
                        \ <span\
                        \ class=\"hiddennote\"\
                        \ id=\"note_2:0:-1:0\"\
                        \ onClick=\"hideNote('2:0:-1:0')\"/>\
                        \ (Augustine,\
                        \ <chuck_temp_title>\
                        \ <span class=\"hiitalic\">\
                        \ De civ. Dei\
                        \ </span>\
                        \ </chuck_temp_title>\
                        \ , VIII, 1).\
                        \ <span class=\"foreign\">\
                        \ Θεολόγος\
                        \ </span>\
                        \ est\
                        \ <span class=\"foreign\">\
                        \ ὁ τὸν θεὸν ἐκ θεοῦ ὲνώπιον τοῦ θεοῦ εἰς δόξαν αὐτοῦ λέγων\
                        \ </span>\
                        \ <a href=\"javascript:displayNote('2:0:-1:2', '/cgi-bin/asp/philo/dkbl/getnote.pl?c.830:2:0:-1:2.barth')\">\
                        \ [note]\
                        \ </a>\
                        \ <span\
                        \ class=\"hiddennote\"\
                        \ id=\"note_2:0:-1:2\"\
                        \ onClick=\"hideNote('2:0:-1:2')\"/>\
                        \ (\
                        \ <xref type=\"pub\">\
                        \ <bibl>\
                        \ <author>\
                        \ Coccejus\
                        \ </author>\
                        \ ,\
                        \ <chuck_temp_title>\
                        \ <span class=\"hiitalic\">\
                        \ Summa theol.\
                        \ </span>\
                        \ </chuck_temp_title>\
                        \ ,\
                        \ <date>\
                        \ 1699\
                        \ </date>\
                        \ , 1, 1\
                        \ </bibl>\
                        \ </xref>\
                        \ ).\
                        \ </p>\
                        \ </span>"
                expected = "Theology is de divinitate ratio sive sermo [note] \
                           \(Augustine, De civ. Dei , VIII, 1). Θεολόγος est ὁ \
                           \τὸν θεὸν ἐκ θεοῦ ὲνώπιον τοῦ θεοῦ εἰς δόξαν αὐτοῦ \
                           \λέγων [note] ( Coccejus , Summa theol. , 1699 , \
                           \1, 1 )."
            in  input `shouldBuild` expected
