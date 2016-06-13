{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module BarthPar.Scrape.XMLSpec where


import           Control.Lens
import           Data.Maybe
import           Data.MonoTraversable hiding (Element)
import qualified Data.MonoTraversable as MT
import qualified Data.Text.Lazy       as TL
import           Text.XML
import           Text.XML.Lens

import           Test.Hspec

import           BarthPar.Scrape.XML  (filterEl)


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

spec :: Spec
spec =
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
