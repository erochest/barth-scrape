{-# LANGUAGE OverloadedStrings #-}


module BarthPar.XML where


import           Data.Bifunctor
import qualified Data.Map.Lazy          as M
import           Data.Maybe
import qualified Data.Text              as T
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.XML


tagsToDocument :: [Tag T.Text] -> Maybe Document
tagsToDocument = treeToDocument . tagTree

treeToDocument :: [TagTree T.Text] -> Maybe Document
treeToDocument [] = Nothing
treeToDocument [tag] =
    flip (Document (Prologue [] Nothing [])) []
             <$> treeToElement tag
treeToDocument tags =
    Just $ Document
             (Prologue [] Nothing [])
             (Element "div" M.empty $ map treeToNode tags)
             []

treeToElement :: TagTree T.Text -> Maybe Element
treeToElement = extractElement . treeToNode

treeToNode :: TagTree T.Text -> Node
treeToNode (TagBranch name attrs children) =
    NodeElement . Element
                    (toName name)
                    (M.fromList $ map (first toName) attrs)
                    $ map treeToNode children
treeToNode (TagLeaf tag) = fromMaybe (NodeContent "") $ tagToNode tag

toName :: T.Text -> Name
toName name = Name name Nothing Nothing

tagToNode :: Tag T.Text -> Maybe Node
tagToNode (TagOpen _ _)        = Nothing
tagToNode (TagClose _)         = Nothing
tagToNode (TagText text)       = Just $ NodeContent text
tagToNode (TagComment comment) = Just $ NodeComment comment
tagToNode (TagWarning _)       = Nothing
tagToNode (TagPosition _ _)    = Nothing

extractElement :: Node -> Maybe Element
extractElement (NodeElement e) = Just e
extractElement _               = Nothing
