{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Scrape.Types where


import           Control.Lens
import           Data.Bifunctor
import qualified Data.HashMap.Strict    as M
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Lazy.Builder
import           Text.Numeral.Roman
import qualified Text.XML               as XML
import           Text.XML.Lens

import           BarthPar.Scrape.Utils


type VolumeTitle   = T.Text
type SectionTitle  = T.Text
type SectionHeader = (Int, SectionTitle)
type Content       = T.Text
type MetaMap       = M.HashMap T.Text T.Text

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
