{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Scrape.Types where


import           Control.Lens           hiding ((.=))
import qualified Data.HashMap.Strict    as M
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Lazy.Builder
import           Data.Yaml
import           Network.URI
import           Text.Numeral.Roman
import qualified Text.XML               as XML
import           Text.XML.Lens          hiding ((.=))


type VolumeTitle   = T.Text
type SectionTitle  = T.Text
type SectionHeader = (Int, SectionTitle)
type Content       = T.Text
type MetaMap       = Object
type InputSource   = Either String URI

class Metadata a where
    asMetadata :: a -> MetaMap

data VolumeID
    = Volume !Int !Int !Int
    | Appendix !Int
    deriving (Show, Eq)

instance Buildable VolumeID where
    build (Volume v s p) = mconcat [ "Volume "
                                   , toRoman v
                                   , ","
                                   , build s
                                   , " §"
                                   , build p
                                   ]
    build (Appendix a)   = "Appendix " <> toRoman a

instance Metadata VolumeID where
    asMetadata (Volume v s p) = [ ("volume" , toJSON v)
                                , ("section", toJSON s)
                                , ("page"   , toJSON p)
                                ]
    asMetadata (Appendix a)   = [ ("appendix", toJSON a)
                                ]

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

instance Metadata Section where
    asMetadata = foldMap ( M.fromList
                         . zip ["section_number", "section_title"]
                         . toListOf both
                         . (toJSON `bimap` toJSON)
                         )
                 . sectionHead

-- TODO: Add input page
data Page
    = Page
    { pageVolumeId    :: !VolumeID
    , pageVolumeTitle :: !VolumeTitle
    , pageAbstract    :: !XML.Element
    , pageContent     :: ![(Int, Section)]
    } deriving (Show, Eq)

instance Buildable Page where
    build = buildElement . pageAbstract

instance Metadata Page where
    asMetadata Page{..} =
        M.insert "title" (toJSON pageVolumeTitle) $ asMetadata pageVolumeId

data Output
    = Output
    { outputFilePath :: !FilePath
    , outputMetadata :: !MetaMap
    , outputContent  :: !Builder
    } deriving (Show, Eq)

buildElement :: Element -> Builder
buildElement = foldMap build . toListOf (entire . text)
