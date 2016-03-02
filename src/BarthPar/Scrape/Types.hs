{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


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


buildElement :: Element -> Builder
buildElement = foldMap build . toListOf (entire . text)


type VolumeTitle   = T.Text
type SectionTitle  = T.Text
type SectionHeader = (Int, SectionTitle)
type Content       = T.Text
type MetaMap       = Object
type InputSource   = Either String URI

class Metadata a where
    asMetadata :: a -> MetaMap

data VolumeID
    = Volume
      { _volumeNumber  :: !Int
      , _volumePart    :: !Int
      , _volumeSection :: !Int
      }
    | Appendix { _appendixNumber :: !Int }
    deriving (Show, Eq)
$(makePrisms ''VolumeID)
$(makeLenses ''VolumeID)

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
    { _sectionHead     :: !(Maybe SectionHeader)
    , _sectionContent  :: ![XML.Element]
    , _sectionExcursus :: !(Maybe XML.Element)
    } deriving (Show, Eq)
$(makeLenses ''Section)

instance Buildable Section where
    build Section{..} =
        mconcat [ foldMap buildElement _sectionContent
                , "\n\n---\n\n"
                , foldMap buildElement _sectionExcursus
                ]

instance Metadata Section where
    asMetadata = foldMap ( M.fromList
                         . zip ["number", "title"]
                         . toListOf both
                         . (toJSON `bimap` toJSON)
                         )
                 . _sectionHead

-- TODO: Add input page
data Page
    = Page
    { _pageVolumeId    :: !VolumeID
    , _pageVolumeTitle :: !VolumeTitle
    , _pageAbstract    :: !XML.Element
    , _pageContent     :: ![(Int, Section)]
    } deriving (Show, Eq)
$(makeLenses ''Page)

instance Buildable Page where
    build = buildElement . _pageAbstract

instance Metadata Page where
    asMetadata Page{..} =
        M.insert "title" (toJSON _pageVolumeTitle) $ asMetadata _pageVolumeId

data Output
    = Output
    { _outputFilePath :: !FilePath
    , _outputMetadata :: !MetaMap
    , _outputContent  :: !Builder
    } deriving (Show, Eq)
$(makeLenses ''Output)
