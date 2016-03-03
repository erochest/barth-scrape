{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


module BarthPar.Scrape.Types where


import           Control.Error
import           Control.Lens           hiding ((.=))
import           Control.Monad.Reader
import           Data.Data
import qualified Data.HashMap.Strict    as M
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Lazy.Builder
import           Data.Yaml
import           GHC.Generics
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
type PureScript    = Either String

data MetadataTarget
    = TargetNone
    | TargetYamlHeader
    | TargetJSON
    deriving (Show, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)
$(makePrisms ''MetadataTarget)

data ScrapeState
    = ScrapeState
    { _scrapeDebugging :: !Bool
    , _scrapeMetadata  :: !MetadataTarget
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''ScrapeState)

newtype Scrape a = Scrape { unScrape :: ReaderT ScrapeState Script a }
    deriving (Functor, Applicative, Monad, MonadReader ScrapeState)

toScript :: Bool -> MetadataTarget-> Scrape a -> Script a
toScript debugging target s =
    runReaderT (unScrape s) $ ScrapeState debugging target

runScrape :: Bool -> MetadataTarget -> Scrape a -> IO a
runScrape debugging target s = runScript $ toScript debugging target s

io :: PureScript a -> Scrape a
io ps = Scrape . ReaderT $ const $ hoistEither ps

scrape :: Script a -> Scrape a
scrape s = Scrape . ReaderT $ const s

scrapeIO :: IO a -> Scrape a
scrapeIO s = Scrape . ReaderT $ const $ scriptIO s

throwS :: String -> Scrape a
throwS = scrape . throwE

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
