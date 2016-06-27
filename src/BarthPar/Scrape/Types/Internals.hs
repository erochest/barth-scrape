{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module BarthPar.Scrape.Types.Internals where


import           Control.DeepSeq
import           Control.Lens
import qualified Data.Aeson             as A
import           Data.Csv               hiding (HasHeader (..), Header, header)
import qualified Data.Csv               as Csv
import           Data.Data
import           Data.Ord
import qualified Data.Text              as T
import           Data.Text.Buildable
import qualified Data.Text.Format       as F
import           Data.Text.Lazy.Builder
import           Data.Yaml
import           GHC.Generics           hiding (to)
import           Network.URI


type Title       = T.Text
type Content     = T.Text
type MetaMap     = Object
type PureScript  = Either String
type InputSource = PureScript URI

data Header
    = Header
    { _headerN     :: !Int
    , _headerTitle :: !Title
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Header)

instance NFData Header

instance Ord Header where
    compare = comparing _headerN

instance ToJSON Header where
    toJSON (Header n t) = A.object [ "n"     A..= n
                                   , "title" A..= t
                                   ]

instance Buildable Header where
    build (Header n t) = F.build "{}. {}" (n, t)

data MetadataTarget
    = TargetNone
    | TargetYamlHeader
    | TargetJSON
    | TargetCSV
    deriving (Show, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)
$(makePrisms ''MetadataTarget)

instance NFData MetadataTarget

data Chunking
    = VolumeChunks      -- ^ CD I
    | PartChunks        -- ^ CD I.1
    | ChapterChunks     -- ^ CD I.1.§1
    | ParagraphChunks   -- ^ CD I.1.§1.1
    | BlockChunks       -- ^ CD I.1.§1.1.1
    | SizedChunks       { _chunkSize :: !Int }
    deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''Chunking)
$(makeClassy ''Chunking)

instance NFData Chunking

data ChapterLoc
    = ChapterLoc
    { _locVolume  :: !Int
    , _locPart    :: !Int
    , _locChapter :: !Int
    } deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeClassy ''ChapterLoc)

instance NFData ChapterLoc

data CsvOutput
    = CsvOutput
    { _csvHeader :: !Csv.Header
    , _csvRecord :: !NamedRecord
    } deriving (Show, Eq)
$(makeClassy ''CsvOutput)

data Output
    = Output
    { _outputFilePath :: !FilePath
    , _outputMetadata :: !MetaMap
    , _outputContent  :: !Builder
    , _outputCsv      :: !(Maybe CsvOutput)
    } deriving (Show, Eq)
$(makeClassy ''Output)
