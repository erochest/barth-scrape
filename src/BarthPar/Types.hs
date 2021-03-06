{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}


module BarthPar.Types where


import           Control.Parallel.Strategies
import           Data.Aeson
import qualified Data.ByteString.Char8       as B8
import           Data.Csv
import           Data.Data
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import qualified Data.Text                   as T
import qualified Data.Vector.Unboxed         as UV
import           GHC.Generics

import           BarthPar.Scrape.Types


data Actions
    = CsvToJson
    { jsonInputFile  :: !FilePath
    , jsonOutputFile :: !FilePath
    , jsonChunkSize  :: !Int
    }
    | Scrape
    { scrapeDebug     :: !Bool
    , scrapeClean     :: !Bool
    , scrapeInput     :: !(Either FilePath String)
    , scrapeMetadata  :: !MetadataTarget
    , scrapeChunks    :: !Chunking
    , scrapeOutputDir :: !FilePath
    }
    | ScrapePage
    { pageFile      :: !FilePath
    , pageVolume    :: !T.Text
    , pageTitle     :: !T.Text
    , pageMetadata  :: !MetadataTarget
    , pageChunks    :: !Chunking
    , pageOutputDir :: !FilePath
    }
    deriving (Show)

type TopicId     = Int
type Weight      = Double
type InputRow    = (TopicId, Token, Weight)
type InputByWord = [[InputRow]]
type WeightV     = UV.Vector Weight
type TokenIndex  = M.HashMap Token Int

newtype Token = Token { unToken :: B8.ByteString }
                deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance Hashable Token

instance ToJSON Token where
    toJSON = toJSON . B8.unpack . unToken

instance FromField Token where
    parseField = return . Token

data Node
        = Node
        { name   :: !Token
        , topic  :: !TopicId
        , nodeId :: !Int
        } deriving (Show, Eq, Data, Typeable, Generic)

instance ToJSON Node

data Link
        = Link
        { source :: !Int
        , target :: !Int
        , weight :: !Weight
        } deriving (Show, Data, Typeable, Generic)

instance ToJSON Link

instance NFData Link

data Network
        = Network
        { nodes :: [Node]
        , links :: [Link]
        } deriving (Show, Data, Typeable, Generic)

instance ToJSON Network
