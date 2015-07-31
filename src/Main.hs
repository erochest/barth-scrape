{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Conduit
import           Control.Arrow               ((***))
import           Control.Error
import           Control.Lens
import           Control.Parallel.Strategies
import           Data.Aeson
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Char8       as B8
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as L8
import           Data.Csv                    hiding (Parser, header)
import qualified Data.Csv                    as Csv
import           Data.Data
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import qualified Data.List                   as L
import           Data.Ord
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import           GHC.Generics
import           Options.Applicative         hiding ((<$>), (<*>))


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

inputTopicId :: Lens' InputRow TopicId
inputTopicId = _1

inputWord :: Lens' InputRow Token
inputWord = _2

inputWeight :: Lens' InputRow Weight
inputWeight = _3

makeNodes :: InputByWord -> ([Node], TokenIndex)
makeNodes = (catMaybes *** snd) . swap . mapAccumL toNode (0, M.empty)
    where
        toNode (i, idx) rs@((_, n, _):_) =
            let topic  = L.maximum $ map (view inputTopicId) rs
                index' = M.insert n i idx
            in  ((i + 1, index'), Just $ Node n topic i)
        toNode i [] = (i, Nothing)

makeLinks :: TokenIndex -> InputByWord -> [Link]
makeLinks tindex byWord = concatMap (uncurry (loop tvectors)) tvectors
    -- ^ par
    where
        toVector rs@((_, t, _):_) =   (,)
                                  <$> M.lookup t tindex
                                  <*> Just ( UV.fromList
                                           . map (view inputWeight)
                                           $ L.sortBy (comparing (view inputTopicId)) rs)
        toVector []               = Nothing

        tvectors = V.fromList $ mapMaybe toVector byWord

        loop matrix i ws = catMaybes . V.toList $ V.map (uncurry (link i ws)) matrix

        link i iws j jws | i == j    = Nothing
                         | otherwise = Just . Link i j $ euclid iws jws

euclid :: UV.Vector Double -> UV.Vector Double -> Double
euclid xs ys = sqrt . UV.sum . UV.map (**2) $ UV.zipWith (-) xs ys

splitTabs :: FromRecord a => L8.ByteString -> Either String (V.Vector a)
splitTabs = traverse (Csv.runParser . parseRecord . V.fromList . fmap L8.toStrict . L8.split '\t')
          . V.fromList
          . L8.lines

lazyWriteNetwork :: Int -> FilePath -> Network -> Script ()
lazyWriteNetwork chunkSize output n = runResourceT $ doc n $$ sinkFile output
    where
        doc Network{..} = do
            yield "{\"nodes\":"
            injectJsonArray $ smartMap chunkSize A.encode nodes
            yield ",\"links\":"
            injectJsonArray $ smartMap chunkSize A.encode links
            yield "}"

injectJsonArray :: Monad m => [BL.ByteString] -> Source m BL.ByteString
injectJsonArray []     = yield "[]"
injectJsonArray (x:xs) = do
    yield "["
    yield x
    mapM_ item xs
    yield "]"
    where
        item y = yield "," >> yield y
        {-# INLINE item #-}

smartMap :: NFData y => Int -> (x -> y) -> [x] -> [y]
smartMap 0  f xs = map f xs
smartMap 1  f xs = parMap rdeepseq f xs
smartMap cs f xs = (map f xs) `using` (parListChunk cs rdeepseq)

smartList :: NFData y => Int -> [y] -> [y]
smartList 0  ys = ys
smartList 1  ys = ys `using` parList rdeepseq
smartList cs ys = ys `using` parListChunk cs rdeepseq

main :: IO ()
main = do
    Options{..} <- execParser opts
    runScript $ do
        byWord <-  EitherT
               $   fmap ( L.groupBy ((==) `on` getWord)
                        . L.sortBy (comparing getWord)
                        . V.toList
                        )
               .   splitTabs
               <$> BL.readFile inputFile
        let (nodes, tokenIndex) = makeNodes byWord
        lazyWriteNetwork chunkSize outputFile
            . Network nodes
            $ makeLinks tokenIndex byWord
    where
        getWord = view inputWord


data Options
        = Options
        { inputFile  :: !FilePath
        , outputFile :: !FilePath
        , chunkSize  :: !Int
        } deriving (Show)

opts' :: Parser Options
opts' =   Options
      <$> strOption   (  short 'i' <> long "input" <> metavar "TSV_FILE"
                      <> help "The input file (the weights file from MALLET).")
      <*> strOption   (  short 'o' <> long "output" <> metavar "JSON_FILE"
                      <> help "The JSON file to write the output to.")
      <*> option auto (  short 'c' <> long "chunk-size" <> metavar "CHUNK_SIZE" <> value 0
                      <> help "The size of chunks to divide sequences into for parallel\
                         \ processing. (Default is 0, which means no parallelization.)")

opts :: ParserInfo Options
opts =   info (helper <*> opts')
              (  fullDesc
              <> progDesc "Read a MALLET term weighting file into a JSON network."
              <> header "barth-par -- parallel reading a MALLET term weighting\
                        \  file into a JSON network.")
