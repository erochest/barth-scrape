{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Arrow              ((&&&))
import           Control.Error
import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Csv                   hiding (Parser, header)
import qualified Data.Csv                   as Csv
import           Data.Data
import           Data.Function
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L
import           Data.Ord
import qualified Data.Text                  as T
import           Data.Traversable
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV
import           GHC.Generics
import           Options.Applicative        hiding ((<$>), (<*>))


type TopicId     = Int
type Token       = T.Text
type Weight      = Double
type InputRow    = (TopicId, Token, Weight)
type InputByWord = [[InputRow]]
type WeightV     = UV.Vector Weight
type TokenIndex  = M.HashMap Token Int

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

data Network
        = Network
        { nodes :: [Node]
        , links :: [Link]
        } deriving (Show, Data, Typeable, Generic)

instance ToJSON Network

inputTopicId :: Lens' InputRow TopicId
inputTopicId = _1

inputWord :: Lens' InputRow T.Text
inputWord = _2

inputWeight :: Lens' InputRow Weight
inputWeight = _3

makeNodes :: InputByWord -> ([Node], TokenIndex)
makeNodes = (id &&& indexTokens) . catMaybes . snd . mapAccumL toNode 0
    where
        toNode i rs@((_, n, _):_) =
            let topic = L.maximum $ map (view inputTopicId) rs
            in  (i + 1, Just $ Node n topic i)
        toNode i [] = (i, Nothing)

        indexTokens = M.fromList . map (name &&& nodeId)

makeLinks :: TokenIndex -> InputByWord -> [Link]
makeLinks tindex byWord = concatMap (uncurry (loop tvectors)) tvectors
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

euclid :: (UV.Unbox a, Floating a) => UV.Vector a -> UV.Vector a -> a
euclid xs ys = sqrt . UV.sum . UV.map (^(2::Int)) $ UV.zipWith (-) xs ys

splitTabs :: FromRecord a => L8.ByteString -> Either String (V.Vector a)
splitTabs = traverse (Csv.runParser . parseRecord . V.fromList . fmap L8.toStrict . L8.split '\t')
          . V.fromList
          . L8.lines

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
        scriptIO . BL.writeFile outputFile
                 . A.encode
                 $ Network nodes (makeLinks tokenIndex byWord)
    where
        getWord = view inputWord


data Options
        = Options
        { inputFile  :: !FilePath
        , outputFile :: !FilePath
        } deriving (Show)

opts' :: Parser Options
opts' =   Options
      <$> strOption (  short 'i' <> long "input" <> metavar "TSV_FILE"
                    <> help "The input file (the weights file from MALLET).")
      <*> strOption (  short 'o' <> long "output" <> metavar "JSON_FILE"
                    <> help "The JSON file to write the output to.")

opts :: ParserInfo Options
opts =   info (helper <*> opts')
              (  fullDesc
              <> progDesc "Read a MALLET term weighting file into a JSON network."
              <> header "barth-par -- parallel reading a MALLET term weighting\
                        \  file into a JSON network.")
