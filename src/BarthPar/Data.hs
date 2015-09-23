{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module BarthPar.Data where


import           Conduit
import           Control.Error
import           Control.Lens
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Csv
import qualified Data.Csv                   as Csv
import qualified Data.Vector                as V

import           BarthPar.Lens
import           BarthPar.Parallel
import           BarthPar.Types


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

getWord :: InputRow -> Token
getWord = view inputWord
