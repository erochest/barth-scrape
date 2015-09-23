{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Error
import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import           Data.Function
import qualified Data.List            as L
import           Data.Ord
import qualified Data.Vector          as V

import           BarthPar.Data
import           BarthPar.Graph
import           BarthPar.Lens
import           BarthPar.Types
import           Opts


main :: IO ()
main = do
    runScript $ do
        Options{..} <- parseOpts
        byWord <-  ExceptT
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
