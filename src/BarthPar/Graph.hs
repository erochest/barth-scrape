{-# LANGUAGE TupleSections #-}


module BarthPar.Graph where


import           Control.Arrow       ((***))
import           Control.Lens
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Maybe
import           Data.Ord
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

import           BarthPar.Lens
import           BarthPar.Types
import           BarthPar.Utils


makeNodes :: InputByWord -> ([Node], TokenIndex)
makeNodes = (catMaybes *** snd) . swap . mapAccumL toNode (0, M.empty)
    where
        toNode (i, idx) rs@((_, n, _):_) =
            let topik  = view inputTopicId
                       $ L.maximumBy (comparing (view inputWeight)) rs
                index' = M.insert n i idx
            in  ((i + 1, index'), Just $ Node n topik i)
        toNode i [] = (i, Nothing)

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
