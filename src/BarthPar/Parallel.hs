module BarthPar.Parallel where


import           Control.Parallel.Strategies


smartMap :: NFData y => Int -> (x -> y) -> [x] -> [y]
smartMap 0  f xs = map f xs
smartMap 1  f xs = parMap rdeepseq f xs
smartMap cs f xs = map f xs `using` parListChunk cs rdeepseq

smartList :: NFData y => Int -> [y] -> [y]
smartList 0  ys = ys
smartList 1  ys = ys `using` parList rdeepseq
smartList cs ys = ys `using` parListChunk cs rdeepseq
