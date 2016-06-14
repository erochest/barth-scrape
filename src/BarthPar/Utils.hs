module BarthPar.Utils where


import qualified Data.Text           as T
import qualified Data.Vector.Unboxed as UV


euclid :: UV.Vector Double -> UV.Vector Double -> Double
euclid xs ys = sqrt . UV.sum . UV.map (**2) $ UV.zipWith (-) xs ys

normalize :: T.Text -> T.Text
normalize = T.unwords . T.words
