{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Utils where


import           Data.Foldable
import           Data.Monoid
import           Data.Sequence          ((|>))
import qualified Data.Sequence          as Seq
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Vector.Unboxed    as UV


euclid :: UV.Vector Double -> UV.Vector Double -> Double
euclid xs ys = sqrt . UV.sum . UV.map (**2) $ UV.zipWith (-) xs ys

normalize :: T.Text -> T.Text
normalize = T.unwords . T.words

paragraphs :: T.Text -> [T.Text]
paragraphs = T.splitOn "\n\n"

normalizeWrap :: T.Text -> T.Text
normalizeWrap = render
              . foldMap (<> "\n\n")
              . fmap ( fromText
                     . T.unlines
                     . finis
                     . foldl' step (0, Seq.empty, Seq.empty)
                     . filter (not . T.null)
                     . T.words
                     )
              . paragraphs
    where
        step :: (Int, Seq.Seq T.Text, Seq.Seq T.Text)
             -> T.Text
             -> (Int, Seq.Seq T.Text, Seq.Seq T.Text)
        step (len, line, accum) word
            | len' >= 80 = ( wordLen
                           , Seq.singleton word
                           , accum |> T.unwords (toList line)
                           )
            | otherwise  = (len', line |> word, accum)
            where
                wordLen = T.length word
                len'    = len + wordLen + Seq.length line - 1

        finis :: (Int, Seq.Seq T.Text, Seq.Seq T.Text) -> [T.Text]
        finis (_, line, accum) = toList $ accum |> T.unwords (toList line)

render :: Builder -> T.Text
render = TL.toStrict . toLazyText
