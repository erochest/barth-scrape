{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Types.Utils where


import           Data.Foldable
import qualified Data.List                       as L
import           Data.Monoid
import qualified Data.Text                       as T
import           Data.Text.Buildable
import qualified Data.Text.Format                as F
import           Data.Text.Lazy.Builder
import           Data.Yaml
import           Text.Numeral.Roman
import           Text.XML.Lens

import           BarthPar.Scrape.XML             (buildText)

import           BarthPar.Scrape.Types.Internals


buildElement :: Element -> Builder
buildElement = buildText . NodeElement

roman :: (Profunctor p, Contravariant f, Ord n, Num n) => Optic' p f n T.Text
roman = to toRoman

metaHeader :: a
           -> Getting (First Int) a Int
           -> Getting (First Title) a Title
           -> Value
metaHeader a n title = object [ ("n"    , toJSON $ a ^? n    )
                              , ("title", toJSON $ a ^? title)
                              ]

hr :: Buildable b => [b] -> Builder
hr = fold . L.intersperse "\n\n---\n\n" . fmap build

left0 :: Buildable b => Int -> b -> Builder
left0 = (`F.left` '0')
