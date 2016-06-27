{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Types.Titled where


import           Control.Lens
import qualified Data.Text.Format                as F
import qualified Data.Text.Lazy                  as TL

import           BarthPar.Scrape.Types.DOM
import           BarthPar.Scrape.Types.Internals
import           BarthPar.Scrape.Types.Lens
import           BarthPar.Scrape.Types.Utils


class Titled a where
    getTitle :: a -> Title

instance Titled Chunk where
    getTitle c = TL.toStrict
               $ F.format "CD {}.{}.§{}.{} ({}@{})"
                          ( c ^. chunkVolumeID . roman
                          , c ^. chunkPart
                          , c ^. chunkChapterID
                          , c ^. chunkParagraphID
                          , c ^. chunkN
                          , c ^. chunkStart
                          )

instance Titled ContentBlock where
    getTitle b = TL.toStrict
               $ F.format "CD {}.{}.§{}.{} ({})"
                          ( b ^. blockVolumeID . roman
                          , b ^. blockPart
                          , b ^. blockChapterID
                          , b ^. blockParagraphID
                          , b ^. blockN
                          )

instance Titled (Paragraph c) where
    getTitle p = TL.toStrict
               $ F.format "CD {}.{}.{}.§{}"
                          ( p ^. paragraphVolumeID . roman
                          , p ^. paragraphPart
                          , p ^. paragraphChapterID
                          , p ^. paragraphN
                          )

instance Titled (Chapter c) where
    getTitle c = TL.toStrict
               $ F.format "CD {}.{}.{}"
                          ( c ^. chapterVolumeID . roman
                          , c ^. chapterPart
                          , c ^. chapterN
                          )

instance Titled (Part c) where
    getTitle p = TL.toStrict
               $ F.format "CD {}.{}"
                          (p ^? partVolumeID . roman, p ^? partN)

instance Titled (Volume c) where
    getTitle v = TL.toStrict
               . F.format "CD {}"
               . F.Only
               $ v ^? volumeN . roman
