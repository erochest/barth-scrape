{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Types.Filed where


import           Control.Lens
import qualified Data.Text.Format            as F
import qualified Data.Text.Lazy              as TL

import           BarthPar.Scrape.Types.DOM
import           BarthPar.Scrape.Types.Lens
import           BarthPar.Scrape.Types.Utils


class Filed a where
    getFilePath :: a -> FilePath

instance Filed Chunk where
    getFilePath c = TL.unpack
                  $ F.format "chunk-{}-{}-{}-{}-{}-{}.md"
                  ( c ^. chunkVolumeID
                  , c ^. chunkPart
                  , left0 2 $ c ^. chunkChapterID
                  , left0 3 $ c ^. chunkParagraphID
                  , left0 4 $ c ^. chunkN
                  , left0 6 $ c ^. chunkStart
                  )

instance Filed ContentBlock where
    getFilePath b = TL.unpack
                  $ F.format "block-{}-{}-{}-{}-{}.md"
                  ( b ^. blockVolumeID
                  , b ^. blockPart
                  , left0 2 $ b ^. blockChapterID
                  , left0 3 $ b ^. blockParagraphID
                  , left0 4 $ b ^. blockN
                  )

instance Filed (Paragraph c) where
    getFilePath p = TL.unpack
                  $ F.format "paragraph-{}-{}-{}-{}.md"
                  ( p ^. paragraphVolumeID
                  , p ^. paragraphPart
                  , left0 2 $ p ^. paragraphChapterID
                  , left0 3 $ p ^. paragraphN
                  )

instance Filed (Chapter c) where
    getFilePath c = TL.unpack
                  $ F.format "chapter-{}-{}-{}.md"
                  ( c ^. chapterVolumeID
                  , c ^. chapterPart
                  , left0 2 $ c ^. chapterN
                  )

instance Filed (Part c) where
    getFilePath p = TL.unpack
                  $ F.format "part-{}-{}.md"
                  ( p ^. partVolumeID
                  , p ^. partN
                  )

instance Filed (Volume c) where
    getFilePath v = TL.unpack
                  . F.format "volume-{}.md"
                  . F.Only
                  $ v ^. volumeN
