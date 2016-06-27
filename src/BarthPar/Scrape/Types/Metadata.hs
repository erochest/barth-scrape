{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Types.Metadata where


import           Control.Lens
import           Data.Yaml

import           BarthPar.Scrape.Types.DOM
import           BarthPar.Scrape.Types.Internals
import           BarthPar.Scrape.Types.Titled


class Metadata a where
    asMetadata :: a -> MetaMap

instance Metadata Header where
    asMetadata (Header n title) =
        [ ("n"    , toJSON n    )
        , ("title", toJSON title)
        ]

instance Metadata Chunk where
    asMetadata c = [ ("volume"   , toJSON (c ^. chunkVolume))
                   , ("part"     , toJSON (c ^. chunkPart))
                   , ("chapter"  , toJSON (c ^. chunkChapter))
                   , ("paragraph", toJSON (c ^. chunkParagraph))
                   , ("n"        , toJSON (c ^. chunkN))
                   , ("start"    , toJSON (c ^. chunkStart))
                   ]

instance Metadata ContentBlock where
    asMetadata b = [ ("volume"   , toJSON (b ^. blockVolume))
                   , ("part"     , toJSON (b ^. blockPart))
                   , ("chapter"  , toJSON (b ^. blockChapter))
                   , ("paragraph", toJSON (b ^. blockParagraph))
                   , ("block"    , toJSON (b ^. blockN))
                   ]

instance Metadata (Paragraph c) where
    asMetadata p = [ ("volume"   , toJSON (p ^. paragraphVolume))
                   , ("part"     , toJSON (p ^. paragraphPart))
                   , ("chapter"  , toJSON (p ^. paragraphChapter))
                   , ("paragraph", toJSON (Header (p ^. paragraphN)
                                                  (p ^. paragraphTitle)))
                   ]

instance Metadata (Chapter c) where
    asMetadata c = [ ("volume" , toJSON (c ^. chapterVolume))
                   , ("part"   , toJSON (c ^. chapterPart))
                   , ("chapter", toJSON (Header (c ^. chapterN)
                                                (c ^. chapterTitle)))
                   ]

instance Metadata (Part c) where
    asMetadata p = [ ("volume" , toJSON (p ^. partVolume))
                   , ("part"   , toJSON (p ^. partN))
                   ]

instance Metadata (Volume c) where
    asMetadata v = [("volume", toJSON ([ ("n"    , toJSON (v ^. volumeN))
                                       , ("title", toJSON (getTitle v))
                                       ] :: Object)
                    )]
