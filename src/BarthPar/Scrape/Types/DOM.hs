{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module BarthPar.Scrape.Types.DOM where


import           Control.Arrow                   ((&&&))
import           Control.DeepSeq
import           Control.Lens                    hiding ((.=))
import           Data.Csv                        hiding (HasHeader (..), Header,
                                                  header)
import           Data.Data
import           Data.Ord
import           Data.Text.Buildable
import qualified Data.Text.Format                as F
import qualified Data.Text.Lazy                  as TL
import           GHC.Generics                    hiding (to)
import           Lucid

import           BarthPar.Scrape.Types.Internals
import           BarthPar.Scrape.Types.Utils


data Chunk
    = Chunk
    { _chunkVolume    :: !Header
    , _chunkPart      :: !Int
    , _chunkChapter   :: !Header
    , _chunkParagraph :: !Header
    , _chunkN         :: !Int
    , _chunkStart     :: !Int
    , _chunkContent   :: !Content
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Chunk)

instance NFData Chunk

instance Ord Chunk where
    compare = comparing (   _chunkVolume
                        &&& _chunkPart
                        &&& _chunkChapter
                        &&& _chunkParagraph
                        &&& _chunkN
                        )

instance ToHtml Chunk where
    toHtml Chunk{..} =
        details_ $ do
            summary_ . toHtml $ F.format "Chunk #{} @ {}" (_chunkN, _chunkStart)
            div_ $ toHtml _chunkContent
    toHtmlRaw Chunk{..} =
        details_ $ do
            summary_ . toHtmlRaw
                     $ F.format "Chunk #{} @ {}" (_chunkN, _chunkStart)
            div_ $ toHtmlRaw _chunkContent


instance ToNamedRecord Chunk where
    toNamedRecord c =
        namedRecord [ "volume_n"        .= (c ^. chunkVolume . headerN)
                    , "volume_title"    .= (c ^. chunkVolume . headerTitle)
                    , "part"            .= (c ^. chunkPart)
                    , "chapter_n"       .= (c ^. chunkChapter . headerN)
                    , "chapter_title"   .= (c ^. chunkChapter . headerTitle)
                    , "paragraph_n"     .= (c ^. chunkParagraph . headerN)
                    , "paragraph_title" .= (c ^. chunkParagraph . headerTitle)
                    , "chunk_n"         .= (c ^. chunkN)
                    , "chunk_start"     .= (c ^. chunkStart)
                    ]

instance DefaultOrdered Chunk where
    headerOrder _ = [ "volume_n"
                    , "volume_title"
                    , "part"
                    , "chapter_n"
                    , "chapter_title"
                    , "paragraph_n"
                    , "paragraph_title"
                    , "chunk_n"
                    , "chunk_start"
                    ]

instance Buildable Chunk where
    build = build . view chunkContent

data ContentBlock                           -- ^ CD I.1.1.§.N
    = ContentBlock
    { _blockVolume    :: !Header
    , _blockPart      :: !Int
    , _blockChapter   :: !Header
    , _blockParagraph :: !Header
    , _blockN         :: !Int
    , _blockContent   :: !Content
    , _blockExcursus  :: !Content
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''ContentBlock)

instance NFData ContentBlock

instance Ord ContentBlock where
    compare = comparing (   _blockVolume
                        &&& _blockPart
                        &&& _blockChapter
                        &&& _blockParagraph
                        &&& _blockN
                        )

instance ToHtml ContentBlock where
    toHtml ContentBlock{..} =
        details_ $ do
            summary_ . toHtml . F.format "Block #{}" $ F.Only _blockN
            div_ $ toHtml _blockContent
            blockquote_ $ toHtml _blockExcursus
    toHtmlRaw ContentBlock{..} =
        details_ $ do
            summary_ . toHtmlRaw . F.format "Block #{}" $ F.Only _blockN
            div_ $ toHtmlRaw _blockContent
            blockquote_ $ toHtmlRaw _blockExcursus

instance ToNamedRecord ContentBlock where
    toNamedRecord b =
        namedRecord [ "volume_n"        .= (b ^. blockVolume . headerN)
                    , "volume_title"    .= (b ^. blockVolume . headerTitle)
                    , "part"            .= (b ^. blockPart)
                    , "chapter_n"       .= (b ^. blockChapter . headerN)
                    , "chapter_title"   .= (b ^. blockChapter . headerTitle)
                    , "paragraph_n"     .= (b ^. blockParagraph . headerN)
                    , "paragraph_title" .= (b ^. blockParagraph . headerTitle)
                    , "block_n"         .= (b ^. blockN)
                    ]

instance DefaultOrdered ContentBlock where
    headerOrder _ = [ "volume_n"
                    , "volume_title"
                    , "part"
                    , "chapter_n"
                    , "chapter_title"
                    , "paragraph_n"
                    , "paragraph_title"
                    , "block_n"
                    ]

instance Buildable ContentBlock where
    build b = hr [ b ^. blockContent
                 , b ^. blockExcursus
                 ]

data Paragraph c
    = Paragraph                         -- ^ CD I.1.1.§
    { _paragraphVolume  :: !Header
    , _paragraphPart    :: !Int
    , _paragraphChapter :: !Header
    , _paragraphN       :: !Int
    , _paragraphTitle   :: !Title
    , _paragraphContent :: ![c]
    } deriving (Show, Eq, Data, Typeable, Generic, Functor)
$(makeLenses ''Paragraph)

instance NFData c => NFData (Paragraph c)

instance Eq c => Ord (Paragraph c) where
    compare = comparing (   _paragraphVolume
                        &&& _paragraphPart
                        &&& _paragraphChapter
                        &&& _paragraphN
                        )

instance ToHtml c => ToHtml (Paragraph c) where
    toHtml Paragraph{..} =
        details_ $ do
            summary_ . toHtml
                     $ F.format "¶ {}. {}" (_paragraphN, _paragraphTitle)
            mapM_ toHtml _paragraphContent
    toHtmlRaw Paragraph{..} =
        details_ $ do
            summary_ . toHtmlRaw
                     $ F.format "¶ {}. {}" (_paragraphN, _paragraphTitle)
            mapM_ toHtmlRaw _paragraphContent

instance ToNamedRecord (Paragraph c) where
    toNamedRecord p =
        namedRecord [ "volume_n"        .= (p ^. paragraphVolume . headerN)
                    , "volume_title"    .= (p ^. paragraphVolume . headerTitle)
                    , "part"            .= (p ^. paragraphPart)
                    , "chapter_n"       .= (p ^. paragraphChapter . headerN)
                    , "chapter_title"   .= (p ^. paragraphChapter . headerTitle)
                    , "paragraph_n"     .= (p ^. paragraphN)
                    , "paragraph_title" .= (p ^. paragraphTitle)
                    ]

instance DefaultOrdered (Paragraph c) where
    headerOrder _ = [ "volume_n"
                    , "volume_title"
                    , "part"
                    , "chapter_n"
                    , "chapter_title"
                    , "paragraph_n"
                    , "paragraph_title"
                    ]

instance Buildable c => Buildable (Paragraph c) where
    build p = hr $ p ^. paragraphContent

data Chapter c
    = Chapter                           -- ^ CD I.1.§
    { _chapterVolume   :: !Header
    , _chapterPart     :: !Int
    , _chapterAbstract :: ![c]
    , _chapterN        :: !Int
    , _chapterTitle    :: !Title
    , _chapterContent  :: ![Paragraph c]
    } deriving (Show, Eq, Data, Typeable, Generic, Functor)
$(makeLenses ''Chapter)

instance NFData c => NFData (Chapter c)

instance Eq c => Ord (Chapter c) where
    compare = comparing (   _chapterVolume
                        &&& _chapterPart
                        &&& _chapterN
                        )

instance ToHtml c => ToHtml (Chapter c) where
    toHtml Chapter{..} =
        details_ $ do
            summary_ . toHtml
                     $ F.format "Chapter {}. {}" (_chapterN, _chapterTitle)
            blockquote_ $ mapM_ toHtml _chapterAbstract
            mapM_ toHtml _chapterContent
    toHtmlRaw Chapter{..} =
        details_ $ do
            summary_ . toHtmlRaw
                     $ F.format "Chapter {}. {}" (_chapterN, _chapterTitle)
            blockquote_ $ mapM_ toHtmlRaw _chapterAbstract
            mapM_ toHtmlRaw _chapterContent

instance ToNamedRecord (Chapter c) where
    toNamedRecord c =
        namedRecord [ "volume_n"      .= (c ^. chapterVolume . headerN)
                    , "volume_title"  .= (c ^. chapterVolume . headerTitle)
                    , "part"          .= (c ^. chapterPart)
                    , "chapter_n"     .= (c ^. chapterN)
                    , "chapter_title" .= (c ^. chapterTitle)
                    ]

instance DefaultOrdered (Chapter c) where
    headerOrder _ = [ "volume_n"
                    , "volume_title"
                    , "part"
                    , "chapter_n"
                    , "chapter_title"
                    ]

instance Buildable c => Buildable (Chapter c) where
    build c = hr
            . (foldMap build (c ^. chapterAbstract):)
            . fmap build
            $ c ^. chapterContent

data Part c
    = Part                              -- ^ CD I.1
    { _partVolume  :: !Header
    , _partN       :: !Int
    , _partContent :: ![Chapter c]
    } deriving (Show, Eq, Data, Typeable, Generic, Functor)
$(makeLenses ''Part)

instance NFData c => NFData (Part c)

instance Eq c => Ord (Part c) where
    compare = comparing (_partVolume &&& _partN)

instance ToHtml c => ToHtml (Part c) where
    toHtml Part{..} =
        details_ $ do
            summary_ . toHtml . F.format "Part {}" $ F.Only _partN
            mapM_ toHtml _partContent
    toHtmlRaw Part{..} =
        details_ $ do
            summary_ . toHtmlRaw . F.format "Part {}" $ F.Only _partN
            mapM_ toHtml _partContent

instance ToNamedRecord (Part c) where
    toNamedRecord p =
        namedRecord [ "volume_n"     .= (p ^. partVolume . headerN)
                    , "volume_title" .= (p ^. partVolume . headerTitle)
                    , "part"         .= (p ^. partN)
                    ]

instance DefaultOrdered (Part c) where
    headerOrder _ = [ "volume_n"
                    , "volume_title"
                    , "part"
                    ]

instance Buildable c => Buildable (Part c) where
    build p = hr $ p ^. partContent

data Volume c                          -- ^ CD I
    = Volume
    { _volumeN       :: !Int
    , _volumeTitle   :: !Title
    , _volumeContent :: ![Part c]
    } deriving (Show, Eq, Data, Typeable, Generic, Functor)
$(makeLenses ''Volume)

instance NFData c => NFData (Volume c)

instance ToHtml c => ToHtml (Volume c) where
    toHtml Volume{..} =
        details_ $ do
            summary_ . toHtml $ F.format "V {}. {}" (_volumeN, _volumeTitle)
            mapM_ toHtml _volumeContent
    toHtmlRaw Volume{..} =
        details_ $ do
            summary_ . toHtmlRaw $ F.format "V {}. {}" (_volumeN, _volumeTitle)
            mapM_ toHtmlRaw _volumeContent

instance Eq c => Ord (Volume c) where
    compare = comparing _volumeN

instance ToNamedRecord (Volume c) where
    toNamedRecord v =
        namedRecord [ "volume_n"        .= (v ^. volumeN)
                    , "volume_title"    .= getTitle v
                    ]
        where
            getTitle v' = TL.toStrict
                        . F.format "CD {}"
                        . F.Only
                        $ v' ^? volumeN . roman

instance DefaultOrdered (Volume c) where
    headerOrder _ = [ "volume_n"
                    , "volume_title"
                    ]

instance Buildable c => Buildable (Volume c) where
    build v = hr $ v ^. volumeContent

data Corpus c = Corpus { _corpus :: ![Volume c] }
    deriving (Show, Eq, Data, Typeable, Generic, Functor)
$(makeLenses ''Corpus)

instance NFData c => NFData (Corpus c)

instance ToHtml c => ToHtml (Corpus c) where
    toHtml    = mapM_ toHtml . _corpus
    toHtmlRaw = mapM_ toHtmlRaw . _corpus
