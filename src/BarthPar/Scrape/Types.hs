{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module BarthPar.Scrape.Types where


import           Control.Arrow          ((&&&))
import           Control.DeepSeq
import           Control.Error
import           Control.Lens           hiding ((.=))
import           Control.Monad.Reader
import qualified Data.Aeson             as A
import           Data.Csv               hiding (HasHeader (..), Header, header)
import qualified Data.Csv               as Csv
import           Data.Data
import           Data.Foldable
import qualified Data.List              as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text              as T
import           Data.Text.Buildable
import qualified Data.Text.Format       as F
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import qualified Data.Vector            as V
import           Data.Yaml              hiding ((.=))
import           GHC.Generics           hiding (to)
import           Lucid
import           Network.URI
import           Text.Numeral.Roman
import           Text.XML.Lens          hiding (to, (.=), (|>))

import           BarthPar.Scrape.XML    (buildText)


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


type Title       = T.Text
type Content     = T.Text
type MetaMap     = Object
type PureScript  = Either String
type InputSource = PureScript URI

class Metadata a where
    asMetadata :: a -> MetaMap

class Titled a where
    getTitle :: a -> Title

class Filed a where
    getFilePath :: a -> FilePath

data Header
    = Header
    { _headerN     :: !Int
    , _headerTitle :: !Title
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''Header)

instance NFData Header

instance Ord Header where
    compare = comparing _headerN

instance ToJSON Header where
    toJSON (Header n t) = A.object [ "n"     A..= n
                                   , "title" A..= t
                                   ]

instance Metadata Header where
    asMetadata (Header n title) =
        [ ("n"    , toJSON n    )
        , ("title", toJSON title)
        ]

instance Buildable Header where
    build (Header n t) = F.build "{}. {}" (n, t)

data MetadataTarget
    = TargetNone
    | TargetYamlHeader
    | TargetJSON
    | TargetCSV
    deriving (Show, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)
$(makePrisms ''MetadataTarget)

instance NFData MetadataTarget

data Chunking
    = VolumeChunks      -- ^ CD I
    | PartChunks        -- ^ CD I.1
    | ChapterChunks     -- ^ CD I.1.§1
    | ParagraphChunks   -- ^ CD I.1.§1.1
    | BlockChunks       -- ^ CD I.1.§1.1.1
    | SizedChunks       { _chunkSize :: !Int }
    deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''Chunking)
$(makeClassy ''Chunking)

instance NFData Chunking

data ScrapeState
    = ScrapeState
    { _scrapeDebugging :: !Bool
    , _scrapeMetadata  :: !MetadataTarget
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeClassy ''ScrapeState)

instance NFData ScrapeState

newtype Scrape a = Scrape { unScrape :: ReaderT ScrapeState Script a }
    deriving (Functor, Applicative, Monad, MonadReader ScrapeState)

toScript :: Bool -> MetadataTarget-> Scrape a -> Script a
toScript debugging target s =
    runReaderT (unScrape s) $ ScrapeState debugging target

runScrape :: Bool -> MetadataTarget -> Scrape a -> IO a
runScrape debugging target s = runScript $ toScript debugging target s

io :: PureScript a -> Scrape a
io ps = Scrape . ReaderT $ const $ hoistEither ps

scrape :: Script a -> Scrape a
scrape s = Scrape . ReaderT $ const s

scrapeIO :: IO a -> Scrape a
scrapeIO s = Scrape . ReaderT $ const $ scriptIO s

throwS :: String -> Scrape a
throwS = scrape . throwE

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

chunkVolumeID :: Lens' Chunk Int
chunkVolumeID = chunkVolume . headerN

chunkVolumeTitle :: Lens' Chunk Title
chunkVolumeTitle = chunkVolume . headerTitle

chunkChapterID :: Lens' Chunk Int
chunkChapterID = chunkChapter . headerN

chunkChapterTitle :: Lens' Chunk Title
chunkChapterTitle = chunkChapter . headerTitle

chunkParagraphID :: Lens' Chunk Int
chunkParagraphID = chunkParagraph . headerN

chunkParagraphTitle :: Lens' Chunk Title
chunkParagraphTitle = chunkParagraph . headerTitle

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

instance Metadata Chunk where
    asMetadata c = [ ("volume"   , toJSON (c ^. chunkVolume))
                   , ("part"     , toJSON (c ^. chunkPart))
                   , ("chapter"  , toJSON (c ^. chunkChapter))
                   , ("paragraph", toJSON (c ^. chunkParagraph))
                   , ("n"        , toJSON (c ^. chunkN))
                   , ("start"    , toJSON (c ^. chunkStart))
                   ]

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

instance ToNamedRecord Chunk where
    toNamedRecord c =
        namedRecord [ "volume_n"        .= (c ^. chunkVolumeID)
                    , "volume_title"    .= (c ^. chunkVolumeTitle)
                    , "part"            .= (c ^. chunkPart)
                    , "chapter_n"       .= (c ^. chunkChapterID)
                    , "chapter_title"   .= (c ^. chunkChapterTitle)
                    , "paragraph_n"     .= (c ^. chunkParagraphID)
                    , "paragraph_title" .= (c ^. chunkParagraphTitle)
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

blockVolumeID :: Lens' ContentBlock Int
blockVolumeID = blockVolume . headerN

blockVolumeTitle :: Lens' ContentBlock Title
blockVolumeTitle = blockVolume . headerTitle

blockChapterID :: Lens' ContentBlock Int
blockChapterID = blockChapter . headerN

blockChapterTitle :: Lens' ContentBlock Title
blockChapterTitle = blockChapter . headerTitle

blockParagraphID :: Lens' ContentBlock Int
blockParagraphID = blockParagraph . headerN

blockParagraphTitle :: Lens' ContentBlock Title
blockParagraphTitle = blockParagraph . headerTitle

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

instance Metadata ContentBlock where
    asMetadata b = [ ("volume"   , toJSON (b ^. blockVolume))
                   , ("part"     , toJSON (b ^. blockPart))
                   , ("chapter"  , toJSON (b ^. blockChapter))
                   , ("paragraph", toJSON (b ^. blockParagraph))
                   , ("block"    , toJSON (b ^. blockN))
                   ]

instance Titled ContentBlock where
    getTitle b = TL.toStrict
               $ F.format "CD {}.{}.§{}.{} ({})"
                          ( b ^. blockVolumeID . roman
                          , b ^. blockPart
                          , b ^. blockChapterID
                          , b ^. blockParagraphID
                          , b ^. blockN
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

instance ToNamedRecord ContentBlock where
    toNamedRecord b =
        namedRecord [ "volume_n"        .= (b ^. blockVolumeID)
                    , "volume_title"    .= (b ^. blockVolumeTitle)
                    , "part"            .= (b ^. blockPart)
                    , "chapter_n"       .= (b ^. blockChapterID)
                    , "chapter_title"   .= (b ^. blockChapterTitle)
                    , "paragraph_n"     .= (b ^. blockParagraphID)
                    , "paragraph_title" .= (b ^. blockParagraphTitle)
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

paragraphVolumeID :: Lens' (Paragraph c) Int
paragraphVolumeID = paragraphVolume . headerN

paragraphVolumeTitle :: Lens' (Paragraph c) Title
paragraphVolumeTitle = paragraphVolume . headerTitle

paragraphChapterID :: Lens' (Paragraph c) Int
paragraphChapterID = paragraphChapter . headerN

paragraphChapterTitle :: Lens' (Paragraph c) Title
paragraphChapterTitle = paragraphChapter . headerTitle

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

instance Metadata (Paragraph c) where
    asMetadata p = [ ("volume"   , toJSON (p ^. paragraphVolume))
                   , ("part"     , toJSON (p ^. paragraphPart))
                   , ("chapter"  , toJSON (p ^. paragraphChapter))
                   , ("paragraph", toJSON (Header (p ^. paragraphN)
                                                  (p ^. paragraphTitle)))
                   ]

instance Titled (Paragraph c) where
    getTitle p = TL.toStrict
               $ F.format "CD {}.{}.{}.§{}"
                          ( p ^. paragraphVolumeID . roman
                          , p ^. paragraphPart
                          , p ^. paragraphChapterID
                          , p ^. paragraphN
                          )

instance Filed (Paragraph c) where
    getFilePath p = TL.unpack
                  $ F.format "paragraph-{}-{}-{}-{}.md"
                  ( p ^. paragraphVolumeID
                  , p ^. paragraphPart
                  , left0 2 $ p ^. paragraphChapterID
                  , left0 3 $ p ^. paragraphN
                  )

instance ToNamedRecord (Paragraph c) where
    toNamedRecord p =
        namedRecord [ "volume_n"        .= (p ^. paragraphVolumeID)
                    , "volume_title"    .= (p ^. paragraphVolumeTitle)
                    , "part"            .= (p ^. paragraphPart)
                    , "chapter_n"       .= (p ^. paragraphChapterID)
                    , "chapter_title"   .= (p ^. paragraphChapterTitle)
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

data ChapterLoc
    = ChapterLoc
    { _locVolume  :: !Int
    , _locPart    :: !Int
    , _locChapter :: !Int
    } deriving (Show, Eq, Ord, Data, Typeable, Generic)
$(makeClassy ''ChapterLoc)

instance NFData ChapterLoc

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

chapterVolumeID :: Lens' (Chapter c) Int
chapterVolumeID = chapterVolume . headerN

chapterVolumeTitle :: Lens' (Chapter c) Title
chapterVolumeTitle = chapterVolume . headerTitle

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

instance Metadata (Chapter c) where
    asMetadata c = [ ("volume" , toJSON (c ^. chapterVolume))
                   , ("part"   , toJSON (c ^. chapterPart))
                   , ("chapter", toJSON (Header (c ^. chapterN)
                                                (c ^. chapterTitle)))
                   ]

instance Titled (Chapter c) where
    getTitle c = TL.toStrict
               $ F.format "CD {}.{}.{}"
                          ( c ^. chapterVolumeID . roman
                          , c ^. chapterPart
                          , c ^. chapterN
                          )

instance Filed (Chapter c) where
    getFilePath c = TL.unpack
                  $ F.format "chapter-{}-{}-{}.md"
                  ( c ^. chapterVolumeID
                  , c ^. chapterPart
                  , left0 2 $ c ^. chapterN
                  )

instance ToNamedRecord (Chapter c) where
    toNamedRecord c =
        namedRecord [ "volume_n"      .= (c ^. chapterVolumeID)
                    , "volume_title"  .= (c ^. chapterVolumeTitle)
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

partVolumeID :: Lens' (Part c) Int
partVolumeID = partVolume . headerN

partVolumeTitle :: Lens' (Part c) Title
partVolumeTitle = partVolume . headerTitle

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

instance Metadata (Part c) where
    asMetadata p = [ ("volume" , toJSON (p ^. partVolume))
                   , ("part"   , toJSON (p ^. partN))
                   ]

instance Titled (Part c) where
    getTitle p = TL.toStrict
               $ F.format "CD {}.{}"
                          (p ^? partVolumeID . roman, p ^? partN)

instance Filed (Part c) where
    getFilePath p = TL.unpack
                  $ F.format "part-{}-{}.md"
                  ( p ^. partVolumeID
                  , p ^. partN
                  )

instance ToNamedRecord (Part c) where
    toNamedRecord p =
        namedRecord [ "volume_n"     .= (p ^. partVolumeID)
                    , "volume_title" .= (p ^. partVolumeTitle)
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

instance Metadata (Volume c) where
    asMetadata v = [("volume", toJSON ([ ("n"    , toJSON (v ^. volumeN))
                                       , ("title", toJSON (getTitle v))
                                       ] :: Object)
                    )]

instance Titled (Volume c) where
    getTitle v = TL.toStrict
               . F.format "CD {}"
               . F.Only
               $ v ^? volumeN . roman

instance Filed (Volume c) where
    getFilePath v = TL.unpack
                  . F.format "volume-{}.md"
                  . F.Only
                  $ v ^. volumeN

instance ToNamedRecord (Volume c) where
    toNamedRecord v =
        namedRecord [ "volume_n"        .= (v ^. volumeN)
                    , "volume_title"    .= getTitle v
                    ]

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

data Saved x
    = Saved
    { _savedFilePath :: !FilePath
    , _savedData     :: !x
    } deriving (Show, Eq, Data, Typeable, Generic, Functor)
$(makeLenses ''Saved)

instance NFData x => NFData (Saved x)

instance ToNamedRecord x => ToNamedRecord (Saved x) where
    toNamedRecord s =
        toNamedRecord (s ^. savedData)
        <>
        namedRecord [ "filepath" .= (s ^. savedFilePath) ]

instance DefaultOrdered x => DefaultOrdered (Saved x) where
    headerOrder s = "filepath" `V.cons` headerOrder (s ^. savedData)

data CsvOutput
    = CsvOutput
    { _csvHeader :: !Csv.Header
    , _csvRecord :: !NamedRecord
    } deriving (Show, Eq)
$(makeClassy ''CsvOutput)

data Output
    = Output
    { _outputFilePath :: !FilePath
    , _outputMetadata :: !MetaMap
    , _outputContent  :: !Builder
    , _outputCsv      :: !(Maybe CsvOutput)
    } deriving (Show, Eq)
$(makeClassy ''Output)
