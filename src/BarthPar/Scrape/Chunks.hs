{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Chunks where


import           Control.Error
import           Control.Lens           hiding ((|>))
import           Data.Csv
import qualified Data.List              as L
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Buildable
import           Data.Text.Encoding
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import           Data.Tuple             (swap)
import qualified Data.Vector            as V

import           BarthPar.Scrape.Types
import           BarthPar.Scrape.Utils  (window)
import           BarthPar.Utils


chunk :: Chunking -> MetadataTarget -> Corpus ContentBlock -> [Output]

chunk VolumeChunks metaTarget c =
    map (makeOutput metaTarget) $ c ^. corpus

chunk PartChunks metaTarget c =
    map (makeOutput metaTarget)
        $ c ^.. corpus . traverse . volumeContent . traverse

chunk ChapterChunks metaTarget c =
    map (makeOutput metaTarget)
        $ c ^.. corpus . traverse
              . volumeContent . traverse
              . partContent . traverse

chunk ParagraphChunks metaTarget c =
    map (makeOutput metaTarget)
        $ c ^.. corpus . traverse
              . volumeContent . traverse
              . partContent . traverse
              . chapterContent . traverse

chunk BlockChunks metaTarget c =
    map (makeOutput metaTarget)
        $ c ^.. corpus . traverse
              . volumeContent . traverse
              . partContent . traverse
              . chapterContent . traverse
              . paragraphContent . traverse

chunk (SizedChunks s) metaTarget c =
    map (makeOutput metaTarget)
        . toListOf ( traverse
                   . volumeContent . traverse
                   . partContent . traverse
                   . chapterContent . traverse
                   . paragraphContent . traverse
                   )
        . map (rechunkV s)
        $ c ^. corpus

withCsv :: (ToNamedRecord a, DefaultOrdered a)
        => FilePath -> MetadataTarget -> a -> Maybe CsvOutput
withCsv _  TargetNone       _ = Nothing
withCsv _  TargetYamlHeader _ = Nothing
withCsv _  TargetJSON       _ = Nothing
withCsv fp TargetCSV r        =
    Just $ CsvOutput ("filename" `V.cons` headerOrder r)
                     ([("filename", encodeUtf8 $ T.pack fp)] <> toNamedRecord r)

makeOutput :: ( Buildable o
              , Filed o
              , Metadata o
              , DefaultOrdered o
              , ToNamedRecord o
              )
           => MetadataTarget -> o -> Output
makeOutput meta v =
    Output outputPath (asMetadata v) (build v) (withCsv outputPath meta v)
    where
        outputPath = getFilePath v

rechunkV :: Int -> Volume ContentBlock -> Volume Chunk
rechunkV size = over (volumeContent . traverse) (rechunkP size)

rechunkP :: Int -> Part ContentBlock -> Part Chunk
rechunkP size = over (partContent . traverse) (rechunkC size)

rechunkC :: Int -> Chapter ContentBlock -> Chapter Chunk
rechunkC size c = Chapter
                { _chapterVolume   = _chapterVolume   c
                , _chapterPart     = _chapterPart     c
                , _chapterAbstract = rechunk size (_chapterAbstract c)
                , _chapterN        = _chapterN        c
                , _chapterTitle    = _chapterTitle    c
                , _chapterContent  = fmap (rechunkPg size) (_chapterContent c)
                }

rechunkPg :: Int -> Paragraph ContentBlock -> Paragraph Chunk
rechunkPg size = over paragraphContent (rechunk size)

rechunk :: Int -> [ContentBlock] -> [Chunk]
rechunk size bs@(b:_) = uncurry (L.zipWith3 c [0 :: Int ..])
                      . L.unzip
                      . fmap swap
                      . mapMaybe ( sequenceA
                                 . swap
                                 . (headZ `bimap` (normalizeWrap . TL.toStrict . TL.unwords))
                                 )
                      . fmap L.unzip
                      . window size (floor $ fromIntegral size / (2.0 :: Double))
                      . zip [0 :: Int ..]
                      . TL.words
                      . toLazyText
                      . mconcat
                      . L.intersperse "\n\n"
                      $ fmap build bs
    where
        c = Chunk (_blockVolume b) (_blockPart b) (_blockChapter b)
                  (_blockParagraph b)
rechunk _ _ = []
