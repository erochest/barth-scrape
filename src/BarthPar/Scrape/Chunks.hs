{-# LANGUAGE OverloadedStrings #-}


module BarthPar.Scrape.Chunks where


import           Control.Error
import           Control.Lens           hiding ((|>))
import           Data.Csv
import           Data.Foldable
import qualified Data.List              as L
import           Data.Sequence          ((|>))
import qualified Data.Sequence          as Seq
import           Data.Text.Buildable
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import           Data.Tuple             (swap)

import           BarthPar.Scrape.Types


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
        => MetadataTarget -> a -> Maybe CsvOutput
withCsv TargetNone       _ = Nothing
withCsv TargetYamlHeader _ = Nothing
withCsv TargetJSON       _ = Nothing
withCsv TargetCSV r = Just $ CsvOutput (headerOrder r) (toNamedRecord r)

makeOutput :: ( Buildable o
              , Filed o
              , Metadata o
              , DefaultOrdered o
              , ToNamedRecord o
              )
           => MetadataTarget -> o -> Output
makeOutput meta v =
    Output (getFilePath v) (asMetadata v) (build v) (withCsv meta v)

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
                                 . (headZ `bimap` (TL.toStrict . TL.unwords))
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

-- | Based off of http://stackoverflow.com/a/27727244/34864
window :: Int -> Int -> [a] -> [[a]]
window len offset = go 0 Seq.empty
    where
        go n s (a:as) | n' <  len = go n' s' as
                      | n' == len = toList s'  : go n' s' as
                      | otherwise = toList s'' : go n s'' as
            where
                n'  = n + 1
                s'  = s |> a
                s'' = Seq.drop offset s'
        go _ _ _ = []

