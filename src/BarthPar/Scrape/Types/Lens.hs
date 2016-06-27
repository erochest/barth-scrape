module BarthPar.Scrape.Types.Lens where


import           Control.Lens

import           BarthPar.Scrape.Types.DOM
import           BarthPar.Scrape.Types.Internals


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

paragraphVolumeID :: Lens' (Paragraph c) Int
paragraphVolumeID = paragraphVolume . headerN

paragraphVolumeTitle :: Lens' (Paragraph c) Title
paragraphVolumeTitle = paragraphVolume . headerTitle

paragraphChapterID :: Lens' (Paragraph c) Int
paragraphChapterID = paragraphChapter . headerN

paragraphChapterTitle :: Lens' (Paragraph c) Title
paragraphChapterTitle = paragraphChapter . headerTitle

chapterVolumeID :: Lens' (Chapter c) Int
chapterVolumeID = chapterVolume . headerN

chapterVolumeTitle :: Lens' (Chapter c) Title
chapterVolumeTitle = chapterVolume . headerTitle

partVolumeID :: Lens' (Part c) Int
partVolumeID = partVolume . headerN

partVolumeTitle :: Lens' (Part c) Title
partVolumeTitle = partVolume . headerTitle
