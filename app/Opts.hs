{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Opts where


import           Control.Error
import           Data.Char
import qualified Data.Text             as T
import           Options.Applicative   hiding ((<$>), (<*>))
import           Text.Read             (readMaybe)

import           BarthPar.Scrape.Types hiding (Scrape, chunk, header)
import           BarthPar.Types


csvToJsonOpts :: Parser Actions
csvToJsonOpts =
    CsvToJson
    <$> strOption   (  short 'i' <> long "input" <> metavar "TSV_FILE"
                    <> help "The input file (the weights file from MALLET).")
    <*> strOption   (  short 'o' <> long "output" <> metavar "JSON_FILE"
                    <> help "The JSON file to write the output to.")
    <*> option auto (  short 'c' <> long "chunk-size" <> metavar "CHUNK_SIZE"
                    <> value 0
                    <> help "The size of chunks to divide sequences into for\
                            \ parallel processing. (Default is 0, which means\
                            \ no parallelization.)")

inputOpts :: Parser (Either FilePath String)
inputOpts =
    fmap Right (strOption (  short 'u' <> long "root-url" <> metavar "URL"
                          <> help "The URL for the root to scrape."))
    <|>
    fmap Left (strOption (  short 'f' <> long "root-file" <> metavar "FILEPATH"
                         <> help "The path to the root file to scrape."))

scrapeOpts :: Parser Actions
scrapeOpts =
    Scrape
    <$> switch    (  short 'd' <> long "debug" <> help "More debugging output.")
    <*> switch    (  short 'c' <> long "clean"
                  <> help "Remove all files from the output directory before\
                          \ processing.")
    <*> inputOpts
    <*> option mtVal (  short 'm' <> long "metadata" <> metavar "TARGET"
                     <> value TargetNone
                     <> help "How to handle the metadata. This is one of\
                             \ 'none' (none), 'yaml' (YAML header),\
                             \ 'json' (JSON side file), 'csv' (CSV side\
                             \ file including content).")
    <*> option chunkVal (  short 'c' <> long "chunking" <> metavar "CHUNKING"
                        <> value ParagraphChunks
                        <> help "How to chunk the input documents in the \
                                \output. This can be 'volume'; 'part'; \
                                \'chapter'; 'paragraph'; 'block' (by \
                                \punctuated-paragraph and excursus); or a \
                                \number for chunks within a paragraph for a \
                                \given size. Default is by paragraph.")
    <*> strOption (  short 'o' <> long "output" <> metavar "DIRNAME"
                  <> help "The directory to put the scraped documents into.")

pageOpts :: Parser Actions
pageOpts
    = ScrapePage
    <$> strOption (  short 'i' <> long "input" <> metavar "FILENAME"
                  <> help "The page to process.")
    <*> option (T.pack <$> str)
               (  short 'V' <> long "volume" <> metavar "VOLUME_TITLE"
               <> value "CD Volume I,1 (§§ 1-12)"
               <> help "The volume title to use for this. \
                       \Default = 'CD Volume I,1 (§§ 1-12)''.")
    <*> option (T.pack <$> str)
               (  short 'p' <> long "page" <> metavar "PAGE_TITLE"
               <> value "§ 1 The Task of Dogmatics"
               <> help "The page title to use for this. \
                       \Default = '§ 1 The Task of Dogmatics'.")
    <*> option mtVal (  short 'm' <> long "metadata" <> metavar "TARGET"
                     <> value TargetNone
                     <> help "How to handle the metadata. This is one of\
                             \ 'none' (none), 'yaml' (YAML header),\
                             \ 'json' (JSON side file), 'csv' (CSV side\
                             \ file including content).")
    <*> option chunkVal (  short 'c' <> long "chunking" <> metavar "CHUNKING"
                        <> value ParagraphChunks
                        <> help "How to chunk the input documents in the \
                                \output. This can be 'volume'; 'part'; \
                                \'chapter'; 'paragraph'; 'block' (by \
                                \punctuated-paragraph and excursus); or a \
                                \number for chunks within a paragraph for a \
                                \given size. Default is by paragraph.")
    <*> strOption (  short 'o' <> long "output" <> metavar "DIRNAME"
                  <> help "The directory to put the scraped documents into.")

mtVal :: ReadM MetadataTarget
mtVal = fmap (fmap toLower) str >>= \case
        'n':_ -> return TargetNone
        'y':_ -> return TargetYamlHeader
        'j':_ -> return TargetJSON
        'c':_ -> return TargetCSV
        e     -> fail $ "Invalid metadata target: '" ++ e ++ "'."

chunkVal :: ReadM Chunking
chunkVal = fmap (fmap toLower) str >>= chunk
    where
        chunk :: String -> ReadM Chunking
        chunk ('v':_) = return VolumeChunks
        chunk "part"  = return PartChunks
        chunk ('c':_) = return ChapterChunks
        chunk ('p':_) = return ParagraphChunks
        chunk ('b':_) = return BlockChunks
        chunk input | all isDigit input = maybe fail' (return . SizedChunks) $ readMaybe input
                    | otherwise         = fail'
                    where
                        fail' = fail $ "Invalid chunking: '" ++ input ++ "'."

opts' :: Parser Actions
opts' = subparser
        (  command "csv-to-json"
               (info (helper <*> csvToJsonOpts)
                    (progDesc "Convert the MALLET CSV output to JSON."))
        <> command "scrape"
               (info (helper <*> scrapeOpts)
                    (progDesc "Download and extract the text from the\
                              \ site."))
        <> command "page"
                (info (helper <*> pageOpts)
                    (progDesc "Download and extract the text from a\
                              \ single page."))
        )

opts :: ParserInfo Actions
opts =   info (helper <*> opts')
              (  fullDesc
              <> progDesc "Read a MALLET term weighting file into a JSON\
                          \ network."
              <> header "barth-par -- parallel reading a MALLET term weighting\
                        \  file into a JSON network.")

parseOpts :: Script Actions
parseOpts = scriptIO $ execParser opts
