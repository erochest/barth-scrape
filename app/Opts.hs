module Opts where


import           Control.Error
import           Options.Applicative hiding ((<$>), (<*>))

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
    (fmap Right (strOption (  short 'u' <> long "root-url" <> metavar "URL"
                           <> help "The URL for the root to scrape.")))
    <|>
    (fmap Left (strOption (  short 'f' <> long "root-file" <> metavar "FILEPATH"
                          <> help "The path to the root file to scrape.")))

scrapeOpts :: Parser Actions
scrapeOpts =
    Scrape
    <$> switch    (  short 'c' <> long "clean"
                  <> help "Remove all files from the output directory before\
                          \ processing.")
    <*> inputOpts
    <*> strOption (  short 'o' <> long "output" <> metavar "DIRNAME"
                  <> help "The directory to put the scraped documents into.")

opts' :: Parser Actions
opts' = subparser
        (  command "csv-to-json"
               (info (helper <*> csvToJsonOpts)
                        (progDesc "Convert the MALLET CSV output to JSON."))
        <> command "scrape"
               (info (helper <*> scrapeOpts)
                         (progDesc "Download and extract the text from the\
                                   \ site."))
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
