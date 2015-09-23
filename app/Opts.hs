module Opts where


import           Control.Error
import           Options.Applicative hiding ((<$>), (<*>))


data Options
        = Options
        { inputFile  :: !FilePath
        , outputFile :: !FilePath
        , chunkSize  :: !Int
        } deriving (Show)

opts' :: Parser Options
opts' =   Options
      <$> strOption   (  short 'i' <> long "input" <> metavar "TSV_FILE"
                      <> help "The input file (the weights file from MALLET).")
      <*> strOption   (  short 'o' <> long "output" <> metavar "JSON_FILE"
                      <> help "The JSON file to write the output to.")
      <*> option auto (  short 'c' <> long "chunk-size" <> metavar "CHUNK_SIZE" <> value 0
                      <> help "The size of chunks to divide sequences into for parallel\
                         \ processing. (Default is 0, which means no parallelization.)")

opts :: ParserInfo Options
opts =   info (helper <*> opts')
              (  fullDesc
              <> progDesc "Read a MALLET term weighting file into a JSON network."
              <> header "barth-par -- parallel reading a MALLET term weighting\
                        \  file into a JSON network.")

parseOpts :: Script Options
parseOpts = scriptIO $ execParser opts
