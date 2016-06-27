{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module BarthPar.Scrape.Types.Scrape where


import           Control.DeepSeq
import           Control.Error
import           Control.Lens
import           Control.Monad.Reader
import           Data.Data
import           GHC.Generics                    hiding (to)

import           BarthPar.Scrape.Types.Internals


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
