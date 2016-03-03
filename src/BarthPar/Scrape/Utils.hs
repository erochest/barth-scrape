module BarthPar.Scrape.Utils where


import           Control.Error
import           Control.Lens
import           Control.Monad         (when)
import qualified Data.Text             as T
import           Data.Text.Read
import           Debug.Trace
import           Text.Groom
import           Text.Numeral.Roman
-- import           Control.Concurrent.Async

import           BarthPar.Scrape.Types


tshow :: Show a => a -> T.Text
tshow = T.pack . show

watch :: Show a => String -> a -> a
watch msg x = trace (msg ++ ": " ++ groom x) x

watchM :: (Monad m, Show a) => String -> a -> m a
watchM msg x = traceM (msg ++ ": " ++ groom x) >> return x

forcePS :: String -> [a] -> PureScript a
forcePS _ (x:_) = Right x
forcePS e []    = Left  e

smapConcurrently :: Traversable t => (a -> Scrape b) -> t a -> Scrape (t b)
-- smapConcurrently f = scriptIO . mapConcurrently (runScript . f)
smapConcurrently = mapM

decimalPS :: Integral a => T.Text -> PureScript a
decimalPS input =
    case decimal input of
      Right (i, lo)
          | T.null lo -> Right i
          | otherwise -> Left  $ "Trailing input: \"" ++ T.unpack lo ++ "\""
      Left e          -> Left e

fromRomanPS :: T.Text -> PureScript Int
fromRomanPS s = note ("Unable to parse " ++ show s) $ fromRoman s

debugging :: Scrape () -> Scrape ()
debugging s = do
  debug <- view scrapeDebugging
  when debug s

debugging' :: a -> Scrape a -> Scrape a
debugging' a s = do
  debug <- view scrapeDebugging
  if debug
  then s
  else return a
