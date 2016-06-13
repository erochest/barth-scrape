module BarthPar.Scrape.Utils where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable
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

watchF :: Show b => String -> (a -> b) -> a -> a
watchF msg f x = trace (msg ++ ": " ++ groom (f x)) x

watchMF :: (Monad m, Show b) => String -> (a -> b) -> a -> m a
watchMF msg f x = traceM (msg ++ ": " ++ groom (f x)) >> return x

watchPS :: String -> PureScript a -> PureScript a
watchPS tag r@(Right _) = trace (tag ++ " OK")        r
watchPS tag r@(Left  e) = trace (tag ++ " ERR " ++ e) r

watchS :: String -> Scrape a -> Scrape a
watchS tag =
    Scrape . mapReaderT (mapExceptT (fmap $ watchPS tag)) . unScrape

forcePS :: String -> [a] -> PureScript a
forcePS _ (x:_) = Right x
forcePS e []    = Left e

forceM :: (Foldable t, Monoid a, Eq a) => String -> t a -> PureScript a
forceM e xs = if x == mempty
                 then Left e
                 else Right x
    where
        x = fold xs

smapConcurrently :: Traversable t => (a -> Scrape b) -> t a -> Scrape (t b)
-- smapConcurrently f = scriptIO . mapConcurrently (runScript . f)
smapConcurrently = mapM

decimalPS :: Integral a => T.Text -> PureScript a
decimalPS input =
    case decimal input of
      Right (i, lo)
          | T.null lo -> Right i
          | otherwise -> Left  $ "Trailing input: \"" ++ T.unpack lo ++ "\""
      Left e          -> Left  $ "decimalPS: " ++ e

fromRomanPS :: T.Text -> PureScript Int
fromRomanPS s = note ("Unable to parse " ++ show s) $ fromRoman s

anyNumberPS :: T.Text -> PureScript Int
anyNumberPS t =   decimalPS t
              <|> fromRomanPS t
              <|> Left ("Unable to parse number \"" ++ T.unpack t ++ "\"")

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

normalize :: T.Text -> T.Text
normalize = T.unwords . T.words
