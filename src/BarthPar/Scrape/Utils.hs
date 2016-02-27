module BarthPar.Scrape.Utils where


import           Control.Error
import qualified Data.Text          as T
import           Data.Text.Read
import           Debug.Trace
import           Text.Groom
import           Text.Numeral.Roman
-- import           Control.Concurrent.Async


tshow :: Show a => a -> T.Text
tshow = T.pack . show

watch :: Show a => String -> a -> a
watch msg x = trace (msg ++ ": " ++ groom x) x

watchM :: (Monad m, Show a) => String -> a -> m a
watchM msg x = traceM (msg ++ ": " ++ groom x) >> return x

forceS :: String -> [a] -> Script a
forceS _ (x:_) = return x
forceS e []    = throwE e

smapConcurrently :: Traversable t => (a -> Script b) -> t a -> Script (t b)
-- smapConcurrently f = scriptIO . mapConcurrently (runScript . f)
smapConcurrently = mapM

decimalS :: Integral a => T.Text -> Script (a, T.Text)
decimalS = hoistEither . decimal

fromRomanS :: T.Text -> Script Int
fromRomanS s = fromRoman s ?? ("Unable to parse " ++ show s)
