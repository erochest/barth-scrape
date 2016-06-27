module BarthPar.Scrape.UtilsSpec where


import qualified Data.List as L
import Test.Hspec
import Test.QuickCheck

import BarthPar.Scrape.Utils


data WindowInputs = WI Int Int String
                  deriving (Eq, Show)

instance Arbitrary WindowInputs where
    arbitrary = do
        xs <- suchThat arbitrary (not . L.null)
        w  <- choose (0, length xs)
        o  <- choose (0, w)
        return $ WI w o xs

spec :: Spec
spec =
    describe "window" $ do
        it "should move forward over the input." $
            window 1 1 "abcdef" `shouldBe` ["a", "b", "c", "d", "e", "f"]

        it "should skip over items." $
            window 2 2 "abcdef" `shouldBe` ["ab", "cd", "ef"]

        it "should include items more than once." $
            window 4 2 "abcdefgh" `shouldBe` ["abcd", "cdef", "efgh", "gh"]

        -- TODO: This one doesn't seem to ever finish. I'm not sure if that's
        -- because of the Arbitrary instance above or the test itself.
        -- it "should have all windows except the last be the right size." $
        --     property $ \(WI w o xs) ->
        --         let ws = window w o xs
        --         in  all ((== w) . length) (init ws) && length (last ws) <= w
