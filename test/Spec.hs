module Main where

import           Control.Monad (guard, when)
import           Data.Bool (bool)
import           Data.Foldable (for_)
import           Data.Tuple (swap)
import qualified Data.Vector as V
import           Lib hiding (main)
import           Linear.V2
import           Test.Hspec


makePattern :: [a] -> Pattern a
makePattern = Pattern . V.fromList

main :: IO ()
main = hspec $ do
  let boring  = makePattern $ replicate 9 0
      middle  = makePattern [0, 0, 0, 0, 1, 0, 0, 0, 0]
      checker = makePattern [0, 1, 0, 1, 0, 1, 0, 1, 0]

  describe "your mum" $ do
    it "should always be boring" $ do
      for_ [-2..2] $ \y ->
        for_ [-2..2] $ \x ->
          agrees 3 (V2 x y) boring boring `shouldBe` True

    it "should border" $ do
      for_ [-2..2] $ \y ->
        for_ [-2..2] $ \x -> do
          agrees 3 (V2 x y) boring middle `shouldBe` (abs y == 2 || abs x == 2)
          agrees 3 (V2 x y) middle boring `shouldBe` (abs y == 2 || abs x == 2)

    it "should checker" $ do
      for_ [-2..2] $ \y ->
        for_ [-2..2] $ \x ->
          agrees 3 (V2 x y) checker checker
            `shouldBe` bool False True ((x + y) `mod` 2 == 0)

  describe "tests" $ do
    it "should work" $ do
      frequency "hello" `shouldBe` [(1, 'e'), (1, 'h'), (2, 'l'), (1, 'o')]

