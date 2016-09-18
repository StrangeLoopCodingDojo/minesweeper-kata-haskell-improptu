module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.String.Strip

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "minesweeper test" $ do

    it "test scenario" $ do
      let board = mkBoard 2 1
      let (Board w h _) = board
      w `shouldBe` 2
      h `shouldBe` 1
      isEmpty board 1 0 `shouldBe` True
      countCorrectCells board `shouldBe` 0
      (w * h) `shouldBe` 2
      getStatus board `shouldBe` Playing

      let board' = addMine board 1 0
      isEmpty board' 1 0 `shouldBe` False
      cellAt board'  1 0 `shouldBe` Cell Mine False

      let board'2 = select board' 0 0
      cellAt board'2 0 0 `shouldBe` Cell (Empty 1) False

      isFlagged board'2 1 0 `shouldBe` False

      let board'3 = toggleFlag board'2 1 0
      isFlagged board'3 1 0 `shouldBe` True

      let board'4 = toggleFlag board'3 1 0
      isFlagged board'4 1 0 `shouldBe` False

      cellAt board'4 1 0 `shouldBe` Cell Mine False

      getStatus board'4 `shouldBe` Playing
      let board'5 = toggleFlag board'4 1 0
      getStatus board'5 `shouldBe` Victory

      --TODO: Exploded status
