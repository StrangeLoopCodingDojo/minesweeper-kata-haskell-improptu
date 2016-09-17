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

      let board' = addMine board 1 0
      isEmpty board' 1 0 `shouldBe` False

      let board'2 = select board' 0 0
      checkValue board'2 0 0 `shouldBe` Empty 1

      -- flag 0,1
      let board'3 = flag board'2 0 1
      isFlagged board'3 0 1 `shouldBe` True
