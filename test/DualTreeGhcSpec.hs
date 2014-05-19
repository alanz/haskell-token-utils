module DualTreeGhcSpec (main, spec) where

import           Test.Hspec


import qualified GHC        as GHC
-- import qualified Outputable as GHC

-- import qualified GHC.SYB.Utils as SYB
import Data.Maybe

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types

-- import Data.Tree.DUAL

import TestUtils

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do

  -- ---------------------------------------------

  describe "layoutTreeToSourceTree" $ do
    it "retrieves the tokens in SourceTree format LetExpr" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/LetExpr.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      (GHC.showRichTokenStream toks) `shouldBe` "-- A simple let expression, to ensure the layout is detected\n\n module Layout.LetExpr where\n\n foo = let x = 1\n           y = 2\n       in x + y\n\n "
      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          "0:((1,1),(9,1))\n"++
          "1:((1,1),(3,7))\n"++
          "1:((3,8),(3,22))\n"++
          "1:((3,23),(3,28))\n"++
          "1:((5,1),(7,15))\n"++
          "2:((5,1),(5,4))\n"++
          "2:((5,5),(7,15))\n"++
          "3:((5,5),(5,6))\n"++
          "3:((5,7),(7,15))\n"++
          "4:((5,7),(5,10))\n"++
          "4:((5,11),(6,16))(Above None (5,11) (6,16) FromAlignCol (1,-9))\n"++
          "5:((5,11),(5,16))\n"++
          "6:((5,11),(5,12))\n"++
          "6:((5,13),(5,16))\n"++
          "7:((5,13),(5,14))\n"++
          "7:((5,15),(5,16))\n"++
          "5:((6,11),(6,16))\n"++
          "6:((6,11),(6,12))\n"++
          "6:((6,13),(6,16))\n"++
          "7:((6,13),(6,14))\n"++
          "7:((6,15),(6,16))\n"++
          "4:((7,10),(7,15))\n"++
          "5:((7,10),(7,11))\n"++
          "5:((7,12),(7,13))\n"++
          "5:((7,14),(7,15))\n"++
          "1:((9,1),(9,1))\n"
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""
      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

