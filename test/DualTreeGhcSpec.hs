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
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""
      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

