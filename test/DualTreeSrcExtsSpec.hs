module DualTreeSrcExtsSpec (main, spec) where

import           Test.Hspec


import Data.Maybe

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty

import SrcExtsUtils
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
      -- (t,toks) <- parsedFileGhc "./test/testdata/Layout/LetExpr.hs"
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/LetExpr.hs"

      (showTokenStream toks) `shouldBe` "-- A simple let expression, to ensure the layout is detected\n\nmodule Layout.LetExpr where\n\nfoo = let x = 1\n          y = 2\n      in x + y\n"
      let origSource = (showTokenStream toks)

      (show modu) `shouldBe` ""

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""
      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------
{-
    it "retrieves the tokens in SourceTree format LetStmt" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/LetStmt.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      (GHC.showRichTokenStream $ bypassGHCBug7351 toks) `shouldBe` "-- A simple let statement, to ensure the layout is detected\n\nmodule Layout.LetStmt where\n\nfoo = do\n        let x = 1\n            y = 2\n        x+y\n\n"

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []
{-
      (drawTreeCompact layout) `shouldBe`
          "0:((1,1),(10,1))\n"++
          "1:((1,1),(3,7))\n"++
          "1:((3,8),(3,22))\n"++
          "1:((3,23),(3,28))\n"++
          "1:((5,1),(8,12))\n"++
          "2:((5,1),(5,4))\n"++
          "2:((5,5),(8,12))\n"++
          "3:((5,5),(5,6))\n"++
          "3:((5,7),(8,12))\n"++
          "4:((5,7),(5,9))\n"++
          "4:((6,9),(8,12))(Above FromAlignCol (1,-1) (6,9) (8,12) FromAlignCol (2,-11))\n"++
          "5:((6,9),(7,18))\n"++
          "6:((6,9),(6,12))\n"++
          "6:((6,13),(7,18))(Above None (6,13) (7,18) FromAlignCol (1,-9))\n"++
          "7:((6,13),(6,18))\n"++
          "8:((6,13),(6,14))\n"++
          "8:((6,15),(6,18))\n"++
          "9:((6,15),(6,16))\n"++
          "9:((6,17),(6,18))\n"++
          "7:((7,13),(7,18))\n"++
          "8:((7,13),(7,14))\n"++
          "8:((7,15),(7,18))\n"++
          "9:((7,15),(7,16))\n"++
          "9:((7,17),(7,18))\n"++
          "5:((8,9),(8,12))\n"++
          "6:((8,9),(8,10))\n"++
          "6:((8,10),(8,11))\n"++
          "6:((8,11),(8,12))\n"++
          "1:((10,1),(10,1))\n"
-}

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --    ""

      (renderSourceTree srcTree) `shouldBe`
          "-- A simple let statement, to ensure the layout is detected\n\nmodule Layout.LetStmt where\n\nfoo = do\n        let x = 1\n            y = 2\n        x+y\n\n"

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format LayoutIn2" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Renaming/LayoutIn2.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      (GHC.showRichTokenStream $ bypassGHCBug7351 toks) `shouldBe` "module LayoutIn2 where\n\n--Layout rule applies after 'where','let','do' and 'of'\n\n--In this Example: rename 'list' to 'ls'.\n\nsilly :: [Int] -> Int\nsilly list = case list of  (1:xs) -> 1\n--There is a comment\n                           (2:xs)\n                             | x < 10    -> 4  where  x = last xs\n                           otherwise -> 12\n\n"

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format LetIn1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/LetIn1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Where" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Where.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format PatBind" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/PatBind.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

{-
      (drawTreeCompact layout) `shouldBe`
         "0:((1,1),(13,1))\n"++
         "1:((1,1),(1,7))\n"++
         "1:((1,8),(1,22))\n"++
         "1:((1,23),(1,28))\n"++
         "1:((4,1),(4,18))\n"++
         "2:((4,1),(4,4))\n"++
         "2:((4,5),(4,7))\n"++
         "2:((4,8),(4,9))\n"++
         "2:((4,9),(4,12))\n"++
         "2:((4,12),(4,13))\n"++
         "2:((4,14),(4,17))\n"++
         "2:((4,17),(4,18))\n"++
         "1:((5,1),(5,9))\n"++
         "2:((5,1),(5,2))\n"++
         "2:((5,3),(5,5))\n"++
         "2:((5,6),(5,9))\n"++
         "1:((6,1),(6,9))\n"++
         "2:((6,1),(6,2))\n"++
         "2:((6,3),(6,5))\n"++
         "2:((6,6),(6,9))\n"++
         "1:((7,1),(10,12))\n"++
         "2:((7,1),(7,10))\n"++
         "2:((7,11),(7,12))\n"++
         "2:((7,13),(7,38))\n"++
         "3:((7,13),(7,17))\n"++
         "3:((7,18),(7,19))\n"++
         "3:((7,20),(7,38))\n"++
         "4:((7,20),(7,30))\n"++
         "5:((7,20),(7,23))\n"++
         "5:((7,24),(7,30))\n"++
         "6:((7,24),(7,25))\n"++
         "6:((7,25),(7,26))\n"++
         "6:((7,28),(7,30))\n"++
         "4:((7,32),(7,38))\n"++
         "5:((7,32),(7,33))\n"++
         "5:((7,33),(7,34))\n"++
         "5:((7,36),(7,38))\n"++
         "2:((8,3),(8,8))\n"++
         "2:((9,5),(10,12))(Above FromAlignCol (1,-4) (9,5) (10,12) FromAlignCol (3,-11))\n"++
         "3:((9,5),(9,14))\n"++
         "4:((9,5),(9,7))\n"++
         "4:((9,8),(9,10))\n"++
         "4:((9,11),(9,14))\n"++
         "3:((10,5),(10,12))\n"++
         "4:((10,5),(10,7))\n"++
         "4:((10,8),(10,12))\n"++
         "5:((10,8),(10,9))\n"++
         "5:((10,10),(10,12))\n"++
         "1:((13,1),(13,1))\n"
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format TokenTest" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""


      let srcTree = layoutTreeToSourceTree layout
{-
      srcTree `shouldBe`
          []
-}
      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Md1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/MoveDef/Md1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Layout.LetIn1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TypeUtils/LayoutLet1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Layout.Comments1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Comments1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format LocToName" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/LocToName.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""
{-
      (drawTreeCompact layout) `shouldBe`
         "0:((1,1),(25,1))\n"++
         "1:((1,1),(1,7))\n"++
         "1:((1,8),(1,17))\n"++
         "1:((1,18),(12,3))\n"++
         "1:((20,1),(24,18))\n"++
         "2:((20,1),(20,11))\n"++
         "2:((20,12),(20,41))\n"++
         "3:((20,12),(20,18))\n"++
         "3:((20,19),(20,20))\n"++
         "3:((20,21),(20,41))\n"++
         "4:((20,21),(20,25))\n"++
         "5:((20,21),(20,22))\n"++
         "5:((20,23),(20,24))\n"++
         "5:((20,24),(20,25))\n"++
         "4:((20,26),(20,27))\n"++
         "4:((20,28),(20,41))\n"++
         "5:((20,28),(20,38))\n"++
         "5:((20,39),(20,41))\n"++
         "2:((24,1),(24,18))\n"++
         "3:((24,1),(24,11))\n"++
         "3:((24,12),(24,14))\n"++
         "3:((24,15),(24,16))\n"++
         "3:((24,17),(24,18))\n"++
         "1:((25,1),(25,1))\n"
-}
      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format DupDef.Dd1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/DupDef/Dd1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe` ""

{-
      srcTree `shouldBe`
          []
-}

      (renderSourceTree srcTree) `shouldBe` origSource

    -- --------------------------------------

    it "retrieves the tokens in SourceTree format Renaming.LayoutIn4" $ do
      (t, toks) <- parsedFileGhc "./test/testdata/Renaming/LayoutIn4.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      -- (show layout) `shouldBe` ""
      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- --------------------------------------

    it "retrieves the tokens in SourceTree format Layout.Lift with deletion/insertion 1" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/Lift.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format MoveDef.Demote with deletion/insertion 2" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/MoveDef/Demote.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Layout.FromMd1 with deletion 2" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/FromMd1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (drawTreeCompact layout) `shouldBe`
      --     ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Layout.FromMd1 with deletion 3" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/FromMd1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (drawTreeCompact layout) `shouldBe`
      --     ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Layout.Where2 with deletion 4" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/Where2.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (drawTreeCompact layout) `shouldBe`
      --     ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format TypeUtils.LayoutLet2" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/TypeUtils/LayoutLet2.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format Renaming.LayoutIn1" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Renaming/LayoutIn1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Renaming.LayoutIn1" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Renaming/LayoutIn1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after adding a local decl Layout.Lift" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/Lift.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after demoting Demote.D2" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Demote/D2.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after add params AddParams1" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/AddParams1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Renaming.D5" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Renaming/D5.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Layout.D5Simple" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/D5Simple.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming TypeUtils.LayoutLet2" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/TypeUtils/LayoutLet2.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Renaming.LayoutIn3" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Renaming/LayoutIn3.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format TypeUtils.Empty" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TypeUtils/Empty.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      (GHC.showRichTokenStream toks) `shouldBe` "module Empty where\n\n "
      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""
      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Layout.Do1" $ do
      (t,toks) <-  parsedFileGhc "./test/testdata/Layout/Do1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Move1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Move1.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format HsDo" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/HsDo.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format forall" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/ForAll.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format DerivD" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Derive.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Class" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Class.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format default decl" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Default.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format foreign decl" $ do
      pendingWith "ghc-mod 4.x cannot load this file"
{-
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Foreign.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource
-}

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format template haskell" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/TH.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format PArr" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/PArr.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- (show toks) `shouldBe` ""

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Arrow" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Arrow.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format TemplateHaskell" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TH/Main.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Utils.hs" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Renaming/Utils.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Utils.hs with renaming" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Renaming/Utils.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

{-
      (drawTreeCompact layout) `shouldBe`
          ""
-}

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource




-}
