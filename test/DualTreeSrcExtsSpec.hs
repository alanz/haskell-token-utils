module DualTreeSrcExtsSpec (main, spec) where

import           Test.Hspec


import Data.Maybe

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Pretty
import Language.Haskell.TokenUtils.Utils

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

      -- (show modu) `shouldBe` ""

      let layout = allocTokens modu toks
      -- (show layout) `shouldBe` ""
      (drawTreeWithToks layout) `shouldBe` ""
      -- (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""
      (showPpr srcTree) `shouldBe` ""
      (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format LetStmt" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/LetStmt.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      (showTokenStream toks) `shouldBe` "-- A simple let statement, to ensure the layout is detected\n\nmodule Layout.LetStmt where\n\nfoo = do\n        let x = 1\n            y = 2\n        x+y\n"

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --    ""

      (renderSourceTree srcTree) `shouldBe`
          "-- A simple let statement, to ensure the layout is detected\n\nmodule Layout.LetStmt where\n\nfoo = do\n        let x = 1\n            y = 2\n        x+y\n"

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format LayoutIn2" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Renaming/LayoutIn2.hs"

      (showTokenStream toks) `shouldBe` "module LayoutIn2 where\n\n--Layout rule applies after 'where','let','do' and 'of'\n\n--In this Example: rename 'list' to 'ls'.\n\nsilly :: [Int] -> Int\nsilly list = case list of  (1:xs) -> 1\n--There is a comment\n                           (2:xs)\n                             | x < 10    -> 4  where  x = last xs\n                           otherwise -> 12\n"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format LetIn1" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/LiftToToplevel/LetIn1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Where" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Where.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format PatBind" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/PatBind.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format TokenTest" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/TokenTest.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
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
      ParseOk (modu,toks) <- loadFile "./test/testdata/MoveDef/Md1.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Layout.LetIn1" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/TypeUtils/LayoutLet1.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format Layout.Comments1" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Comments1.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format LocToName" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/LocToName.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------------------

    it "retrieves the tokens in SourceTree format DupDef.Dd1" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/DupDef/Dd1.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      -- (show layout) `shouldBe` ""

      -- (show layout) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree layout

      -- (show srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- --------------------------------------

    it "retrieves the tokens in SourceTree format Renaming.LayoutIn4" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Renaming/LayoutIn4.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
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
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/Lift.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format MoveDef.Demote with deletion/insertion 2" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/MoveDef/Demote.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Layout.FromMd1 with deletion 2" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/FromMd1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
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
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/FromMd1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
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
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/Where2.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
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
      ParseOk (modu,toks) <-  loadFile "./test/testdata/TypeUtils/LayoutLet2.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format Renaming.LayoutIn1" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Renaming/LayoutIn1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Renaming.LayoutIn1" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Renaming/LayoutIn1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after adding a local decl Layout.Lift" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/Lift.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after demoting Demote.D2" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Demote/D2.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after add params AddParams1" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/AddParams1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Renaming.D5" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Renaming/D5.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Layout.D5Simple" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/D5Simple.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming TypeUtils.LayoutLet2" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/TypeUtils/LayoutLet2.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Renaming.LayoutIn3" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Renaming/LayoutIn3.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format TypeUtils.Empty" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/TypeUtils/Empty.hs"

      (showTokenStream toks) `shouldBe` "module Empty where\n"
      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""
      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- -----------------------------------------------------------------

    it "retrieves the tokens in SourceTree format after renaming Layout.Do1" $ do
      ParseOk (modu,toks) <-  loadFile "./test/testdata/Layout/Do1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (show srcTree) `shouldBe`
      --     ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Move1" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Move1.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format HsDo" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/HsDo.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format forall" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/ForAll.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format DerivD" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Derive.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Class" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Class.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format default decl" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Default.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format foreign decl" $ do
      pendingWith "ghc-mod 4.x cannot load this file"
{-
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Foreign.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      (invariant layout) `shouldBe` []


      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource
-}

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format template haskell" $ do
      pendingWith "must work out how to load TemplateHaskell in HSE"
{-
      -- rr <- loadFileWithMode templateHaskellMode "./test/testdata/Layout/TH.hs"
      -- (show rr) `shouldBe` ""
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/TH.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource
-}
    -- ---------------------------------

    it "retrieves the tokens in SourceTree format PArr" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/PArr.hs"

      -- (show toks) `shouldBe` ""

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Arrow" $ do
      pendingWith "must see if HSE supports arrow notation"
{-
      ParseOk (modu,toks) <- loadFile "./test/testdata/Layout/Arrow.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource
-}

    -- ---------------------------------

    it "retrieves the tokens in SourceTree format TemplateHaskell" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/TH/Main.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Utils.hs" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Renaming/Utils.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource


    -- ---------------------------------

    it "retrieves the tokens in SourceTree format Utils.hs with renaming" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Renaming/Utils.hs"

      let origSource = (showTokenStream toks)

      let layout = allocTokens modu toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      -- (invariant layout) `shouldBe` []

      let srcTree = layoutTreeToSourceTree layout
      -- (showGhc srcTree) `shouldBe` ""

      -- (show $ retrieveLines srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource





