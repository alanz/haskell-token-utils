module DualTreeSrcExtsSpec (main, spec) where

import           Test.Hspec

import Control.Exception
import Data.Maybe

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Pretty
import Language.Haskell.TokenUtils.Utils
import Language.Haskell.TokenUtils.HSE.Layout

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty

import TestUtils


-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  -- ---------------------------------------------

  describe "reformatting after name change" $ do

    it "preserves layout in where clause with renaming length change" $ do
      ParseOk (modu,toks) <- loadFile "./test/testdata/Renaming/LayoutIn1.hs"

      let origSource = (showTokenStream toks)
      let layout = allocTokens modu toks

      {-
      HaRe log of this renaming

      renameTopLevelVarName start..
      renamePN:renameGroup
      renamePNworker:renameVar at :test/testdata/Renaming/LayoutIn1.hs:7:17-18
      renamePNworker:renameVar mrn,mrnq :(sq, Nothing)
      replaceToken test/testdata/Renaming/LayoutIn1.hs:7:17-18:(((False,0,0,7),17),((False,0,0,7),19))((((7,17),(7,23)),ITvarid "square"),"square")
      renamePNworker:renameVar at :test/testdata/Renaming/LayoutIn1.hs:7:24-25
      renamePNworker:renameVar mrn,mrnq :(sq, Nothing)
      replaceToken test/testdata/Renaming/LayoutIn1.hs:7:24-25:(((False,0,0,7),24),((False,0,0,7),26))((((7,24),(7,30)),ITvarid "square"),"square")
      renamePNworker:rename at :RealSrcSpan (SrcSpanOneLine {srcSpanFile = "./test/testdata/Renaming/LayoutIn1.hs", srcSpanLine = 7, srcSpanSCol = 35, srcSpanECol = 37})(((False,0,0,7),35),((False,0,0,7),37))
      replaceToken test/testdata/Renaming/LayoutIn1.hs:7:35-36:(((False,0,0,7),35),((False,0,0,7),37))((((7,35),(7,41)),ITvarid "square"),"square")
      replaceToken test/testdata/Renaming/LayoutIn1.hs:7:35-36:(((False,0,0,7),35),((False,0,0,7),37))((((7,35),(7,41)),ITvarid "square"),"square")
      renamePNWorker.renameFunBind.renameFunBind:starting matches
      renamePNWorker.renameFunBind.renameFunBind.renameFunBind:matches done
      renameTopLevelVarName done
      doRenaming done
      fetchToksFinal (not showing toks)

      -- ----------------
      -}
      -- replaceToken test/testdata/Renaming/LayoutIn1.hs:7:17-18:(((False,0,0,7),17),((False,0,0,7),19))((((7,17),(7,23)),ITvarid "square"),"square")
      let tok = (T (VarId "square"))
      let tok1 = Loc (mkHseSrcSpan (7,17) (7,23)) tok
      let ss1 = Span (7,17) (7,19)
      let f2 = replaceTokenForSrcSpan layout ss1 tok1

      -- replaceToken test/testdata/Renaming/LayoutIn1.hs:7:24-25:(((False,0,0,7),24),((False,0,0,7),26))((((7,24),(7,30)),ITvarid "square"),"square")
      let f3 = replaceTokenForSrcSpan f2 (Span (7,24) (7,26)) (Loc (mkHseSrcSpan (7,24) (7,30)) tok)

      -- replaceToken test/testdata/Renaming/LayoutIn1.hs:7:35-36:(((False,0,0,7),35),((False,0,0,7),37))((((7,35),(7,41)),ITvarid "square"),"square")
      let f4 = replaceTokenForSrcSpan f3 (Span (7,35) (7,37)) (Loc (mkHseSrcSpan (7,35) (7,41)) tok)

      -- replaceToken test/testdata/Renaming/LayoutIn1.hs:7:35-36:(((False,0,0,7),35),((False,0,0,7),37))((((7,35),(7,41)),ITvarid "square"),"square")
      let f5 = replaceTokenForSrcSpan f4 (Span (7,35) (7,37)) (Loc (mkHseSrcSpan (7,35) (7,41)) tok)

      -- (drawTreeCompact f5) `shouldBe` ""

      let srcTree = layoutTreeToSourceTree f5

      -- (showPpr srcTree) `shouldBe` ""

      (renderSourceTree srcTree) `shouldBe` origSource

