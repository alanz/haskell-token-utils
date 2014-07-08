module TokenUtilsGhcSpec (main, spec) where

import           Test.Hspec

import qualified FastString as GHC
import qualified GHC        as GHC
import qualified Lexer      as GHC

import qualified GHC.SYB.Utils as SYB

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Tree

import qualified Data.Map as Map
import qualified Data.Tree.Zipper as Z

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.GHC.Layout
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Utils

import TestUtils

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do

  -- ---------------------------------------------

  describe "getTokensFor" $ do
    it "gets the tokens for a given srcloc, and caches them in the tree" $ do
      -- (t,toks) <- parsedFileTokenTestGhc
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let decl@(GHC.L l _) = head decls

      let l = ss2gs ((19,1),(21,14))
      let forest = mkTreeFromTokens toks
      let (tm',declToks) = getTokensFor True forest (gs2ss l)

      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      -- (showSrcSpan l) `shouldBe` "((19,1),(21,14))"
      -- (showGhc decl) `shouldBe` "TokenTest.foo x y\n  = do { c <- System.IO.getChar;\n         GHC.Base.return c }"
      (showToks declToks) `shouldBe` "[((18,1),(18,19),((((18,1),(18,19)),ITlineComment \"-- leading comment\"),\"-- leading comment\")),((19,1),(19,1),((((19,1),(19,1)),ITsemi),\"\")),((19,1),(19,4),((((19,1),(19,4)),ITvarid \"foo\"),\"foo\")),((19,5),(19,6),((((19,5),(19,6)),ITvarid \"x\"),\"x\")),((19,7),(19,8),((((19,7),(19,8)),ITvarid \"y\"),\"y\")),((19,9),(19,10),((((19,9),(19,10)),ITequal),\"=\")),((20,3),(20,5),((((20,3),(20,5)),ITdo),\"do\")),((20,6),(20,6),((((20,6),(20,6)),ITvocurly),\"\")),((20,6),(20,7),((((20,6),(20,7)),ITvarid \"c\"),\"c\")),((20,8),(20,10),((((20,8),(20,10)),ITlarrow),\"<-\")),((20,11),(20,18),((((20,11),(20,18)),ITvarid \"getChar\"),\"getChar\")),((21,6),(21,6),((((21,6),(21,6)),ITsemi),\"\")),((21,6),(21,12),((((21,6),(21,12)),ITvarid \"return\"),\"return\")),((21,13),(21,14),((((21,13),(21,14)),ITvarid \"c\"),\"c\"))]"

      -- Note: Although the tokens include leading and following
      -- comments, the SrcSpan must tie up with the original GHC
      -- SrcSpan in the AST
      (drawTreeEntry tm') `shouldBe`
            "((1,1),(21,14))\n|\n"++
            "+- ((1,1),(15,17))\n|\n"++
            "`- ((19,1),(21,14))\n"

    -- ---------------------------------

    it "gets the tokens for an added srcloc 1" $ do
      -- (t,toks) <- parsedFileDupDefDd1
      (t,toks) <- parsedFileGhc "./test/testdata/DupDef/Dd1.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let decl@(GHC.L l _) = head $ drop 6 decls
      let l = ss2gs ((4,1),(4,19))

      let forest = mkTreeFromTokens toks
      let (tm',declToks) = getTokensFor True forest (gs2ss l)

      -- (showGhc l) `shouldBe` "test/testdata/DupDef/Dd1.hs:4:1-18"
      -- (showSrcSpan l) `shouldBe` "((4,1),(4,19))"
      -- (showGhc decl) `shouldBe` "DupDef.Dd1.toplevel x = DupDef.Dd1.c GHC.Num.* x"
      (showToks declToks) `shouldBe` "[((4,1),(4,1),((((4,1),(4,1)),ITsemi),\"\")),((4,1),(4,9),((((4,1),(4,9)),ITvarid \"toplevel\"),\"toplevel\")),((4,10),(4,11),((((4,10),(4,11)),ITvarid \"x\"),\"x\")),((4,12),(4,13),((((4,12),(4,13)),ITequal),\"=\")),((4,14),(4,15),((((4,14),(4,15)),ITvarid \"c\"),\"c\")),((4,16),(4,17),((((4,16),(4,17)),ITstar),\"*\")),((4,18),(4,19),((((4,18),(4,19)),ITvarid \"x\"),\"x\"))]"

      -- let (tm'',newSpan,decl') = addDeclToksAfterSrcSpan tm' l (PlaceOffset 2 0 2) declToks decl
      let (tm'',newSpan) = addToksAfterSrcSpan tm' (gs2ss l) (PlaceOffset 2 0 2) declToks
      -- (showGhc newSpan) `shouldBe` "foo:1048582:1-18"
      (show newSpan) `shouldBe` "((1048582,1),(1048582,19))"

      -- (SYB.showData SYB.Renamer 0 decl') `shouldBe` "\n(L {foo:1048582:1-18} \n (FunBind \n  (L {test/testdata/DupDef/Dd1.hs:1048582:1-8} {Name: DupDef.Dd1.toplevel}) \n  (False) \n  (MatchGroup \n   [\n    (L {test/testdata/DupDef/Dd1.hs:1048582:1-18} \n     (Match \n      [\n       (L {test/testdata/DupDef/Dd1.hs:1048582:10} \n        (VarPat {Name: x}))] \n      (Nothing) \n      (GRHSs \n       [\n        (L {test/testdata/DupDef/Dd1.hs:4:14-18} \n         (GRHS \n          [] \n          (L {test/testdata/DupDef/Dd1.hs:1048582:14-18} \n           (OpApp \n            (L {test/testdata/DupDef/Dd1.hs:1048582:14} \n             (HsVar {Name: DupDef.Dd1.c})) \n            (L {test/testdata/DupDef/Dd1.hs:1048582:16} \n             (HsVar {Name: GHC.Num.*})) {Fixity: infixl 7} \n            (L {test/testdata/DupDef/Dd1.hs:1048582:18} \n             (HsVar {Name: x}))))))] \n       (EmptyLocalBinds))))] {!type placeholder here?!}) \n  (WpHole) {NameSet: \n  [{Name: DupDef.Dd1.c}]} \n  (Nothing)))"

      (drawTreeEntry tm'') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((1000006,1),(1000006,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

    -- ---------------------------------

    it "gets the tokens for an added srcloc with one line spacing" $ do
      -- (t,toks) <- parsedFileDupDefDd1
      (t,toks) <- parsedFileGhc "./test/testdata/DupDef/Dd1.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      let forest = mkTreeFromTokens toks

      -- let decl@(GHC.L l _) = head $ drop 6 decls
      let l = ss2gs ((4,1),(4,19))
      let (tm',declToks) = getTokensFor True forest (gs2ss l)
      (drawTreeEntry tm') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- (showGhc l) `shouldBe` "test/testdata/DupDef/Dd1.hs:4:1-18"
      -- (showSrcSpan l) `shouldBe` "((4,1),(4,19))"
      -- (showGhc decl) `shouldBe` "DupDef.Dd1.toplevel x = DupDef.Dd1.c GHC.Num.* x"
      (showToks declToks) `shouldBe` "[((4,1),(4,1),((((4,1),(4,1)),ITsemi),\"\")),((4,1),(4,9),((((4,1),(4,9)),ITvarid \"toplevel\"),\"toplevel\")),((4,10),(4,11),((((4,10),(4,11)),ITvarid \"x\"),\"x\")),((4,12),(4,13),((((4,12),(4,13)),ITequal),\"=\")),((4,14),(4,15),((((4,14),(4,15)),ITvarid \"c\"),\"c\")),((4,16),(4,17),((((4,16),(4,17)),ITstar),\"*\")),((4,18),(4,19),((((4,18),(4,19)),ITvarid \"x\"),\"x\"))]"

      -- let Just (GHC.L _ n) = locToName (4, 2) renamed
      -- let typeSig = head $ definingSigsNames [n] renamed
      -- let (GHC.L ln _) = typeSig
      let ln = ss2gs ((3,1),(3,31))
      -- (showSrcSpan ln) `shouldBe` "((3,1),(3,31))"
      let (tm'',sigToks) = getTokensFor True tm' (gs2ss ln)
      (drawTreeEntry tm'') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- let (tm''',newSpan,typeSig') = addDeclToksAfterSrcSpan tm'' l (PlaceOffset 2 0 0) sigToks typeSig
      let (tm''',newSpan) = addToksAfterSrcSpan tm'' (gs2ss l) (PlaceOffset 2 0 0) sigToks 
      -- (showGhc newSpan) `shouldBe` "foo:1048582:1-30"
      (show newSpan) `shouldBe` "((1048582,1),(1048582,31))"

      -- (SYB.showData SYB.Renamer 0 typeSig') `shouldBe` "\n(L {foo:1048582:1-30} \n (TypeSig \n  [\n   (L {test/testdata/DupDef/Dd1.hs:1048582:1-8} {Name: DupDef.Dd1.toplevel})] \n  (L {test/testdata/DupDef/Dd1.hs:1048582:13-30} \n   (HsFunTy \n    (L {test/testdata/DupDef/Dd1.hs:1048582:13-19} \n     (HsTyVar {Name: GHC.Integer.Type.Integer})) \n    (L {test/testdata/DupDef/Dd1.hs:1048582:24-30} \n     (HsTyVar {Name: GHC.Integer.Type.Integer}))))))"

      (drawTreeEntry tm''') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((1000006,1),(1000006,31))\n|\n"++
            "`- ((6,1),(32,18))\n"
      -- -- -- -- --
      {-
      let (ff,tt) = getSrcSpanFor tm''' (fs newSpan)
          z = openZipperToSpan (fs newSpan) $ Z.fromTree ff
          prevToks = retrievePrevLineToks z

          (_,(ForestLine _ endRow,_))       = ghcSrcSpanToForestSpan newSpan
          prevToks' = reverse $ dropWhile (\t -> tokenRow t > endRow) $ reverse  prevToks

      (GHC.showRichTokenStream prevToks') `shouldBe` ""
      (show prevToks') `shouldBe` ""
      -}
      -- --- -- --
      -- let (tm'''',_newSpan',_decl') = addDeclToksAfterSrcSpan tm''' newSpan (PlaceOffset 1 0 2) declToks decl
      let (tm'''',_newSpan') = addToksAfterSrcSpan tm''' newSpan (PlaceOffset 1 0 2) declToks
      -- (showGhc newSpan') `shouldBe` "f:1000006:1-30"

      (drawTreeEntry tm'''') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((1000006,1),(1000006,31))\n|\n"++
            "+- ((1000007,1),(1000007,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

    -- ---------------------------------

    it "gets the tokens for an added indented srcloc" $ do
      -- (t,toks) <- parsedFileDupDefDd1
      (t,toks) <- parsedFileGhc "./test/testdata/DupDef/Dd1.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      let forest = mkTreeFromTokens toks

      -- let Just (GHC.L _ n) = locToName (23, 5) renamed
      -- (showGhc n) `shouldBe` "zz"

      -- let sspan = posToSrcSpan forest ((23,5),(23,11))
      let sspan = ss2gs ((23,5),(23,11))

      -- (showGhc sspan) `shouldBe` "f:23:5-10"
      -- (show sspan) `shouldBe` "f:23:5-10"

      let (tm1,declToks) = getTokensFor True forest (gs2ss sspan)
      (GHC.showRichTokenStream declToks) `shouldBe` "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n     zz = 1"
      (drawTreeEntry tm1) `shouldBe`
           "((1,1),(32,18))\n|\n"++
           "+- ((1,1),(22,8))\n|\n"++
           "+- ((23,5),(23,11))\n|\n"++
           "`- ((25,1),(32,18))\n"

      let (tm2,newSpan) = addToksAfterSrcSpan tm1 (gs2ss sspan) (PlaceIndent 1 0 1) declToks
      -- (showGhc $ ss2gs newSpan) `shouldBe` "foo:1048600:5-10"
      (show newSpan) `shouldBe` "((1048600,5),(1048600,11))"
      (drawTreeEntry tm2) `shouldBe`
           "((1,1),(32,18))\n|\n"++
           "+- ((1,1),(22,8))\n|\n"++
           "+- ((23,5),(23,11))\n|\n"++
           "+- ((1000024,5),(1000024,11))\n|\n"++
           "`- ((25,1),(32,18))\n"

    -- ---------------------------------

    it "gets the tokens after adding and renaming" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/DupDef/Dd1.hs"
      -- (t,toks) <- parsedFileDupDefDd1
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      let forest = mkTreeFromTokens toks

      -- let decl@(GHC.L l _) = head $ drop 6 decls
      let l = ss2gs ((4,1),(4,19))
      (showSrcSpanF l) `shouldBe` "(((False,0,0,4),1),((False,0,0,4),19))"
      let (tm',declToks) = getTokensFor True forest (gs2ss l)
      (drawTreeEntry tm') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- (showGhc l) `shouldBe` "test/testdata/DupDef/Dd1.hs:4:1-18"
      -- (showSrcSpan l) `shouldBe` "((4,1),(4,19))"
      -- (showGhc decl) `shouldBe` "DupDef.Dd1.toplevel x = DupDef.Dd1.c GHC.Num.* x"
      (showToks declToks) `shouldBe` "[((4,1),(4,1),((((4,1),(4,1)),ITsemi),\"\")),((4,1),(4,9),((((4,1),(4,9)),ITvarid \"toplevel\"),\"toplevel\")),((4,10),(4,11),((((4,10),(4,11)),ITvarid \"x\"),\"x\")),((4,12),(4,13),((((4,12),(4,13)),ITequal),\"=\")),((4,14),(4,15),((((4,14),(4,15)),ITvarid \"c\"),\"c\")),((4,16),(4,17),((((4,16),(4,17)),ITstar),\"*\")),((4,18),(4,19),((((4,18),(4,19)),ITvarid \"x\"),\"x\"))]"

      -- let Just (GHC.L _ n) = locToName (4, 2) renamed
      -- let typeSig = head $ definingSigsNames [n] renamed
      -- let (GHC.L ln _) = typeSig
      let ln = ss2gs ((3,1),(3,31))
      (showSrcSpan ln) `shouldBe` "((3,1),(3,31))"
      let (tm'',sigToks) = getTokensFor True tm' (gs2ss ln)
      (drawTreeEntry tm'') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

      let (tm''',newSpan) = addToksAfterSrcSpan tm'' (gs2ss l) (PlaceOffset 2 0 0) sigToks
      -- (showSrcSpanF newSpan) `shouldBe` "f:1048582:1-30"
      (show $ ss2f newSpan) `shouldBe` "(((ForestLine False 0 1 6),1),((ForestLine False 0 1 6),31))"

      -- (SYB.showData SYB.Renamer 0 typeSig') `shouldBe` "\n(L {foo:1048582:1-30} \n (TypeSig \n  [\n   (L {test/testdata/DupDef/Dd1.hs:1048582:1-8} {Name: DupDef.Dd1.toplevel})] \n  (L {test/testdata/DupDef/Dd1.hs:1048582:13-30} \n   (HsFunTy \n    (L {test/testdata/DupDef/Dd1.hs:1048582:13-19} \n     (HsTyVar {Name: GHC.Integer.Type.Integer})) \n    (L {test/testdata/DupDef/Dd1.hs:1048582:24-30} \n     (HsTyVar {Name: GHC.Integer.Type.Integer}))))))"

      (drawTreeEntry tm''') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((1000006,1),(1000006,31))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- getToksForSpan test/testdata/DupDef/Dd1.hs:1048582:1-30:("(((False,0,1,6),1),((False,0,1,6),31))"
      let sspan3 = f2gs $
                        (((ForestLine False 0 1 6),1),
                         ((ForestLine False 0 1 6),31) )
      let (tm4,toks4) = getTokensFor True tm''' (gs2ss sspan3)
      (drawTreeEntry tm4) `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((1000006,1),(1000006,31))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- putToksForSpan test/testdata/DupDef/Dd1.hs:1048582:1-30:(((False,0,1,6),1),((False,0,1,6),31))
      -- NOTE: shortcut, using same toks, it is the book-keeping we
      -- are testing
      let (tm5,_sspan5,_tree5) = updateTokensForSrcSpan tm4 (gs2ss sspan3) toks4
      (drawTreeEntry tm5) `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((10001000006,1),(10001000006,31))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- putDeclToksAfterSpan test/testdata/DupDef/Dd1.hs:1048582:1-30:("(((False,0,1,6),1),((False,0,1,6),31))",PlaceIndent 1 0 2

      -- --
      let newSpan111 = ss2gs ((7,1),(7,19))
      (showSrcSpanF newSpan111) `shouldBe` "(((False,0,0,7),1),((False,0,0,7),19))"
      -- let (forest',newSpan') = addNewSrcSpanAndToksAfter tm5 sspan3 newSpan (PlaceIndent 1 0 2) declToks

      let z = openZipperToSpan (gs2f sspan3) $ Z.fromTree tm5
      (show $ treeStartEnd (Z.tree z))    `shouldBe` "(((ForestLine True 0 1 6),1),((ForestLine True 0 1 6),31))"
      (show (gs2f sspan3)) `shouldBe` "(((ForestLine False 0 1 6),1),((ForestLine False 0 1 6),31))"
      (show $ treeStartEnd (Z.tree z) == (gs2f sspan3)) `shouldBe` "True"

      let f1 = insertSrcSpan tm5 (gs2f sspan3)
      (drawTreeEntry f1) `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((10001000006,1),(10001000006,31))\n|\n"++
            "`- ((6,1),(32,18))\n"

      let (forest',_tree) = getSrcSpanFor tm5 (gs2f sspan3)
      (drawTreeEntry forest') `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((10001000006,1),(10001000006,31))\n|\n"++
            "`- ((6,1),(32,18))\n"
      -- --

      let (tm6,_sspan6) = addToksAfterSrcSpan tm5 (gs2ss sspan3) (PlaceIndent 1 0 2) declToks
      (drawTreeEntry tm6) `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((10001000006,1),(10001000006,31))\n|\n"++
            "+- ((1000007,1),(1000007,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

      -- Context set up at last, actual test:
      -- getToksForSpan test/testdata/DupDef/Dd1.hs:1048583:1-18:("(((False,0,1,7),1),((False,0,1,7),19))"
      let sspan4 = f2gs $
                        (((ForestLine False 0 1 7),1),
                         ((ForestLine False 0 1 7),19) )
      let (tm7,toks7) = getTokensFor True tm6 (gs2ss sspan4)
      (drawTreeEntry tm7) `shouldBe`
            "((1,1),(32,18))\n|\n"++
            "+- ((1,1),(3,31))\n|  |\n"++
            "|  +- ((1,1),(1,24))\n|  |\n"++
            "|  `- ((3,1),(3,31))\n|\n"++
            "+- ((4,1),(4,19))\n|\n"++
            "+- ((10001000006,1),(10001000006,31))\n|\n"++
            "+- ((1000007,1),(1000007,19))\n|\n"++
            "`- ((6,1),(32,18))\n"

      (show toks7) `shouldBe` "[((((7,1),(7,1)),ITsemi),\"\"),((((7,1),(7,9)),ITvarid \"toplevel\"),\"toplevel\"),((((7,10),(7,11)),ITvarid \"x\"),\"x\"),((((7,12),(7,13)),ITequal),\"=\"),((((7,14),(7,15)),ITvarid \"c\"),\"c\"),((((7,16),(7,17)),ITstar),\"*\"),((((7,18),(7,19)),ITvarid \"x\"),\"x\"),((((8,1),(8,1)),ITsemi),\"\")]"

    -- ---------------------------------

    it "gets the tokens after renaming" $ do
      -- (_t,toks) <- parsedFileLiftD1Ghc
      (t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/D1.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      let forest = mkTreeFromTokens toks

      -- putToksForSpan test/testdata/LiftToToplevel/D1.hs:6:24:(((False,0,0,6),24),((False,0,0,6),25))
      -- [((((0,1),(0,3)),ITvarid "sq"),"sq"),((((0,4),(0,5)),IToparen),"("),((((0,5),(0,6)),ITvarid "x"),"x"),((((0,7),(0,10)),ITvarid "pow"),"pow"),((((0,10),(0,11)),ITcparen),")")]

      let sspan1 = f2gs $
                        (((ForestLine False 0 0 6),24),
                         ((ForestLine False 0 0 6),25) )
      newToks <- liftIO $ basicTokenise "sq (x pow)"
      (show newToks) `shouldBe` "[((((0,1),(0,3)),ITvarid \"sq\"),\"sq\"),((((0,4),(0,5)),IToparen),\"(\"),((((0,5),(0,6)),ITvarid \"x\"),\"x\"),((((0,7),(0,10)),ITvarid \"pow\"),\"pow\"),((((0,10),(0,11)),ITcparen),\")\")]"

      let (tm2,_sspan2,_tree2) = updateTokensForSrcSpan forest (gs2ss sspan1) newToks
      (drawTreeEntry tm2) `shouldBe`
            "((1,1),(13,25))\n|\n"++
            "+- ((1,1),(6,23))\n|\n"++
            "+- ((10000000006,24),(10000000006,34))\n|\n"++
            "`- ((6,26),(13,25))\n"

      -- putToksAfterPos ((8,6),(8,8)) at PlaceAdjacent:[((((0,2),(0,5)),ITvarid "pow"),"pow")]
      let sspan2 = f2gs $
                        (((ForestLine False 0 0 8),6),
                         ((ForestLine False 0 0 8),8) )
      toks3 <- liftIO $ basicTokenise " pow"
      (show toks3) `shouldBe` "[((((0,2),(0,5)),ITvarid \"pow\"),\"pow\")]"
      let toks3' = map markToken toks3
      (show toks3') `shouldBe` "[((((0,2),(0,5)),ITvarid \"pow\"),\"pow\")]"
      let (tm3,newSpan3) = addToksAfterSrcSpan tm2 (gs2ss sspan2) PlaceAdjacent toks3'
      (showSrcSpanF $ ss2gs newSpan3) `shouldBe` "(((False,0,1,8),9),((False,0,1,8),12))"
      -- (show tm3) `shouldBe` ""
      (drawTreeEntry tm3) `shouldBe`
            "((1,1),(13,25))\n|\n"++
            "+- ((1,1),(6,23))\n|\n"++
            "+- ((10000000006,24),(10000000006,34))\n|\n"++
            "`- ((6,26),(13,25))\n   |\n"++
            "   +- ((6,26),(7,8))\n   |\n"++
            "   +- ((8,6),(8,8))\n   |\n"++
            "   +- ((1000008,9),(1000008,12))\n   |\n"++
            "   `- ((8,9),(13,25))\n"
      (invariant tm3) `shouldBe` []

      -- The test ....
      -- getToksForSpan test/testdata/LiftToToplevel/D1.hs:8:6-19:("(((False,0,0,8),6),((False,0,0,8),20))",
      let sspan3 = f2gs $
                        (((ForestLine False 0 0 8),6),
                         ((ForestLine False 0 0 8),20) )

      let (tm4,toks4) = getTokensFor True tm3 (gs2ss sspan3)
      (drawTreeEntry tm4) `shouldBe`
            "((1,1),(13,25))\n|\n"++
            "+- ((1,1),(6,23))\n|\n"++
            "+- ((10000000006,24),(10000000006,34))\n|\n"++
            "`- ((6,26),(13,25))\n   |\n"++
            "   +- ((6,26),(7,8))\n   |\n"++
            "   +- ((8,6),(8,20))\n   |  |\n"++
            "   |  +- ((8,6),(8,8))\n   |  |\n"++
            "   |  +- ((1000008,9),(1000008,12))\n   |  |\n"++
            "   |  `- ((8,9),(8,20))\n   |\n"++
            "   `- ((9,6),(13,25))\n"
{-
            "((1,1),(13,25))\n|\n"++
            "+- ((1,1),(6,23))\n|\n"++
            "+- ((10000000006,24),(10000000006,34))\n|\n"++
            "`- ((6,26),(13,25))\n   |\n"++
            "   +- ((6,26),(7,8))\n   |\n"++
            "   +- ((8,6),(8,20))\n   |\n"++
            "   `- ((9,6),(13,25))\n"
-}

{-
tree TId 0:
((1,1),(11,18))
|
+- ((1,1),(6,23))
|
+- ((10000000006,24),(10000000006,34))
|
`- ((6,26),(11,18))
   |
   +- ((6,26),(7,8))
   |
   +- ((8,6),(8,20))
   |
   `- ((8,21),(11,18))
-}


      (showToks toks4) `shouldBe` "[((8,6),(8,6),((((8,6),(8,6)),ITvocurly),\"\")),((8,6),(8,8),((((8,6),(8,8)),ITvarid \"sq\"),\"sq\")),((8,9),(8,12),((((8,9),(8,12)),ITvarid \"pow\"),\"pow\")),((8,9),(8,10),((((8,9),(8,10)),ITvarid \"x\"),\"x\")),((8,11),(8,12),((((8,11),(8,12)),ITequal),\"=\")),((8,13),(8,14),((((8,13),(8,14)),ITvarid \"x\"),\"x\")),((8,15),(8,16),((((8,15),(8,16)),ITvarsym \"^\"),\"^\")),((8,17),(8,20),((((8,17),(8,20)),ITvarid \"pow\"),\"pow\"))]"

    -- ---------------------------------

    it "gets the tokens after updating a SrcSpan" $ do
      -- (_t,toks) <- parsedFileLiftLetIn1Ghc
      (t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/LetIn1.hs"
      -- (show toks) `shouldBe` ""
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let _f1 = allocTokens parsed toks
      -- (show _f1) `shouldBe` ""

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      let forest = mkTreeFromTokens toks

      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:12:22-23:(((False,0,0,12),22),((False,0,0,12),24))
      -- [((((0,1),(0,2)),IToparen),"("),((((0,2),(0,4)),ITvarid "sq"),"sq"),((((0,5),(0,8)),ITvarid "pow"),"pow"),((((0,8),(0,9)),ITcparen),")")]

      let sspan1 =
                        (((forestLineToGhcLine $ ForestLine False 0 0 12),22),
                         ((forestLineToGhcLine $ ForestLine False 0 0 12),24) )
--
      let ff = insertSrcSpan forest (ss2f sspan1)
      (drawTreeEntry ff) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|\n"++
            "+- ((12,22),(12,24))\n|\n"++
            "`- ((12,25),(16,22))\n"
      -- (show ff) `shouldBe` ""
--
      newToks <- liftIO $ basicTokenise "(sq pow)"
      (show newToks) `shouldBe` "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,4)),ITvarid \"sq\"),\"sq\"),((((0,5),(0,8)),ITvarid \"pow\"),\"pow\"),((((0,8),(0,9)),ITcparen),\")\")]"

      let (tm2,_sspan2,_tree2) = updateTokensForSrcSpan forest (sspan1) newToks
      -- (show tm2) `shouldBe` ""
      (drawTreeEntry tm2) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n"

      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:12:29-30:(((False,0,0,12),29),((False,0,0,12),31))
      -- [((((0,1),(0,2)),IToparen),"("),((((0,2),(0,4)),ITvarid "sq"),"sq"),((((0,5),(0,8)),ITvarid "pow"),"pow"),((((0,8),(0,9)),ITcparen),")")]

      let sspan2 =
                        (((forestLineToGhcLine $ ForestLine False 0 0 12),29),
                         ((forestLineToGhcLine $ ForestLine False 0 0 12),31) )
      newToks2 <- liftIO $ basicTokenise "(sq pow)"
      (show newToks2) `shouldBe` "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,4)),ITvarid \"sq\"),\"sq\"),((((0,5),(0,8)),ITvarid \"pow\"),\"pow\"),((((0,8),(0,9)),ITcparen),\")\")]"

      let (tm3,_sspan3,_tree3) = updateTokensForSrcSpan tm2 (sspan2) newToks2
      (drawTreeEntry tm3) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"


      -- The test ....
      -- getToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:12:22-32:("(((False,0,0,12),22),((False,0,0,12),33))"
      let sspan4 =
                        (((forestLineToGhcLine $ ForestLine False 0 0 12),22),
                         ((forestLineToGhcLine $ ForestLine False 0 0 12),33) )
      --

      let f1' = insertSrcSpan tm3 (ss2f sspan4)
      -- (show f1) `shouldBe` ""
      (drawTreeEntry f1') `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|\n"++
            "+- ((12,19),(12,21))\n|\n"++
            "+- ((12,22),(12,33))\n|  |\n"++
            "|  +- ((12,22),(10000000012,30))\n|  |\n"++
            "|  +- ((12,25),(12,28))\n|  |\n"++
            "|  +- ((10000000012,29),(10000000012,37))\n|  |\n"++
            "|  `- ((12,32),(12,33))\n|\n"++
            "`- ((13,24),(16,22))\n"

{-
      let ss1 = posToSrcSpan forest $
                        (((forestLineToGhcLine $ ForestLine False 0 0  1), 1),
                         ((forestLineToGhcLine $ ForestLine False 0 0 12),21) )
-}
      let ss2 =
                        (((forestLineToGhcLine $ ForestLine True  0 0 12),22),
                         ((forestLineToGhcLine $ ForestLine True  0 0 12),30) )
{-
      let ss3 = posToSrcSpan forest $
                        (((forestLineToGhcLine $ ForestLine False 0 0 12),25),
                         ((forestLineToGhcLine $ ForestLine False 0 0 16),22) )
-}

      (show $ containsEnd (((ForestLine True 0 0 12),22),((ForestLine True 0 0 12),30)) (ss2f sspan4)) `shouldBe` "False"
      (show $ containsMiddle (((ForestLine True 0 0 12),22),((ForestLine True 0 0 12),30)) (ss2f sspan4)) `shouldBe` "True"

      (show $ containsEnd (((ForestLine False 0 0 12),25),((ForestLine False 0 0 16),22)) (ss2f sspan4)) `shouldBe` "True"
      (show $ containsMiddle (((ForestLine False 0 0 12),25),((ForestLine False 0 0 16),22)) (ss2f sspan4)) `shouldBe` "False"

      (show $ containsMiddle (((ForestLine False 0 0 12),25),((ForestLine False 0 0 12),28)) (ss2f sspan4)) `shouldBe` "True"

      (show $ containsMiddle (((ForestLine True 0 0 12),29),((ForestLine True 0 0 12),37)) (ss2f sspan4)) `shouldBe` "False"
      (show $ containsEnd    (((ForestLine True 0 0 12),29),((ForestLine True 0 0 12),37)) (ss2f sspan4)) `shouldBe` "True"


      (show $ containsMiddle (((ForestLine False 0 0 12),32),((ForestLine False 0 0 16),22)) (ss2f sspan4)) `shouldBe` "False"

      let (_b1,[_m1a,m1b],_e1) = splitSubtree tm3 (ss2f sspan4)
      -- (show (b1,m1,e1)) `shouldBe` "([],[],[])"

      let (_b2,_m2,_e2) = splitSubtree m1b (ss2f sspan4)
      -- (show (b2,m2,e2)) `shouldBe` "([],[],[])"


      -- let (f2,t2) = getSrcSpanFor tm3 (gs2f sspan4)

      -- (show t2) `shouldBe` ""
      --
      {-
      (show tm3) `shouldBe` ""
      Node {rootLabel = Entry (((ForestLine False 0 0 12),32),((ForestLine False 0 0 16),22)) 
        [((((12,32),(12,33)),ITvarid \"y\"),\"y\"),
         ((((13,24),(13,29)),ITwhere),\"where\"),
      -}
      let ss2g@(_ss2fs,_ss2fe)        = ss2f ss2
      let sspan4f@(_sspan4s,_sspan4e) = ss2f sspan4
      (show (ss2g,sspan4f)) `shouldBe` "((((ForestLine True 0 0 12),22),((ForestLine True 0 0 12),30)),"++
                                       "(((ForestLine False 0 0 12),22),((ForestLine False 0 0 12),33)))"
      -- (ss2fe >= sspan4s,ss2fe <= sspan4e) `shouldBe` (True,False)
      (containsStart ss2g sspan4f,containsEnd ss2g sspan4f) `shouldBe` (True,False)

      let (tm5,toks5) = getTokensFor True tm3 (sspan4)

      -- (showTree tm3) `shouldBe` ""

      (GHC.showRichTokenStream toks5) `shouldBe`
         "\n\n\n\n\n\n\n\n\n\n\n                      (sq pow)x + (sq pow)y"
      -- (showToks toks5) `shouldBe` ""

      (drawTreeEntry tm5) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|\n"++
            "+- ((12,19),(12,21))\n|\n"++
            "+- ((12,22),(12,33))\n|  |\n"++
            "|  +- ((12,22),(10000000012,30))\n|  |\n"++
            "|  +- ((12,25),(12,28))\n|  |\n"++
            "|  +- ((10000000012,29),(10000000012,37))\n|  |\n"++
            "|  `- ((12,32),(12,33))\n|\n"++
            "`- ((13,24),(16,22))\n"

    -- ---------------------------------

    it "gets tokens for a span should be same as getTokensForNoIntros" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Case/D.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      let forest = mkTreeFromTokens toks

      -- getToksForSpan test/testdata/Case/D.hs:5:12-18:("(((False,0,0,5),12),((False,0,0,5),19))",[((((5,12),(5,13)),IToparen),"("),((((5,13),(5,16)),ITvarid "odd"),"odd"),((((5,17),(5,18)),ITvarid "x"),"x"),((((5,18),(5,19)),ITcparen),")")])
      -- getToksForSpan test/testdata/Case/D.hs:(6,16)-(8,19):("(((False,0,0,6),16),((False,0,0,8),20))",[((((1,1),(1,7)),ITmodule),"module"),((((1,8),(1,9)),ITconid "C"),"C"),((((1,10),(1,15)),ITwhere),"where"),((((2,1),(2,35)),ITlineComment "-- Test for refactor of if to case")

      let sspan = ss2gs ((5,12),(5,19))
      -- (showGhc sspan) `shouldBe` "f:5:12-18"
      (showSrcSpanF sspan) `shouldBe` "(((False,0,0,5),12),((False,0,0,5),19))"

      let (tm',declToks) = getTokensFor True forest (gs2ss sspan)
      (drawTreeEntry tm') `shouldBe`
            "((1,1),(13,16))\n|\n"++
            "+- ((1,1),(5,11))\n|\n"++
            "+- ((5,12),(5,19))\n|\n"++
            "`- ((6,16),(13,16))\n"

      let expectedToks = "[((5,12),(5,13),((((5,12),(5,13)),IToparen),\"(\")),((5,13),(5,16),((((5,13),(5,16)),ITvarid \"odd\"),\"odd\")),((5,17),(5,18),((((5,17),(5,18)),ITvarid \"x\"),\"x\")),((5,18),(5,19),((((5,18),(5,19)),ITcparen),\")\"))]"
      (showToks declToks) `shouldBe` expectedToks

      let (tm2,declToks2) = getTokensForNoIntros True forest (gs2ss sspan)
      (drawTreeEntry tm2) `shouldBe`
            "((1,1),(13,16))\n|\n"++
            "+- ((1,1),(5,11))\n|\n"++
            "+- ((5,12),(5,19))\n|\n"++
            "`- ((6,16),(13,16))\n"

      (showToks declToks2) `shouldBe` expectedToks

      let sspan2 = ss2gs ((6,16),(8,20))
      -- (showGhc sspan2) `shouldBe` "f:(6,16)-(8,19)"
      (showSrcSpanF sspan2) `shouldBe` "(((False,0,0,6),16),((False,0,0,8),20))"

      let (tm'',declToks') = getTokensFor True tm' (gs2ss sspan2)
      (drawTreeEntry tm'') `shouldBe`
            "((1,1),(13,16))\n|\n"++
            "+- ((1,1),(5,11))\n|\n"++
            "+- ((5,12),(5,19))\n|\n"++
            "`- ((6,16),(13,16))\n   |\n"++
            "   +- ((6,16),(8,20))\n   |\n"++
            "   `- ((9,16),(13,16))\n"

      let expectedToks2 = "[((6,11),(6,15),((((6,11),(6,15)),ITthen),\"then\")),((6,16),(6,18),((((6,16),(6,18)),ITdo),\"do\")),((7,13),(7,37),((((7,13),(7,37)),ITlineComment \"-- This is an odd result\"),\"-- This is an odd result\")),((8,13),(8,13),((((8,13),(8,13)),ITvocurly),\"\")),((8,13),(8,16),((((8,13),(8,16)),ITvarid \"bob\"),\"bob\")),((8,17),(8,18),((((8,17),(8,18)),ITvarid \"x\"),\"x\")),((8,19),(8,20),((((8,19),(8,20)),ITinteger 1),\"1\"))]"
      (showToks declToks') `shouldBe` expectedToks2

      let (tm3,declToks3) = getTokensForNoIntros True tm2 (gs2ss sspan2)
      (drawTreeEntry tm3) `shouldBe`
            "((1,1),(13,16))\n|\n"++
            "+- ((1,1),(5,11))\n|\n"++
            "+- ((5,12),(5,19))\n|\n"++
            "`- ((6,16),(13,16))\n   |\n"++
            "   +- ((6,16),(8,20))\n   |\n"++
            "   `- ((9,16),(13,16))\n"
      -- no `then` token with this set
      (showToks declToks3) `shouldBe` "[((6,16),(6,18),((((6,16),(6,18)),ITdo),\"do\")),((7,13),(7,37),((((7,13),(7,37)),ITlineComment \"-- This is an odd result\"),\"-- This is an odd result\")),((8,13),(8,13),((((8,13),(8,13)),ITvocurly),\"\")),((8,13),(8,16),((((8,13),(8,16)),ITvarid \"bob\"),\"bob\")),((8,17),(8,18),((((8,17),(8,18)),ITvarid \"x\"),\"x\")),((8,19),(8,20),((((8,19),(8,20)),ITinteger 1),\"1\"))]"

  -- ---------------------------------------------

  describe "getTokensBefore" $ do
    it "gets the tokens before a given srcloc" $ do
      -- (_t,toks) <- parsedFileMoveDefMd1
      (_t,toks) <- parsedFileGhc "./test/testdata/MoveDef/Md1.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed

      let forest = mkTreeFromTokens toks

      let sspan = ss2gs ((24,5),(24,11))

      let (tm',toksSpan) = getTokensFor True forest (gs2ss sspan)

      -- (showGhc sspan) `shouldBe` "f:24:5-10"
      (showSrcSpan sspan) `shouldBe` "((24,5),(24,11))"
      (showToks toksSpan) `shouldBe` "[((24,5),(24,5),((((24,5),(24,5)),ITvocurly),\"\")),((24,5),(24,7),((((24,5),(24,7)),ITvarid \"zz\"),\"zz\")),((24,8),(24,9),((((24,8),(24,9)),ITequal),\"=\")),((24,10),(24,11),((((24,10),(24,11)),ITinteger 1),\"1\"))]"


      (drawTreeEntry tm') `shouldBe`
            "((1,1),(40,17))\n|\n"++
            "+- ((1,1),(23,8))\n|\n"++
            "+- ((24,5),(24,11))\n|\n"++
            "`- ((26,1),(40,17))\n"

      let (tm'',toksBefore) = getTokensBefore tm' (gs2ss sspan)
      (showToks $ drop 100 $ unReverseToks toksBefore) `shouldBe` "[((22,10),(22,11),((((22,10),(22,11)),ITvarsym \"+\"),\"+\")),((22,12),(22,14),((((22,12),(22,14)),ITvarid \"zz\"),\"zz\")),((23,3),(23,8),((((23,3),(23,8)),ITwhere),\"where\"))]"

      (drawTreeEntry tm'') `shouldBe`
            "((1,1),(40,17))\n|\n"++
            "+- ((1,1),(23,8))\n|\n"++
            "+- ((24,5),(24,11))\n|\n"++
            "`- ((26,1),(40,17))\n"

  -- ---------------------------------------------

  describe "getSrcSpanFor" $ do
    it "inserts a SrcSpan if it was not in the forest" $ do
      -- (t,toks) <- parsedFileTokenTestGhc
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      let forest = mkTreeFromTokens toks

      let l = ss2gs ((19,1),(21,14))
      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"
      (showForestSpan (gs2f l)) `shouldBe` "((19,1),(21,14))"
      (drawTreeEntry forest') `shouldBe`
            "((1,1),(21,14))\n|\n"++
            "+- ((1,1),(15,17))\n|\n"++
            "`- ((19,1),(21,14))\n"

      (showForestSpan $ treeStartEnd tree) `shouldBe` "((19,1),(21,14))"

  -- ---------------------------------------------

  describe "containment" $ do
    it "checks containsStart,containsMiddle and containsEnd" $ do
      let sspan@(s,_e) = (((ForestLine False 0 0 24),1),((ForestLine False 0 0 24),4))
      let nspan@(ns,ne) = (((ForestLine False 0 1 24),1),((ForestLine False 0 1 26),14))

      (show $ compare s ns) `shouldBe` "LT"

      -- "0" ++ (show $ s >= ns) `shouldBe` "0True"
      "1" ++ (show $ s <= ne) `shouldBe` "1True"

      "2" ++ (show $ containsStart  nspan sspan) `shouldBe` "2False"
      "3" ++ (show $ containsMiddle nspan sspan) `shouldBe` "3False"
      "4" ++ (show $ containsEnd    nspan sspan) `shouldBe` "4False"


  -- ---------------------------------------------

  describe "insertSrcSpan" $ do
    it "checks that the forest is split into two parts" $ do
      -- (t,toks) <- parsedFileTokenTestGhc
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      let l = ss2gs ((19,1),(21,14))
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      --
      let z = openZipperToSpan (gs2f l) $ Z.fromTree forest
      let toksz = retrieveTokensInterim $ Z.tree z
      let (tokStartPos,tokEndPos) = forestSpanToSimpPos (gs2f l)
      (show (tokStartPos,tokEndPos)) `shouldBe` "((19,1),(21,14))"

      -- (show toksz) `shouldBe` ""
      let (_begin,middle,_end) = splitToks (tokStartPos,tokEndPos) toksz
      (show middle) `shouldBe` "[((((19,1),(19,1)),ITsemi),\"\"),((((19,1),(19,4)),ITvarid \"foo\"),\"foo\"),((((19,5),(19,6)),ITvarid \"x\"),\"x\"),((((19,7),(19,8)),ITvarid \"y\"),\"y\"),((((19,9),(19,10)),ITequal),\"=\"),((((20,3),(20,5)),ITdo),\"do\"),((((20,6),(20,6)),ITvocurly),\"\"),((((20,6),(20,7)),ITvarid \"c\"),\"c\"),((((20,8),(20,10)),ITlarrow),\"<-\"),((((20,11),(20,18)),ITvarid \"getChar\"),\"getChar\"),((((21,6),(21,6)),ITsemi),\"\"),((((21,6),(21,12)),ITvarid \"return\"),\"return\"),((((21,13),(21,14)),ITvarid \"c\"),\"c\")]"

      let (startLoc,endLoc) = startEndLocIncComments' toksz (tokStartPos,tokEndPos)
      (show (startLoc,endLoc)) `shouldBe` "((18,1),(21,14))"

      --

      let forest' = insertSrcSpan forest (gs2f l)
      (invariant forest') `shouldBe` []
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "`- ((19,1),(21,14))\n" -- our inserted span

    -- -----------------------

    it "inserts a span above others, if it spans them" $ do
      -- (t,toks) <- parsedFileTokenTestGhc
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      let l = ss2gs ((19,1),(21,14))
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let forest' = insertSrcSpan forest (gs2f l)
      (invariant forest') `shouldBe` []
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "`- ((19,1),(21,14))\n" -- our inserted span

      let l' = ss2gs ((8,1),(10,10))
      -- (showGhc l') `shouldBe` "f:(8,1)-(10,9)"
      (showSrcSpan l') `shouldBe` "((8,1),(10,10))"

      let forest'' = insertSrcSpan forest' (gs2f l')
      (invariant forest'') `shouldBe` []
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|  |\n"++
              "|  +- ((1,1),(6,14))\n|  |\n"++
              "|  +- ((8,1),(10,10))\n|  |\n"++ -- our inserted span
              "|  `- ((13,1),(15,17))\n|\n"++
              "`- ((19,1),(21,14))\n"

    ------------------------------------

    it "does not delete existing versioned spans" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/D1.hs"
      let forest = mkTreeFromTokens toks

      let sspan = ss2gs ((6,21),(6,41))
      -- (showGhc sspan) `shouldBe` "f:6:21-40"
      (showSrcSpan sspan) `shouldBe` "((6,21),(6,41))"

      let forest1 = insertSrcSpan forest (gs2f sspan)

      declToks <- liftIO $ tokenise (realSrcLocFromTok mkZeroToken) 0 True "where\n  sq = x ^ pow\n"
      -- putToksAfterPos ((6,21),(6,41)) at PlaceIndent 1 4 2
      let (forest2,_newSpan) = addToksAfterSrcSpan forest1 (gs2ss sspan) (PlaceIndent 1 4 2) declToks

      (invariant forest2) `shouldBe` []
      (drawTreeEntry forest2) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000008,20))\n|\n"++
               "`- ((7,1),(13,25))\n"

      -- Context set up, now for the test.

      let sspan2 = ss2gs ((9,1),(9,14))
      -- (showGhc sspan2) `shouldBe` "f:9:1-13"

      let z = openZipperToSpan (gs2f sspan2) $ Z.fromTree forest2
      (drawTreeEntry $ Z.tree z) `shouldBe` "((7,1),(13,25))\n"
      -- (show $ treeStartEnd (Z.tree z)) `shouldBe` "((ForestLine {flInsertVersion = 0, flLine = 1},1),(ForestLine {flInsertVersion = 0, flLine = 13},25))"
      -- (show (Z.firstChild z)) `shouldBe` ""
      -- (show $ Z.next $ fromJust $ (Z.firstChild z)) `shouldBe` ""
 {-
      let childrenAsZ = go [] (Z.firstChild z)
           where
            go acc Nothing = acc
            go acc (Just zz) = go (acc ++ [zz]) (Z.next zz)

      (show $ map treeStartEnd $ map Z.tree childrenAsZ) `shouldBe`
           "[((ForestLine {flInsertVersion = 0, flLine = 1},1),(ForestLine {flInsertVersion = 0, flLine = 6},20)),"++
            "((ForestLine {flInsertVersion = 0, flLine = 6},21),(ForestLine {flInsertVersion = 0, flLine = 6},41)),"++
            "((ForestLine {flInsertVersion = 1, flLine = 7},5),(ForestLine {flInsertVersion = 1, flLine = 11},1)),"++
            "((ForestLine {flInsertVersion = 0, flLine = 7},1),(ForestLine {flInsertVersion = 0, flLine = 13},25))]"


      let xx = filter contains childrenAsZ
           where
             contains zn = (startPos <= nodeStart && endPos >= nodeEnd)
               where
                 (startPos,endPos) = treeStartEnd $ Z.tree zn
                 (nodeStart,nodeEnd) = (fs sspan2)

      -- (show xx) `shouldBe` ""

      let xx' = filter (\zt -> (treeStartEnd $ Z.tree zt) == (fs sspan2)) xx

      -- (show xx') `shouldBe` ""
-}


      (drawTreeEntry $ insertSrcSpan forest2 (gs2f sspan2)) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000008,20))\n|\n"++
               "`- ((7,1),(13,25))\n   |\n"++
               "   +- ((7,1),(7,18))\n   |\n"++
               "   +- ((9,1),(9,14))\n   |\n"++
               "   `- ((11,1),(13,25))\n"

    ------------------------------------

    it "insert a span after deleting one" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/LetIn1.hs"
      let forest = mkTreeFromTokens toks

      -- getToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:12:22-32

      let sspan = ss2gs ((12,22),(12,33))
      -- (showGhc sspan) `shouldBe` "f:12:22-32"
      (showSrcSpan sspan) `shouldBe` "((12,22),(12,33))"

      -- let forest1 = insertSrcSpan forest (fs sspan)
      let (forest1,declToks) = getTokensFor True forest (gs2ss sspan)
      -- (show forest1) `shouldBe` ""

      -- removeToksForPos ((10,22),(11,32))
      let sspan2 = ss2gs ((10,22),(11,32))
      let (f2,_t2) = removeSrcSpan forest1 (gs2f sspan2)

      (invariant f2) `shouldBe` []
      (drawTreeEntry f2) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,21))\n|  |\n"++
            "|  `- ((10,22),(11,32))(1,-13)D\n|\n"++
            "+- ((12,22),(12,33))\n|\n"++
            "`- ((13,24),(16,22))\n"

      -- Context in place, time for test


      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:(10,18)-(12,32):
      (show declToks) `shouldBe` "[((((12,19),(12,19)),ITvccurly),\"\"),((((12,19),(12,21)),ITin),\"in\"),((((12,22),(12,24)),ITvarid \"sq\"),\"sq\"),((((12,25),(12,26)),ITvarid \"x\"),\"x\"),((((12,27),(12,28)),ITvarsym \"+\"),\"+\"),((((12,29),(12,31)),ITvarid \"sq\"),\"sq\"),((((12,32),(12,33)),ITvarid \"y\"),\"y\")]"

      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:(10,18)-(12,32)
      let sspan3 = ss2gs ((10,18),(12,33))

--
      let (_b1,_m1,_e1) = splitSubtree f2 (gs2f sspan3)
      -- (show (b1,m1,e1)) `shouldBe` "([],[],[])"

--


      let (f3,_newSpan3,_tree3) = updateTokensForSrcSpan f2 (gs2ss sspan3) declToks

      (invariant f3) `shouldBe` []
      (drawTreeEntry f3) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(10,17))\n|\n"++
            "+- ((10000000010,18),(10000000010,32))\n|\n"++
            "`- ((13,24),(16,22))\n"

    ------------------------------------

    it "manipulates the Token Tree without breaking the invariant" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/LetIn1.hs"
      let forest = mkTreeFromTokens toks


      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:12:22-23:(((False,0,0,12),22),((False,0,0,12),24))
      newToks1 <- liftIO $ basicTokenise "(sq pow)"
      (show newToks1) `shouldBe` "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,4)),ITvarid \"sq\"),\"sq\"),((((0,5),(0,8)),ITvarid \"pow\"),\"pow\"),((((0,8),(0,9)),ITcparen),\")\")]"

      let sspan1 = ss2gs ((12,22),(12,24))

      let (f2,_newSpan2,_tree2) = updateTokensForSrcSpan forest (gs2ss sspan1) newToks1

      (invariant f2) `shouldBe` []
      (drawTreeEntry f2) `shouldBe`
               "((1,1),(16,22))\n|\n"++
               "+- ((1,1),(11,32))\n|\n"++
               "+- ((10000000012,19),(10000000012,30))\n|\n"++
               "`- ((12,25),(16,22))\n"


      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:12:29-30:(((False,0,0,12),29),((False,0,0,12),31))
      (show newToks1) `shouldBe` "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,4)),ITvarid \"sq\"),\"sq\"),((((0,5),(0,8)),ITvarid \"pow\"),\"pow\"),((((0,8),(0,9)),ITcparen),\")\")]"

      let sspan2 = ss2gs ((12,29),(12,31))

      let (f3,_newSpan3,_tree3) = updateTokensForSrcSpan f2 (gs2ss sspan2) newToks1

      (invariant f3) `shouldBe` []
      (drawTreeEntry f3) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"


      -- getToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:10:25:("(((False,0,0,10),25),((False,0,0,10),26))",[((((10,25),(10,26)),ITinteger 0),"0")])
      let sspan3 = ss2gs ((10,25),(10,26))
      let (f4,_toks4) = getTokensFor True f3 (gs2ss sspan3)

      (invariant f4) `shouldBe` []
      (drawTreeEntry f4) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,24))\n|  |\n"++
            "|  +- ((10,25),(10,26))\n|  |\n"++
            "|  `- ((10,26),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"


      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:10:25:(((False,0,0,10),25),((False,0,0,10),26))
      newToks2 <- liftIO $ basicTokenise "pow" -- TODO: check that
                                               -- using basicTokenise
                                               -- here is ok
      -- (show newToks2) `shouldBe` "[((((10,26),(10,29)),ITvarid \"pow\"),\"pow\")]"
      let (f5,_newSpan5,_tree5) = updateTokensForSrcSpan f4 (gs2ss sspan3) newToks2

      (invariant f5) `shouldBe` []
      (drawTreeEntry f5) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,24))\n|  |\n"++
            "|  +- ((10000000010,25),(10000000010,28))\n|  |\n"++
            "|  `- ((10,26),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"


      -- putToksAfterSpan test/testdata/LiftToToplevel/LetIn1.hs:10:25:(((False,0,0,10),25),((False,0,0,10),26)) at PlaceAdjacent:[(((10,25),(10,26)),ITinteger 0,"0")]
      let sspan5 = ss2gs ((10,25),(10,26))
      newToks3 <- liftIO $ basicTokenise "0"
      (show newToks3) `shouldBe` "[((((0,1),(0,2)),ITinteger 0),\"0\")]"
      let (f6,_newSpan6) = addToksAfterSrcSpan f5 (gs2ss sspan5) PlaceAdjacent newToks3

      (invariant f6) `shouldBe` []
      (drawTreeEntry f6) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,24))\n|  |\n"++
            "|  +- ((10000000010,25),(10000000010,28))\n|  |  |\n"++
            "|  |  +- ((10,25),(10,26))\n|  |  |\n"++
            "|  |  `- ((1000010,29),(1000010,30))\n|  |\n"++
            "|  `- ((10,26),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"


      -- getToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:11:25:("(((False,0,0,11),25),((False,0,0,11),26))",[((((11,25),(11,26)),ITvarid "z"),"z")])

      let sspan6 = ss2gs ((11,25),(11,26))
      let (f7,_toks7) = getTokensFor True f6 (gs2ss sspan6)

      (invariant f7) `shouldBe` []
      (drawTreeEntry f7) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,24))\n|  |\n"++
            "|  +- ((10000000010,25),(10000000010,28))\n|  |  |\n"++
            "|  |  +- ((10,25),(10,26))\n|  |  |\n"++
            "|  |  `- ((1000010,29),(1000010,30))\n|  |\n"++
            "|  `- ((10,26),(11,32))\n|     |\n"++
            "|     +- ((10,26),(11,24))\n|     |\n"++
            "|     +- ((11,25),(11,26))\n|     |\n"++
            "|     `- ((11,26),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"

      -- putToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:11:25:(((False,0,0,11),25),((False,0,0,11),26))[((((11,26),(11,29)),ITvarid "pow"),"pow")]

      let (f8,_newSpan8,_tree8) = updateTokensForSrcSpan f7 (gs2ss sspan6) newToks2

      (invariant f8) `shouldBe` []
      (drawTreeEntry f8) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,24))\n|  |\n"++
            "|  +- ((10000000010,25),(10000000010,28))\n|  |  |\n"++
            "|  |  +- ((10,25),(10,26))\n|  |  |\n"++
            "|  |  `- ((1000010,29),(1000010,30))\n|  |\n"++
            "|  `- ((10,26),(11,32))\n|     |\n"++
            "|     +- ((10,26),(11,24))\n|     |\n"++
            "|     +- ((10000000011,25),(10000000011,28))\n|     |\n"++
            "|     `- ((11,26),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"


      -- putToksAfterSpan test/testdata/LiftToToplevel/LetIn1.hs:11:25:(((False,0,0,11),25),((False,0,0,11),26)) at PlaceAdjacent:[(((11,25),(11,26)),ITvarid "z","z")]

      newToks4 <- liftIO $ basicTokenise "z"
      (show newToks4) `shouldBe` "[((((0,1),(0,2)),ITvarid \"z\"),\"z\")]"
      let (f9,_newSpan9) = addToksAfterSrcSpan f8 (gs2ss sspan6) PlaceAdjacent newToks4

      (invariant f9) `shouldBe` []
      (drawTreeEntry f9) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,24))\n|  |\n"++
            "|  +- ((10000000010,25),(10000000010,28))\n|  |  |\n"++
            "|  |  +- ((10,25),(10,26))\n|  |  |\n"++
            "|  |  `- ((1000010,29),(1000010,30))\n|  |\n"++
            "|  `- ((10,26),(11,32))\n|     |\n"++
            "|     +- ((10,26),(11,24))\n|     |\n"++
            "|     +- ((10000000011,25),(10000000011,28))\n|     |  |\n"++
            "|     |  +- ((11,25),(11,26))\n|     |  |\n"++
            "|     |  `- ((1000011,29),(1000011,30))\n|     |\n"++
            "|     `- ((11,26),(11,32))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"

      -- Context in place, time for test

      -- getToksForSpan test/testdata/LiftToToplevel/LetIn1.hs:(10,22)-(11,31)
      let sspan9 = ss2gs ((10,22),(11,31))
      let (f10,_toks10) = getTokensFor True f9 (gs2ss sspan9)

--
      let z = openZipperToSpan (gs2f sspan9) $ Z.fromTree f9
      -- let (before,middle,end) = doSplitTree (Z.tree z) (fs sspan9)
      let (before1,middle,end) = splitSubtree (Z.tree z) (gs2f sspan9)
      (show (map treeStartEnd before1,map treeStartEnd middle,map treeStartEnd end)) `shouldBe`
               "([],"++
               "[(((ForestLine False 0 0 1),1),((ForestLine False 0 0 10),24)),"++
                "(((ForestLine True 0 0 10),25),((ForestLine True 0 0 10),28)),"++
                "(((ForestLine False 0 0 10),26),((ForestLine False 0 0 11),32))],"++
               "[])"
      -- let (b2,m2,e2) = splitSubToks (head middle) (sf sspan9)
      -- (show (b2,m2,e2)) `shouldBe` ""
      let (b3,m3,e3) = splitSubtree (last middle) (gs2f sspan9)
      (show (map treeStartEnd b3,map treeStartEnd m3,map treeStartEnd  e3)) `shouldBe`
               "([],"++
               "[(((ForestLine False 0 0 10),26),((ForestLine False 0 0 11),24)),"++
                "(((ForestLine True 0 0 11),25),((ForestLine True 0 0 11),28)),"++
                "(((ForestLine False 0 0 11),26),((ForestLine False 0 0 11),32))],"++
               "[])"
      let ss9 = (((ForestLine False 0 0 11),26),((ForestLine False 0 0 12),21))
      (show (containsStart (ss9) (gs2f sspan9),containsEnd ss9 (gs2f sspan9))) `shouldBe` "(False,True)"
      -- let (b4,m4,e4) = splitSubToks (last m3) (sf sspan9)
      -- (show (b4,m4,e4)) `shouldBe` ""
--

      (drawTreeEntry f10) `shouldBe`
            "((1,1),(16,22))\n|\n"++
            "+- ((1,1),(11,32))\n|  |\n"++
            "|  +- ((1,1),(10,21))\n|  |\n"++
            "|  `- ((10,22),(11,31))\n|     |\n"++
            "|     +- ((10,22),(10,24))\n|     |\n"++
            "|     +- ((10000000010,25),(10000000010,28))\n|     |  |\n"++
            "|     |  +- ((10,25),(10,26))\n|     |  |\n"++
            "|     |  `- ((1000010,29),(1000010,30))\n|     |\n"++
            "|     +- ((10,26),(11,24))\n|     |\n"++
            "|     +- ((10000000011,25),(10000000011,28))\n|     |  |\n"++
            "|     |  +- ((11,25),(11,26))\n|     |  |\n"++
            "|     |  `- ((1000011,29),(1000011,30))\n|     |\n"++
            "|     `- ((11,26),(11,31))\n|\n"++
            "+- ((10000000012,19),(10000000012,30))\n|\n"++
            "`- ((12,25),(16,22))\n   |\n"++
            "   +- ((12,25),(12,28))\n   |\n"++
            "   +- ((10000000012,29),(10000000012,37))\n   |\n"++
            "   `- ((12,32),(16,22))\n"
      (invariant f10) `shouldBe` []

    ------------------------------------

    it "updates tokens without breaking things" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/PatBindIn3.hs"
      let forest = mkTreeFromTokens toks


      -- putToksForSpan test/testdata/LiftToToplevel/PatBindIn3.hs:9:16-17:(((False,0,0,9),16),((False,0,0,9),18))
      newToks1 <- liftIO $ basicTokenise "(sq x pow)"
      (show newToks1) `shouldBe` "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,4)),ITvarid \"sq\"),\"sq\"),((((0,5),(0,6)),ITvarid \"x\"),\"x\"),((((0,7),(0,10)),ITvarid \"pow\"),\"pow\"),((((0,10),(0,11)),ITcparen),\")\")]"

      let sspan1 = ss2gs ((9,16),(9,18))

      let (f2,_newSpan2,_tree2) = updateTokensForSrcSpan forest (gs2ss sspan1) newToks1

      (invariant f2) `shouldBe` []
      (drawTreeEntry f2) `shouldBe`
               "((1,1),(15,22))\n|\n"++
               "+- ((1,1),(9,15))\n|\n"++
               "+- ((10000000009,16),(10000000009,26))\n|\n"++
               "`- ((9,19),(15,22))\n"
      -- let toks2 = retrieveTokensFinal f2
      -- (GHC.showRichTokenStream toks2) `shouldBe` "module LiftToToplevel.PatBindIn3 where\n\n --A definition can be lifted from a where or let to the top level binding group.\n --Lifting a definition widens the scope of the definition.\n\n --In this example, lift 'sq' defined in 'sumSquares'\n --This example aims to test changing a constant to a function.\n\n sumSquares x = (sq x pow)+ sq\n            where\n               sq = x^pow\n               pow =2\n\n anotherFun 0 y = sq y\n      where sq x = x^2\n\n "
      (renderLinesFromLayoutTree f2) `shouldBe` "module LiftToToplevel.PatBindIn3 where\n\n--A definition can be lifted from a where or let to the top level binding group.\n--Lifting a definition widens the scope of the definition.\n\n--In this example, lift 'sq' defined in 'sumSquares'\n--This example aims to test changing a constant to a function.\n\nsumSquares x = (sq x pow)+ sq\n           where\n              sq = x^pow\n              pow =2\n\nanotherFun 0 y = sq y\n     where sq x = x^2\n\n"

      -- putToksForSpan test/testdata/LiftToToplevel/PatBindIn3.hs:9:21-22:(((False,0,0,9),21),((False,0,0,9),23))
      (show newToks1) `shouldBe` "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,4)),ITvarid \"sq\"),\"sq\"),((((0,5),(0,6)),ITvarid \"x\"),\"x\"),((((0,7),(0,10)),ITvarid \"pow\"),\"pow\"),((((0,10),(0,11)),ITcparen),\")\")]"

      let sspan2 = ss2gs ((9,21),(9,23))

--
      -- (show f2) `shouldBe` ""
{-
      let z = openZipperToSpan (fs sspan2) $ Z.fromTree f2
      (Z.isLeaf z) `shouldBe` True

      let (forest',tree@(Node (Entry _s _) _)) = getSrcSpanFor f2 (fs sspan2)
      (drawTreeEntry forest') `shouldBe`
               "((1,1),(15,22))\n|\n"++
               "+- ((1,1),(9,15))\n|\n"++
               "+- ((10000000009,16),(10000000009,26))\n|\n"++
               "`- ((9,19),(15,22))\n   |\n"++
               "   +- ((9,19),(9,20))\n   |\n"++
               "   +- ((9,21),(9,23))\n   |\n"++
               "   `- ((10,12),(15,22))\n"

      let zf = openZipperToNode tree $ Z.fromTree forest'
      (show zf) `shouldBe` ""
-}
--

      let (f3,_newSpan3,_tree3) = updateTokensForSrcSpan f2 (gs2ss sspan2) newToks1
      (invariant f3) `shouldBe` []
      (drawTreeEntry f3) `shouldBe`
               "((1,1),(15,22))\n|\n"++
               "+- ((1,1),(9,15))\n|\n"++
               "+- ((10000000009,16),(10000000009,26))\n|\n"++
               "`- ((9,19),(15,22))\n   |\n"++
               "   +- ((9,19),(9,20))\n   |\n"++
               "   +- ((10000000009,21),(10000000009,31))\n   |\n"++
               "   `- ((10,12),(15,22))\n"
      -- let toks3 = retrieveTokensFinal f3
      -- (GHC.showRichTokenStream toks3) `shouldBe` "module LiftToToplevel.PatBindIn3 where\n\n --A definition can be lifted from a where or let to the top level binding group.\n --Lifting a definition widens the scope of the definition.\n\n --In this example, lift 'sq' defined in 'sumSquares'\n --This example aims to test changing a constant to a function.\n\n sumSquares x = (sq x pow)+ (sq x pow)\n            where\n               sq = x^pow\n               pow =2\n\n anotherFun 0 y = sq y\n      where sq x = x^2\n\n "

      (renderLinesFromLayoutTree f3) `shouldBe` "module LiftToToplevel.PatBindIn3 where\n\n--A definition can be lifted from a where or let to the top level binding group.\n--Lifting a definition widens the scope of the definition.\n\n--In this example, lift 'sq' defined in 'sumSquares'\n--This example aims to test changing a constant to a function.\n\nsumSquares x = (sq x pow)+ (sq x pow)\n           where\n              sq = x^pow\n              pow =2\n\nanotherFun 0 y = sq y\n     where sq x = x^2\n\n"

    ------------------------------------

    it "allocates comments in an if then else expression" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Case/C.hs"
      let t1 = mkTreeFromTokens toks

      -- getToksForSpan test/testdata/Case/C.hs:5:12-18:("(((False,0,0,5),12),((False,0,0,5),19))
      let s1 = ss2gs
                 (((forestLineToGhcLine $ ForestLine False 0 0 5),12),
                  ((forestLineToGhcLine $ ForestLine False 0 0 5),19) )
      -- (showGhc s1) `shouldBe` "f:5:12-18"
      let t2 = insertSrcSpan t1 (gs2f s1)

      -- getToksForSpan test/testdata/Case/C.hs:7:13-19:("(((False,0,0,7),13),((False,0,0,7),20))
      let s2 = ss2gs
                 (((forestLineToGhcLine $ ForestLine False 0 0 7),13),
                  ((forestLineToGhcLine $ ForestLine False 0 0 7),20) )
      -- (showGhc s2) `shouldBe` "f:7:13-19"
      let t3 = insertSrcSpan t2 (gs2f s2)

-- innards of insertSrcSpan
      let sspan = gs2f s2
      let z = openZipperToSpan (gs2f s2) $ Z.fromTree t2
      (Z.isLeaf z) `shouldBe` True
      let (Entry _ _ toks1) = Z.label z
      let (tokStartPos,tokEndPos) = forestSpanToSimpPos sspan

      (GHC.showRichTokenStream toks1) `shouldBe` "\n\n\n\n\n           then -- This is an odd result\n             bob x 1\n           else -- This is an even result\n             bob x 2\n\n bob x y = x + y\n\n "

      let (startLoc,endLoc) = startEndLocIncComments' toks1 (tokStartPos,tokEndPos)
      -- let (startLoc,endLoc) = startEndLocIncCommentsDebug toks1 (tokStartPos,tokEndPos)
      (show (tokStartPos,tokEndPos)) `shouldBe` "((7,13),(7,20))"
      (show (startLoc,endLoc)) `shouldBe` "((6,11),(7,20))"
--


      -- getToksForSpan test/testdata/Case/C.hs:9:13-19:("(((False,0,0,9),13),((False,0,0,9),20))  
      let s3 = ss2gs
                 (((forestLineToGhcLine $ ForestLine False 0 0 9),13),
                  ((forestLineToGhcLine $ ForestLine False 0 0 9),20) )
      -- (showGhc s3) `shouldBe` "f:9:13-19"
      let t4 = insertSrcSpan t3 (gs2f s3)

      (drawTreeEntry t4) `shouldBe`
            "((1,1),(11,16))\n|\n"++
            "+- ((1,1),(5,11))\n|\n"++
            "+- ((5,12),(5,19))\n|\n"++
            "`- ((7,13),(11,16))\n   |\n"++
            "   +- ((7,13),(7,20))\n   |\n"++
            "   `- ((9,13),(11,16))\n      |\n"++
            "      +- ((9,13),(9,20))\n      |\n"++
            "      `- ((11,1),(11,16))\n"

      let (_,thenToks) = getTokensFor False t4 (gs2ss s2)
      let (_,elseToks) = getTokensFor False t4 (gs2ss s3)

      (GHC.showRichTokenStream thenToks) `shouldBe`
           "\n\n\n\n\n           "++
           -- "-- This is an odd result\n             bob x 1"
           "then -- This is an odd result\n             bob x 1"
      (GHC.showRichTokenStream elseToks) `shouldBe`
           "\n\n\n\n\n\n\n           "++
           "else -- This is an even result\n             bob x 2"

  -- ---------------------------------------------

  describe "removeSrcSpan" $ do
    it "removes a span from the forest" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      let l = ss2gs ((19,1),(21,14))
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let forest' = insertSrcSpan forest (gs2f l)
      (invariant forest') `shouldBe` []
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "`- ((19,1),(21,14))\n" -- our inserted span

      let (forest'',delTree) = removeSrcSpan forest' (gs2f l)
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
               "+- ((1,1),(15,17))\n|\n"++
               "`- ((19,1),(21,14))(2,0)D\n" -- removed again

      (invariant forest'') `shouldBe` []

      (drawTreeEntry delTree) `shouldBe`
              "((19,1),(21,14))\n" -- removed again

      -- let toks' = retrieveTokensFinal forest''
      -- (showToks toks') `shouldBe` ""
      -- (GHC.showRichTokenStream toks') `shouldBe` "module TokenTest where\n\n -- Test new style token manager\n\n bob a b = x\n   where x = 3\n\n bib a b = x\n   where\n     x = 3\n\n\n bab a b =\n   let bar = 3\n   in     b + bar -- ^trailing comment"

      (renderLinesFromLayoutTree forest'') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment"

    -- ---------------------------------

    it "removes a span and tokens that were not explicitly in the forest" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head $ drop 1 decls
      let l = ss2gs ((13,1),(15,17))
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(13,1)-(15,16)"
      (showSrcSpan l) `shouldBe` "((13,1),(15,17))"

      let (forest',delTree) = removeSrcSpan forest (gs2f l)
      (invariant forest') `shouldBe` []
      (drawTreeEntry forest') `shouldBe`
               "((1,1),(21,14))\n|\n"++
               "+- ((1,1),(10,10))\n|\n"++
               "+- ((13,1),(15,17))(3,-16)D\n|\n"++
               "`- ((19,1),(21,14))\n"

      (drawTreeEntry delTree) `shouldBe`
              "((13,1),(15,17))\n" -- removed again

      -- let toks' = retrieveTokensFinal forest'
      -- (showToks toks') `shouldBe` ""
      -- (GHC.showRichTokenStream toks') `shouldBe` "module TokenTest where\n\n -- Test new style token manager\n\n bob a b = x\n   where x = 3\n\n bib a b = x\n   where\n     x = 3\n\n -- leading comment\n foo x y =\n   do c <- getChar\n      return c\n\n\n\n\n "

      (renderLinesFromLayoutTree forest') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n\n\n\n"

    -- ---------------------------------

    it "removes a span and tokens without destroying the forest 1" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/D1.hs"
      let forest = mkTreeFromTokens toks

      let sspan = ss2gs ((6,21),(6,41))
      -- (showGhc sspan) `shouldBe` "f:6:21-40"
      (showSrcSpan sspan) `shouldBe` "((6,21),(6,41))"

      let forest1 = insertSrcSpan forest (gs2f sspan)

      declToks <- liftIO $ tokenise (realSrcLocFromTok mkZeroToken) 0 True "where\n  sq = x ^ pow\n"
      -- putToksAfterPos ((6,21),(6,41)) at PlaceIndent 1 4 2
      let (forest2,_newSpan) = addToksAfterSrcSpan forest1 (gs2ss sspan) (PlaceIndent 1 4 2) declToks

      (invariant forest2) `shouldBe` []
      (drawTreeEntry forest2) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000008,20))\n|\n"++
               "`- ((7,1),(13,25))\n"

      -- Context set up, now for the test.

      let sspan2 = ss2gs ((9,1),(9,14))
      -- (showGhc sspan2) `shouldBe` "f:9:1-13"
      let (forest3,delTree) = removeSrcSpan forest2 (gs2f sspan2)

      (drawTreeEntry $ insertSrcSpan forest2 (gs2f sspan2)) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000008,20))\n|\n"++
               "`- ((7,1),(13,25))\n   |\n"++
               "   +- ((7,1),(7,18))\n   |\n"++
               "   +- ((9,1),(9,14))\n   |\n"++
               "   `- ((11,1),(13,25))\n"

      (drawTreeEntry delTree) `shouldBe`
              "((9,1),(9,14))\n" -- removed again


      (invariant forest3) `shouldBe` []
      (drawTreeEntry forest3) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000008,20))\n|\n"++
               "`- ((7,1),(13,25))\n   |\n"++
               "   +- ((7,1),(7,18))\n   |\n"++
               "   +- ((9,1),(9,14))(2,-13)D\n   |\n"++
               "   `- ((11,1),(13,25))\n"

      -- let toks' = retrieveTokensFinal forest3
      -- (showToks toks') `shouldBe` ""
      -- (GHC.showRichTokenStream toks') `shouldBe` "module Demote.D1 where\n\n {-demote 'sq' to 'sumSquares'. This refactoring\n  affects module 'D1' and 'C1' -}\n\n sumSquares (x:xs) = sq x + sumSquares xs\n     where\n        sq = x ^ pow\n      \n \n sumSquares [] = 0\n\n pow = 2\n\n main = sumSquares [1..4]\n\n "

      (renderLinesFromLayoutTree forest3) `shouldBe` "module Demote.D1 where\n\n{-demote 'sq' to 'sumSquares'. This refactoring\n  affects module 'D1' and 'C1' -}\n\nsumSquares (x:xs) = sq x + sumSquares xs\n    where\n       sq = x ^ pow\n     \n\nsumSquares [] = 0\n\npow = 2\n\nmain = sumSquares [1..4]\n\n"

    -- ---------------------------------

    it "removes a span and tokens without destroying the forest 2" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/WhereIn6.hs"
      -- let forest = mkTreeFromTokens toks
      let tk = initTokenCache toks

      -- removeToksForPos ((13,1),(13,21))
      let sspan = ss2gs ((13,1),(13,21))
      -- (showGhc sspan) `shouldBe` "f:13:1-20"

      let tk2 = removeToksFromCache tk (gs2ss sspan)
      (drawTokenCache tk2) `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,21))\n|\n"++
               "+- ((1,1),(11,25))\n|\n"++
               "`- ((13,1),(13,21))(2,0)D\n"++
               "tree TId 1:\n"++
               "((100000013,1),(100000013,21))\n"

      -- putToksForSpan test/testdata/Demote/WhereIn6.hs:100000013:16:[((((0,1),(0,2)),ITvarid "x"),"x")]
      -- let ss2 = ss2gs ((100000013,16),(100000013,17))
      let ss2 = ss2gs $ (((forestLineToGhcLine $ ForestLine False 1 0 13),16),
                                       ((forestLineToGhcLine $ ForestLine False 1 0 13),17) )

      -- (showGhc ss2) `shouldBe` "f:100000013:16"
      -- (showGhc ss2) `shouldBe` "f:33554445:16"
      (showSrcSpanF ss2) `shouldBe` "(((False,1,0,13),16),((False,1,0,13),17))"
      toks2 <- basicTokenise "x"
      (show toks2) `shouldBe` "[((((0,1),(0,2)),ITvarid \"x\"),\"x\")]"
      let (tk3,_ss2') = putToksInCache tk2 (gs2ss ss2) toks2
      (drawTokenCache tk3) `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,21))\n|\n"++
               "+- ((1,1),(11,25))\n|\n"++
               "`- ((13,1),(13,21))(2,0)D\n"++
               "tree TId 1:\n"++
               "((100000013,1),(100000013,21))\n|\n"++
               "+- ((100000013,1),(100000013,16))\n|\n"++
               "+- ((10100000013,16),(10100000013,17))\n|\n"++
               "`- ((100000013,17),(100000013,21))\n"++
               "tree TId 2:\n"++
               "((200000013,16),(200000013,17))\n"

      -- (show $ retrieveTokensInterim $ getTreeFromCache ss2 tk3) `shouldBe` "" 
      -- (show $ getTreeFromCache ss2 tk3) `shouldBe` ""
      (GHC.showRichTokenStream $ retrieveTokensInterim $ getTreeFromCache (gs2ss ss2) tk3) `shouldBe`
                "\n\n\n\n\n\n\n\n\n\n\n\n addthree a b c=x+b+c"

      -- putToksForSpan test/testdata/Demote/WhereIn6.hs:100000013:18:[((((0,1),(0,2)),ITvarid "y"),"y")]
      -- let ss3 = ss2gs ((100000013,18),(100000013,19))
      let ss3 = ss2gs $ (((forestLineToGhcLine $ ForestLine False 1 0 13),18),
                                       ((forestLineToGhcLine $ ForestLine False 1 0 13),19) )

      -- (showGhc ss3) `shouldBe` "f:100000013:18"
      -- (showGhc ss3) `shouldBe` "f:33554445:18"
      (showSrcSpanF ss3) `shouldBe` "(((False,1,0,13),18),((False,1,0,13),19))"
      toks3 <- basicTokenise "y"
      (show toks3) `shouldBe` "[((((0,1),(0,2)),ITvarid \"y\"),\"y\")]"
      let (tk4,_ss3') = putToksInCache tk3 (gs2ss ss3) toks3
      (drawTokenCache tk4) `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,21))\n|\n"++
               "+- ((1,1),(11,25))\n|\n"++
               "`- ((13,1),(13,21))(2,0)D\n"++
               "tree TId 1:\n"++
               "((100000013,1),(100000013,21))\n|\n"++
               "+- ((100000013,1),(100000013,16))\n|\n"++
               "+- ((10100000013,16),(10100000013,17))\n|\n"++
               "`- ((100000013,17),(100000013,21))\n   |\n"++
               "   +- ((100000013,17),(100000013,18))\n   |\n"++
               "   +- ((10100000013,18),(10100000013,19))\n   |\n"++
               "   `- ((100000013,19),(100000013,21))\n"++
               "tree TId 2:\n"++
               "((200000013,16),(200000013,17))\n"++
               "tree TId 3:\n"++
               "((300000013,18),(300000013,19))\n"

      (GHC.showRichTokenStream $ retrieveTokensInterim $ getTreeFromCache (gs2ss ss2) tk4) `shouldBe`
                "\n\n\n\n\n\n\n\n\n\n\n\n addthree a b c=x+y+c"



      -- Context set up, now for the test.
{-
      let sspan2 = ss2gs ((9,1),(9,14))
      (showGhc sspan2) `shouldBe` "f:9:1-13"
      let (forest3,delTree) = removeSrcSpan forest2 (fs sspan2)

      (drawTreeEntry $ insertSrcSpan forest2 (fs sspan2)) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000009,6))\n|\n"++
               "`- ((7,1),(13,25))\n   |\n"++
               "   +- ((7,1),(7,18))\n   |\n"++
               "   +- ((9,1),(9,14))\n   |\n"++
               "   `- ((11,1),(13,25))\n"

      (drawTreeEntry delTree) `shouldBe`
              "((9,1),(9,14))\n" -- removed again


      (invariant forest3) `shouldBe` []
      (drawTreeEntry forest3) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(6,20))\n|\n"++
               "+- ((6,21),(6,41))\n|\n"++
               "+- ((1000007,5),(1000009,6))\n|\n"++
               "`- ((7,1),(13,25))\n   |\n"++
               "   +- ((7,1),(7,18))\n   |\n"++
               "   `- ((11,1),(13,25))\n"

      let toks' = retrieveTokens forest3
      -- (showToks toks') `shouldBe` ""
      (GHC.showRichTokenStream toks') `shouldBe` "module Demote.D1 where\n\n {-demote 'sq' to 'sumSquares'. This refactoring\n  affects module 'D1' and 'C1' -}\n\n sumSquares (x:xs) = sq x + sumSquares xs\n     where\n        sq = x ^ pow\n      \n\n \n\n  sumSquares [] = 0\n\n\n\n pow = 2\n\n main = sumSquares [1..4]\n\n "
-}

    -- ---------------------------------

    it "removes a where token followed by a comment" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/NoWhere.hs"
      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let forest = allocTokens parsed toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      -- removeToksForPos ((6,5),(13,10))
      let sspan = ss2gs ((6,5),(6,10))
      -- (showGhc sspan) `shouldBe` "f:6:5-9"

      let (f2,_f1') = removeSrcSpan forest (gs2f sspan)
      (drawTreeCompact f2) `shouldBe`
               "0:((1,1),(18,1))\n"++
               "1:((1,1),(1,7))\n"++
               "1:((1,8),(1,30))\n"++
               "1:((1,31),(1,36))\n"++
               "1:((3,1),(14,25))\n"++
               "2:((3,1),(3,16))\n"++
               "2:((3,17),(14,25))\n"++
               "3:((3,17),(3,27))\n"++
               "4:((3,17),(3,24))\n"++
               "4:((3,25),(3,27))\n"++
               "3:((3,28),(3,29))\n"++
               "3:((3,30),(5,12))\n"++
               "4:((3,30),(3,32))\n"++
               "4:((4,3),(5,12))(Above FromAlignCol (1,-30) (4,3) (5,12) FromAlignCol (1,-7))\n"++
               "5:((4,3),(5,12))\n"++
               "6:((4,3),(4,30))\n"++
               "7:((4,3),(4,10))\n"++
               "7:((4,14),(4,30))\n"++
               "6:((5,3),(5,12))\n"++
               "7:((5,3),(5,9))\n"++
               "7:((5,10),(5,12))\n"++
               "3:((6,5),(6,10))(1,-2)D\n"++
               "3:((7,8),(13,10))\n"++
               "3:((14,8),(14,25))(Above FromAlignCol (8,-3) (14,8) (14,31) FromAlignCol (2,-30))\n"++
               "4:((14,8),(14,25))\n"++
               "5:((14,8),(14,17))\n"++
               "5:((14,18),(14,25))\n"++
               "6:((14,18),(14,19))\n"++
               "6:((14,20),(14,25))\n"++
               "7:((14,20),(14,21))\n"++
               "7:((14,21),(14,24))\n"++
               "7:((14,24),(14,25))\n"++
               "1:((16,1),(16,30))\n"++
               "2:((16,1),(16,17))\n"++
               "2:((16,17),(16,21))\n"++
               "3:((16,18),(16,20))\n"++
               "2:((16,21),(16,30))\n"++
               "3:((16,21),(16,23))\n"++
               "3:((16,24),(16,30))\n"++
               "1:((17,1),(17,29))\n"++
               "2:((17,1),(17,17))\n"++
               "2:((17,18),(17,29))\n"++
               "3:((17,18),(17,19))\n"++
               "3:((17,20),(17,29))\n"++
               "1:((18,1),(18,1))\n"


  -- ---------------------------------------------

  describe "retrieveTokens" $ do
    it "extracts all the tokens from the leaves of the trees, in order" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let forest' = insertSrcSpan forest (gs2f l)
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "`- ((19,1),(21,14))\n" -- our inserted span

      let toksClean = reverse $ dropWhile isEmpty $ reverse toks

      -- let toks' = retrieveTokensFinal forest'
      -- (show toks') `shouldBe` (show toksClean)
      -- (renderTree forest') `shouldBe` (show toksClean)
      -- (renderTree forest') `shouldBe` (showTokenStream  toksClean)
      (renderLayoutTree forest') `shouldBe` (showTokenStream  $ bypassGHCBug7351 toksClean)

  -- ---------------------------------------------

  describe "retrieveTokensFinal" $ do
    it "extracts all the tokens from the leaves of the trees, in order, realigned" $ do
      pending

  -- ---------------------------------------------

  describe "monotonicLineToks" $ do
    it "makes sure the tokens are in strictly increasing order of line" $ do
      pending

  -- -----------------------------------

  describe "updateTokensForSrcSpan" $ do
    it "replaces the tokens for a given span, inserting the span if needed" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let decl@(GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      -- (showSrcSpan l) `shouldBe` "((19,1),(21,14))"
      -- (showGhc decl) `shouldBe` "TokenTest.foo x y\n"++
      --    "  = do { c <- System.IO.getChar;\n         GHC.Base.return c }"

      let (forest',newSpan,_) = updateTokensForSrcSpan forest (gs2ss l) (take 3 toks)
      -- (showGhc newSpan) `shouldBe` "f:(18,1)-(1000018,22)"
      -- (showGhc $ ss2gs newSpan) `shouldBe` "foo:1073741843:1-22"
      (showSrcSpanF (ss2gs newSpan)) `shouldBe` "(((True,0,0,19),1),((True,0,0,19),23))"

      (drawTreeEntry forest') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "`- ((10000000019,1),(10000000019,23))\n"  -- our inserted span

      -- let toks' = retrieveTokensFinal forest'
      -- (GHC.showRichTokenStream toks') `shouldBe` "module TokenTest where\n\n -- Test new style token manager\n\n bob a b = x\n   where x = 3\n\n bib a b = x\n   where\n     x = 3\n\n\n bab a b =\n   let bar = 3\n   in     b + bar -- ^trailing comment\n\n\n -- leading comment\n module TokenTest where"

      (renderLinesFromLayoutTree forest') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nmodule TokenTest where"


    -- --------------------------------------

    it "replaces the tokens for a given span, and returns all the tokens later" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      let toks' = retrieveTokensInterim tree
      let (forest'',sspan) = addNewSrcSpanAndToksAfter forest' (gs2ss l) (gs2ss l) (PlaceOffset 2 0 2) toks'

      (invariant forest'') `shouldBe` []
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "+- ((19,1),(21,14))\n|\n"++
              "`- ((1000019,1),(1000021,14))\n" -- our inserted span

      (showSrcSpanF (ss2gs sspan)) `shouldBe` "(((False,0,1,19),1),((False,0,1,21),14))"

      -- let Just (GHC.L ln n) = locToName (19, 1) renamed
      -- (showGhc n) `shouldBe` "TokenTest.foo"
      -- let ln = ss2gs ((19,1),(19,4))
      -- (showGhc ln) `shouldBe` "test/testdata/TokenTest.hs:19:1-3"

      let (_tree,toksForOp) = getTokensFor True forest'' (gs2ss l)

      let (GHC.L _ ghcTok,_) = head toks
      let newTok = mkToken ghcTok (19,1) "bbb"

      let newToks = replaceTokNoReAlign toksForOp (19,1) newTok
      (show newToks) `shouldBe`   "[((((18,1),(18,19)),ITlineComment \"-- leading comment\"),\"-- leading comment\"),((((19,1),(19,1)),ITsemi),\"\"),((((19,1),(19,4)),ITmodule),\"bbb\"),((((19,5),(19,6)),ITvarid \"x\"),\"x\"),((((19,7),(19,8)),ITvarid \"y\"),\"y\"),((((19,9),(19,10)),ITequal),\"=\"),((((20,3),(20,5)),ITdo),\"do\"),((((20,6),(20,6)),ITvocurly),\"\"),((((20,6),(20,7)),ITvarid \"c\"),\"c\"),((((20,8),(20,10)),ITlarrow),\"<-\"),((((20,11),(20,18)),ITvarid \"getChar\"),\"getChar\"),((((21,6),(21,6)),ITsemi),\"\"),((((21,6),(21,12)),ITvarid \"return\"),\"return\"),((((21,13),(21,14)),ITvarid \"c\"),\"c\")]"

      let (forest''',_newSpan,_) = updateTokensForSrcSpan forest'' (gs2ss l) newToks

      -- let toksFinal = retrieveTokensFinal forest'''

      -- (show toksFinal) `shouldBe` ""
      -- (GHC.showRichTokenStream toksFinal) `shouldBe` "module TokenTest where\n\n -- Test new style token manager\n\n bob a b = x\n   where x = 3\n\n bib a b = x\n   where\n     x = 3\n\n\n bab a b =\n   let bar = 3\n   in     b + bar -- ^trailing comment\n\n\n -- leading comment\n bbb x y =\n   do c <- getChar\n      return c\n\n -- leading comment\n foo x y =\n   do c <- getChar\n      return c\n "

      (renderLinesFromLayoutTree  forest''') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nbbb x y =\n  do c <- getChar\n     return c\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n"

    -- --------------------------------------

    it "replaces the tokens for a given span, removing any sub tree" $ do
       pending -- "write this"

    -- --------------------------------------

    it "replaces a single token" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/WhereIn4.hs"
      let forest = mkTreeFromTokens toks

      let ss1 = ss2gs ((11,18),(11,22))
      -- (showGhc ss1) `shouldBe` "f:11:18-21"
      (showSrcSpan ss1) `shouldBe` "((11,18),(11,22))"

      ss1Tok <- liftIO $ tokenise (realSrcLocFromTok mkZeroToken) 0 True "sq"
      (show ss1Tok) `shouldBe` "[((((0,0),(0,2)),ITvarid \"sq\"),\"sq\")]"

      let (forest2,_ss1',_) = updateTokensForSrcSpan forest (gs2ss ss1) ss1Tok

      (drawTreeEntry forest2) `shouldBe`
           "((1,1),(18,23))\n|\n"++
           "+- ((1,1),(11,17))\n|\n"++
           "+- ((10000000011,18),(10000000011,20))\n|\n"++
           "`- ((11,23),(18,23))\n"

      -- (show forest2) `shouldBe` ""

      let ss2 = ss2gs ((11,27),(11,31))
      -- (showGhc ss2) `shouldBe` "f:11:27-30"
      (showSrcSpan ss2) `shouldBe` "((11,27),(11,31))"

      ss2Tok <- liftIO $ tokenise (realSrcLocFromTok mkZeroToken) 0 True "sq"
      (show ss2Tok) `shouldBe` "[((((0,0),(0,2)),ITvarid \"sq\"),\"sq\")]"

      let (forest3,_ss2',_) = updateTokensForSrcSpan forest2 (gs2ss ss2) ss2Tok

      (drawTreeEntry forest3) `shouldBe`
           "((1,1),(18,23))\n|\n"++
           "+- ((1,1),(11,17))\n|\n"++
           "+- ((10000000011,18),(10000000011,20))\n|\n"++
           "`- ((11,23),(18,23))\n   |\n"++
           "   +- ((11,23),(11,26))\n   |\n"++
           "   +- ((10000000011,27),(10000000011,29))\n   |\n"++
           "   `- ((11,32),(18,23))\n"

      -- (show forest2) `shouldBe` ""
{-
      (drawTreeEntry $ insertSrcSpan forest2 (fs ss2)) `shouldBe`
           "((1,1),(20,1))\n|\n"++
           "+- ((1,1),(11,17))\n|\n"++
           "+- ((11,18),(1000011,20))\n|\n"++
           "`- ((11,23),(20,1))\n   |\n"++
           "   +- ((11,23),(11,26))\n   |\n"++
           "   +- ((11,27),(11,31))\n   |\n"++
           "   `- ((11,32),(20,1))\n"

      let (f,tree) = getSrcSpanFor forest2 (fs ss2)
      let zf = openZipperToNode tree $ Z.fromTree f

      (show tree) `shouldBe` ""
      (show zf) `shouldBe` ""
-}
      let ss3 = ss2gs ((15,14),(15,17))
      -- (showGhc ss3) `shouldBe` "f:15:14-16"
      (showSrcSpan ss3) `shouldBe` "((15,14),(15,17))"

      ss3Tok <- liftIO $ tokenise (realSrcLocFromTok mkZeroToken) 0 True "p"
      (show ss3Tok) `shouldBe` "[((((0,0),(0,1)),ITvarid \"p\"),\"p\")]"

      let (forest4,_ss3',_) = updateTokensForSrcSpan forest3 (gs2ss ss3) ss3Tok
      -- (show forest4) `shouldBe` ""
      (drawTreeEntry forest4) `shouldBe`
           "((1,1),(18,23))\n|\n"++
           "+- ((1,1),(11,17))\n|\n"++
           "+- ((10000000011,18),(10000000011,20))\n|\n"++
           "`- ((11,23),(18,23))\n   |\n"++
           "   +- ((11,23),(11,26))\n   |\n"++
           "   +- ((10000000011,27),(10000000011,29))\n   |\n"++
           "   `- ((11,32),(18,23))\n      |\n"++
           "      +- ((11,32),(15,14))\n      |\n"++
           "      +- ((10000000015,14),(10000000015,15))\n      |\n"++
           "      `- ((17,1),(18,23))\n"

    -- --------------------------------------

    it "replaces a single token,without disturbing adjacent ones" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/WhereIn6.hs"

      -- let forest = mkTreeFromTokens toks
      let tk = initTokenCache toks

      -- removeToksForPos ((13,1),(13,21))
      let sspan = ss2gs ((13,1),(13,21))
      -- (showGhc sspan) `shouldBe` "f:13:1-20"

      -- (showToks toks) `shouldBe` ""
      let tk2 = removeToksFromCache tk (gs2ss sspan)
      (drawTokenCache tk2) `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,21))\n|\n"++
               "+- ((1,1),(11,25))\n|\n"++
               "`- ((13,1),(13,21))(2,0)D\n"++
               "tree TId 1:\n"++
               "((100000013,1),(100000013,21))\n"

      -- let ss2 = ss2gs ((100000013,16),(100000013,17))
      let ss2 = ss2gs $ (((forestLineToGhcLine $ ForestLine False 1 0 13),16),
                                       ((forestLineToGhcLine $ ForestLine False 1 0 13),17) )
      -- (showGhc ss2) `shouldBe` "f:100000013:16"
      -- (showGhc ss2) `shouldBe` "f:33554445:16"
      (showSrcSpanF ss2) `shouldBe` "(((False,1,0,13),16),((False,1,0,13),17))"
      (show (gs2f ss2)) `shouldBe` "(((ForestLine False 1 0 13),16),((ForestLine False 1 0 13),17))"
      let (tokStartPos,tokEndPos) = forestSpanToSimpPos (gs2f ss2)
      (tokStartPos,tokEndPos) `shouldBe` ((13,16),(13,17))

      let f2 = getTreeFromCache (gs2ss ss2) tk2
      (GHC.showRichTokenStream $ retrieveTokensInterim f2) `shouldBe`
               "\n\n\n\n\n\n\n\n\n\n\n\n addthree a b c=a+b+c"
      toks2 <- basicTokenise "x"
      (show toks2) `shouldBe` "[((((0,1),(0,2)),ITvarid \"x\"),\"x\")]"
      --

      let z = openZipperToSpan (gs2f ss2) $ Z.fromTree f2
      let toksz = retrieveTokensInterim $ Z.tree z
      (GHC.showRichTokenStream toksz) `shouldBe` "\n\n\n\n\n\n\n\n\n\n\n\n addthree a b c=a+b+c"
      let (_begin,middle,_end) = splitToks (tokStartPos,tokEndPos) toksz
      -- (show begin) `shouldBe` ""
      (show middle) `shouldBe` "[((((13,16),(13,17)),ITvarid \"a\"),\"a\")]"
      -- (show end) `shouldBe` ""

      let (startLoc,endLoc) = startEndLocIncComments' toksz (tokStartPos,tokEndPos)
      (startLoc,endLoc) `shouldBe` ((13,16),(13,17))

      let fss = insertSrcSpan f2 (gs2f ss2)
      (drawTreeEntry fss) `shouldBe`
               "((100000013,1),(100000013,21))\n|\n"++
               "+- ((13,1),(13,16))\n|\n"++
               "+- ((100000013,16),(100000013,17))\n|\n"++
               "`- ((13,17),(13,21))\n"

      let (forest',_tree) = getSrcSpanFor f2 (gs2f ss2)
      (drawTreeEntry forest') `shouldBe`
               "((100000013,1),(100000013,21))\n|\n"++
               "+- ((13,1),(13,16))\n|\n"++
               "+- ((100000013,16),(100000013,17))\n|\n"++
               "`- ((13,17),(13,21))\n"
      --

      let (f3,_ss1',_) = updateTokensForSrcSpan f2 (gs2ss ss2) toks2

      (drawTreeEntry f3) `shouldBe`
               "((100000013,1),(100000013,21))\n|\n"++
               "+- ((13,1),(13,16))\n|\n"++
               "+- ((10200000013,16),(10200000013,17))\n|\n"++
               "`- ((13,17),(13,21))\n"
      -- (show f3) `shouldBe` ""
      -- (GHC.showRichTokenStream $ retrieveTokensFinal f3) `shouldBe`
      --          "\n\n\n\n\n\n\n\n\n\n\n\n addthree a b c=x+b+c"

      -- "error" `shouldBe` "reinstate the following"
      -- TODO: the following generates a very large number of '\n'
      -- chars. Suspect the line is not being cleanly extracted from
      -- the tree.
      -- let pprVal = retrieveTokensPpr f3
      -- pprVal `shouldBe`
      --   [PprText 13 1 Original "addthree a b c=",
      --    PprText 13 16 Original "x",
      --    PprText 13 17 Original "+b+c"]
      (renderLinesFromLayoutTree f3) `shouldBe`
                "\n\n\n\n\n\n\n\n\n\n\n\naddthree a b c=x+b+c"

  -- ---------------------------------------------

  describe "replaceTokenForSrcSpan" $ do
    it "replaces a single token in a given span, without disturbing the tree" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      -- let forest = mkTreeFromTokens toks
      let forest = allocTokens (GHC.pm_parsed_source $ GHC.tm_parsed_module t) toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let decl@(GHC.L l _) = head decls
      -- let Just (GHC.L _ln n) = locToName (13, 1) renamed
      -- let (_,mm,_) = splitToks ((13,1),(13,3)) toks
      -- (show mm) `shouldBe` ""
      let (_,[_,_,(GHC.L _ n,_)],_) = splitToks ((13,1),(13,3)) toks
      (show n) `shouldBe` "ITvarid \"bab\""
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"
      -- (showGhc decl) `shouldBe` "TokenTest.foo x y\n  = do { c <- System.IO.getChar;\n         GHC.Base.return c }"
      -- (showGhc n) `shouldBe` "TokenTest.bab"

      -- let newTok@(GHC.L lt _,_) = markToken $ newNameTok False l n
      let newTok@(GHC.L lt _,_) = markToken $ mkToken n (fst $ gs2ss l) "bab"

      (show newTok) `shouldBe` "((((19,1),(19,4)),ITvarid \"bab\"),\"bab\")"
      let forest' = replaceTokenForSrcSpan forest (gs2ss lt) newTok


      -- let toks' = retrieveTokensFinal forest'
      -- (GHC.showRichTokenStream toks') `shouldBe` "module TokenTest where\n\n -- Test new style token manager\n\n bob a b = x\n   where x = 3\n\n bib a b = x\n   where\n     x = 3\n\n\n bab a b =\n   let bar = 3\n   in     b + bar -- ^trailing comment\n\n\n -- leading comment\n bab x y =\n   do c <- getChar\n      return c\n\n\n\n\n "

      (renderLinesFromLayoutTree forest') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nbab x y =\n  do c <- getChar\n     return c\n\n\n\n\n"

    -- ---------------------------------

    it "replaces a single token in an added span" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/DupDef/Dd1.hs"
      let parsed = (GHC.pm_parsed_source $ GHC.tm_parsed_module t)
      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""
      -- let f1 = mkTreeFromTokens toks
      let f1 = allocTokens parsed toks


      (invariant f1) `shouldBe` []

      let ss1 = ss2gs ((4,1),(4,19))
      let (f2,_toks1) = getTokensFor True f1 (gs2ss ss1)

      let ss2 = ss2gs ((3,1),(3,31))
      let (f3,toks2) = getTokensFor True f2 (gs2ss ss2)


      -- putDeclToksAfterSpan test/testdata/DupDef/Dd1.hs:4:1-18:("(((False,0,0,4),1),((False,0,0,4),19))",PlaceAbsCol 2 1 0,[((((3,1),(3,1)),ITvocurly),""),((((3,1),(3,9)),ITvarid "toplevel"),"toplevel"),((((3,10),(3,12)),ITdcolon),"::"),((((3,13),(3,20)),ITconid "Integer"),"Integer"),((((3,21),(3,23)),ITrarrow),"->"),((((3,24),(3,31)),ITconid "Integer"),"Integer")])
      let (f4,ss4) = addToksAfterSrcSpan f3 (gs2ss ss1) (PlaceAbsCol 2 1 0) toks2
{-
      (drawTreeEntry f4) `shouldBe`
              "((1,1),(32,18))\n|\n"++
              "+- ((1,1),(3,31))\n|  |\n"++
              "|  +- ((1,1),(1,24))\n|  |\n"++
              "|  `- ((3,1),(3,31))\n|\n"++
              "+- ((4,1),(4,19))\n|\n"++
              "+- ((1000006,1),(1000006,31))\n|\n"++
              "`- ((6,1),(32,18))\n"
-}
      (showSrcSpan $ ss2gs ss4) `shouldBe` "((1048582,1),(1048582,31))"
      (showSrcSpanF $ ss2gs ss4) `shouldBe` "(((False,0,1,6),1),((False,0,1,6),31))"

      -- renamePN.worker entry:l=(((False,0,1,6),1),((False,0,1,6),9))
      let ss5 = ss2gs $
                        (((forestLineToGhcLine $ ForestLine False 0 1 6),1),
                         ((forestLineToGhcLine $ ForestLine False 0 1 6),9) )

      -- (markToken $ newNameTok useQual' l newName)
      let newName = mkTestGhcName 1 Nothing "bar2"
      let newTok = markToken $ newNameTok False ss5 newName
      (show newTok) `shouldBe` "((((6,1),(6,5)),ITvarid \"bar2\"),\"bar2\")"

--
      let  (GHC.L _tl _,_) = newTok
      let z = openZipperToSpan (gs2f ss5) $ Z.fromTree f4

      (drawTreeEntry $ Z.tree z) `shouldBe`
                    "((1000006,1),(1000006,31))\n"
--
      let f5 = replaceTokenForSrcSpan f4 (gs2ss ss5) newTok
{-
      (drawTreeEntry f5) `shouldBe`
              "((1,1),(32,18))\n|\n"++
              "+- ((1,1),(3,31))\n|  |\n"++
              "|  +- ((1,1),(1,24))\n|  |\n"++
              "|  `- ((3,1),(3,31))\n|\n"++
              "+- ((4,1),(4,19))\n|\n"++
              "+- ((1000006,1),(1000006,31))\n|\n"++
              "`- ((6,1),(32,18))\n"
-}
      -- let toks' = retrieveTokensFinal f5
      -- (GHC.showRichTokenStream toks') `shouldBe` "module DupDef.Dd1 where\n\n toplevel :: Integer -> Integer\n toplevel x = c * x\n\n bar2 :: Integer -> Integerc,d :: Integer\n c = 7\n d = 9\n\n -- Pattern bind\n tup :: (Int, Int)\n h :: Int\n t :: Int\n tup@(h,t) = head $ zip [1..10] [3..ff]\n   where\n     ff :: Int\n     ff = 15\n\n data D = A | B String | C\n\n ff y = y + zz\n   where\n     zz = 1\n\n l z =\n   let\n     ll = 34\n   in ll + z\n\n dd q = do\n   let ss = 5\n   return (ss + q)\n\n "

      -- (showGhc $ layoutTreeToSourceTree f5) `shouldBe` ""
      (renderLinesFromLayoutTree f5) `shouldBe` "module DupDef.Dd1 where\n\ntoplevel :: Integer -> Integer\ntoplevel x = c * x\n\nbar2 :: Integer -> Integerc,d :: Integer\nc = 7\nd = 9\n\n-- Pattern bind\ntup :: (Int, Int)\nh :: Int\nt :: Int\ntup@(h,t) = head $ zip [1..10] [3..ff]\n  where\n    ff :: Int\n    ff = 15\n\ndata D = A | B String | C\n\nff y = y + zz\n  where\n    zz = 1\n\nl z =\n  let\n    ll = 34\n  in ll + z\n\ndd q = do\n  let ss = 5\n  return (ss + q)\n\n"

  -- ---------------------------------------------

{- Note: this function will not come across to haskell-token-utils
  describe "posToSrcSpan" $ do
    it "Converts a simple position to a SrcSpan in the context of a forest" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      let sspan = ss2gs ((1,2),(3,5))
      (showGhc sspan) `shouldBe` "f:(1,2)-(3,4)"
      (showSrcSpan sspan) `shouldBe` "((1,2),(3,5))"
-}
  -- ---------------------------------------------

  describe "ghcLineToForestLine" $ do
    it "converts a GHC line to a ForestLine" $ do
      (ghcLineToForestLine         34) `shouldBe` ForestLine False 0  0 34
      (ghcLineToForestLine   0x100022) `shouldBe` ForestLine False 0  1 34
      (ghcLineToForestLine  0xbe00022) `shouldBe` ForestLine False 5 30 34
      (ghcLineToForestLine 0x49400022) `shouldBe` ForestLine True  4 20 34
      (ghcLineToForestLine 0x40100022) `shouldBe` ForestLine True  0  1 34

  describe "forestLineToGhcLine" $ do
    it "converts a ForestLine value to a GHC line" $ do
      (hex $ forestLineToGhcLine $ ForestLine False 0  0 34) `shouldBe`       "0x22"
      (hex $ forestLineToGhcLine $ ForestLine False 0  1 34) `shouldBe`   "0x100022"
      (hex $ forestLineToGhcLine $ ForestLine False 5 30 34) `shouldBe`  "0xbe00022"
                                        -- 0xbe = 101 11110
      (hex $ forestLineToGhcLine $ ForestLine True  4 20 34) `shouldBe` "0x49400022"
                                   -- 0x494 = 1 00100 10100
      (hex $ forestLineToGhcLine $ ForestLine True  0  1 34) `shouldBe` "0x40100022"

  -- ---------------------------------------------

  describe "ForestLine Ord" $ do
    it "implements Ord for ForestLine" $ do
      compare (ForestLine False 0 0 1) (ForestLine False 0 0 3) `shouldBe` LT
      compare (ForestLine False 0 0 3) (ForestLine False 0 1 3) `shouldBe` LT
      compare (ForestLine False 0 1 1) (ForestLine False 0 2 3) `shouldBe` LT
      compare (ForestLine False 0 9 3) (ForestLine False 0 0 4) `shouldBe` LT
      compare (ForestLine True  0 0 6) (ForestLine False 0 0 4) `shouldBe` GT

      compare (ForestLine False 0 7 3) (ForestLine False 0 7 3) `shouldBe` EQ

      compare (ForestLine False 0 0 4) (ForestLine False 0 0 3) `shouldBe` GT

  -- ---------------------------------------------

  describe "insertNodeAfter" $ do
    it "adds a new SrcSpan after a specified one in the forest" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      -- let (ghcl,_c) = getGhcLoc l
      let (ghcl,_c) = fst (gs2ss l)
      let (ForestLine ch tr v lin) = ghcLineToForestLine ghcl
      let newSpan' = insertForestLineInSrcSpan (ForestLine ch tr (v+1) lin) l

      let toksNew = take 3 toks
      let newNode = Node (Entry (gs2f newSpan') NoChange toksNew) []
      -- let newNode = Node (Entry l toks) []

      -- let toks' = retrieveTokens tree
      let forest'' = insertNodeAfter tree newNode forest'
      (invariant forest'') `shouldBe` []
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "+- ((19,1),(21,14))\n|\n"++
              "`- ((1000019,1),(1000021,14))\n" -- our inserted span


      -- let toksFinal = retrieveTokensFinal forest''
      (GHC.showRichTokenStream toksNew) `shouldBe` "module TokenTest where"
      (renderLinesFromLayoutTree forest'') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\nmodule TokenTest where"

  -- ---------------------------------------------

  describe "addNewSrcSpanAndToksAfter" $ do
    it "adds a new SrcSpan after an existing one in the forest 0" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      -- (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      let toks' = retrieveTokensInterim tree
      let (forest'',sspan) = addNewSrcSpanAndToksAfter forest' (gs2ss l) (gs2ss l) (PlaceOffset 2 0 2) toks'
      (invariant forest'') `shouldBe` []
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "+- ((19,1),(21,14))\n|\n"++
              "`- ((1000019,1),(1000021,14))\n" -- our inserted span

      (showSrcSpanF $ ss2gs sspan) `shouldBe` "(((False,0,1,19),1),((False,0,1,21),14))"

      -- let toksFinal = retrieveTokensFinal forest''
      -- (showToks toksFinal) `shouldBe` ""
      (renderLinesFromLayoutTree forest'') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n"

  -- ---------------------------------------------

  describe "addToksAfterSrcSpan" $ do
    it "adds a new SrcSpan after an existing one in the forest 1" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      let toks' = retrieveTokensInterim tree
      let (forest'',sspan) = addToksAfterSrcSpan forest' (gs2ss l) (PlaceOffset 2 0 2) toks'
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "+- ((19,1),(21,14))\n|\n"++
              "`- ((1000024,1),(1000026,14))\n" -- our inserted span

      (showSrcSpanF $ ss2gs sspan) `shouldBe` "(((False,0,1,24),1),((False,0,1,26),14))"
      (invariant forest'') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest''
      (renderLinesFromLayoutTree forest'') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n"

    -- ---------------------------------

    it "adds a new SrcSpan after an existing one in the forest 2" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head $ tail decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(13,1)-(15,16)"
      let l = ss2gs ((13,1),(15,17))
      (showSrcSpan l) `shouldBe` "((13,1),(15,17))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      let toks' = retrieveTokensInterim tree
      let (forest'',sspan) = addToksAfterSrcSpan forest' (gs2ss l) (PlaceOffset 2 0 2) toks'
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(10,10))\n|\n"++
              "+- ((13,1),(15,17))\n|\n"++
              "+- ((1000017,1),(1000019,17))\n|\n"++ -- our inserted span
              "`- ((19,1),(21,14))\n"
      (showSrcSpanF $ ss2gs sspan) `shouldBe` "(((False,0,1,17),1),((False,0,1,19),17))"
      (invariant forest'') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest''
      (renderLinesFromLayoutTree forest'') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n\n\n\n"

    -- ---------------------------------

    it "adds a new SrcSpan after an existing one in the forest, with an indent" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(19,1)-(21,13)"
      let l = ss2gs ((19,1),(21,14))
      (showSrcSpan l) `shouldBe` "((19,1),(21,14))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      let toks' = retrieveTokensInterim tree
      let (forest'',sspan) = addToksAfterSrcSpan forest' (gs2ss l) (PlaceOffset 2 4 2) toks'
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(15,17))\n|\n"++
              "+- ((19,1),(21,14))\n|\n"++
              "`- ((1000024,5),(1000026,18))\n" -- our inserted span

      (showSrcSpanF $ ss2gs sspan) `shouldBe` "(((False,0,1,24),5),((False,0,1,26),18))"
      (invariant forest'') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest''
      (renderLinesFromLayoutTree forest'')  `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n    -- leading comment\n    foo x y =\n      do c <- getChar\n         return c\n"

    -- ---------------------------------

    it "adds a new SrcSpan after an existing one in the forest, in a where clause" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/Where.hs"

      (GHC.showRichTokenStream toks) `shouldBe`
            "module LiftToToplevel.Where where\n\n anotherFun 0 y = sq y\n      where sq x = x^2\n "

      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let forest = allocTokens parsed toks


      -- let forest = mkTreeFromTokens toks

      let sspan = ss2gs ((4,12),(4,22))
      newToks <- liftIO $ basicTokenise "abc = 3"

      let (forest'',sspan') = addToksAfterSrcSpan forest (gs2ss sspan) (PlaceOffset 1 0 1) newToks



      (showSrcSpanF $ss2gs sspan') `shouldBe` "(((False,0,1,5),6),((False,0,1,5),13))"
      (invariant forest'') `shouldBe` []
      -- (show forest'') `shouldBe` ""

--
{-
      let (forest',tree) = getSrcSpanForDeep forest (gs2f sspan)
      let zf = openZipperToNodeDeep tree $ Z.fromTree forest'
      (show $ Z.tree zf) `shouldBe` "foo"
-}
--
{-
      let toksFinal = retrieveTokensFinal forest''
      (GHC.showRichTokenStream toksFinal) `shouldBe` ""
-}
      let pprFinal = retrieveLinesFromLayoutTree forest''
      show pprFinal `shouldBe`
         "[(1 1 0 SOriginal ONone \"module LiftToToplevel.Where where\"),"++
          "(3 1 0 SOriginal ONone \"anotherFun 0 y = sq y\"),"++
          "(4 6 0 SOriginal OGroup \"where sq x = x^2\"),"++
          "(5 6 0 SWasAdded OGroup \"abc = 3\")]"


      -- NOTE: alignment is out, the tokens are supposed to have an
      -- offset when they go in.
      (renderLines pprFinal) `shouldBe` "module LiftToToplevel.Where where\n\nanotherFun 0 y = sq y\n     where sq x = x^2\n     abc = 3"

    -- ---------------------------------

    it "adds a new SrcSpan after an existing one, with an indent based on whole prior line" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/MoveDef/Demote.hs"
      let forest = mkTreeFromTokens toks

      --  removeToksForPos ((7,1),(7,6))
      let sspan = ss2gs ((7,1),(7,6))
      let (forest',_delTree) = removeSrcSpan forest (gs2f sspan)
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"
      (invariant forest') `shouldBe` []


      --  putToksAfterPos ((4,14),(4,19))
      let sspan' = ss2gs ((4,14),(4,19))

      let finsert = insertSrcSpan forest' (gs2f sspan')
      (drawTreeEntry finsert) `shouldBe`
               "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|  |\n"++
               "|  +- ((1,1),(4,13))\n|  |\n"++
               "|  `- ((4,14),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"

      let newToks = take 3 toks
      let (forest'',sspan'') = addToksAfterSrcSpan finsert (gs2ss sspan') (PlaceOffset 2 4 2) newToks
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|  |\n"++
               "|  +- ((1,1),(4,13))\n|  |\n"++
               "|  +- ((4,14),(4,19))\n|  |\n"++
               "|  `- ((1000006,5),(1000006,32))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"

      (showSrcSpanF $ ss2gs sspan'') `shouldBe` "(((False,0,1,6),5),((False,0,1,6),32))"
      -- (invariant forest'') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest''
      -- (showToks toksFinal) `shouldBe` ""
      (renderLinesFromLayoutTree forest'') `shouldBe` "module MoveDef.Demote where\n\ntoplevel :: Integer -> Integer\ntoplevel x = c * x\n\n    module MoveDef.Demote where\n\nd = 9\n\n\n"

    -- ---------------------------------

    it "adds a new SrcSpan after an existing one, with an indent catering for comments" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/WhereIn5.hs"
      let forest = mkTreeFromTokens toks

      -- removeToksForPos ((14,1),(14,6))
      let sspan = ss2gs ((14,1),(14,6))
      let (forest',_delTree) = removeSrcSpan forest (gs2f sspan)
      (drawTreeEntry forest') `shouldBe`
               "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(12,26))\n|\n"++
               "+- ((14,1),(14,6))(2,-5)D\n|\n"++
               "`- ((16,1),(17,23))\n"
      (invariant forest') `shouldBe` []

      -- addLocalDecl entered:newSource="[pow=2\n\n]"
      -- putToksAfterPos ((11,16),(12,26)) at PlaceIndent 1 0 2
      let sspan' = ss2gs ((11,16),(12,26))

      let finsert = insertSrcSpan forest' (gs2f sspan')
      (drawTreeEntry finsert) `shouldBe`
               "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(12,26))\n|  |\n"++
               "|  +- ((1,1),(11,15))\n|  |\n"++
               "|  `- ((11,16),(12,26))\n|\n"++
               "+- ((14,1),(14,6))(2,-5)D\n|\n"++
               "`- ((16,1),(17,23))\n"

      newToks <- liftIO $ basicTokenise "pow=2\n\n"
      let (forest'',sspan'') = addToksAfterSrcSpan finsert (gs2ss sspan') (PlaceIndent 1 0 2) newToks
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(12,26))\n|  |\n"++
               "|  +- ((1,1),(11,15))\n|  |\n"++
               "|  +- ((11,16),(12,26))\n|  |\n"++
               "|  `- ((1000014,16),(1000014,21))\n|\n"++
               "+- ((14,1),(14,6))(2,-5)D\n|\n"++
               "`- ((16,1),(17,23))\n"

      (showSrcSpanF $ ss2gs sspan'') `shouldBe` "(((False,0,1,14),16),((False,0,1,14),21))"
      -- (invariant forest'') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest''
      -- (showToks toksFinal) `shouldBe` ""
      (renderLinesFromLayoutTree forest'') `shouldBe` "module Demote.WhereIn5 where\n\n--A definition can be demoted to the local 'where' binding of a friend declaration,\n--if it is only used by this friend declaration.\n\n--Demoting a definition narrows down the scope of the definition.\n--In this example, demote the top level 'pow' to 'sq'\n--This example aims to test demoting a function/pattern binding multi-levels.\n\nsumSquares x y = sq x + sq y\n         where sq 0=0\n               sq z=z^pow {-There \nis a comment-}\n               pow=2\n\nanotherFun 0 y = sq y\n     where  sq x = x^2\n\n"

    -- ---------------------------------

    it "adds a new SrcSpan after deleting toks 1" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/MoveDef/Demote.hs"
      let forest = mkTreeFromTokens toks

      --  removeToksForPos ((7,1),(7,6))
      let sspan = ss2gs ((7,1),(7,6))
      let (forest',_) = removeSrcSpan forest (gs2f sspan)
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"
      (invariant forest') `shouldBe` []

      --  putToksAfterPos ((4,14),(4,19))
      let newToks = take 3 toks
      let sspan' = ss2gs ((4,14),(4,19))
      let position = PlaceOffset 1 4 2

      let finsert = insertSrcSpan forest' (gs2f sspan')
      (drawTreeEntry finsert) `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|  |\n"++
               "|  +- ((1,1),(4,13))\n|  |\n"++
               "|  `- ((4,14),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"

      let (f,_t) = getSrcSpanFor forest' (gs2f sspan')
      (drawTreeEntry f) `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|  |\n"++
               "|  +- ((1,1),(4,13))\n|  |\n"++
               "|  `- ((4,14),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"

      let (forest'',newSpan) = addToksAfterSrcSpan forest' (gs2ss sspan') position newToks

      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|  |\n"++
               "|  +- ((1,1),(4,13))\n|  |\n"++
               "|  +- ((4,14),(4,19))\n|  |\n"++
               "|  `- ((1000005,5),(1000005,32))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"

      (showSrcSpanF $ ss2gs newSpan) `shouldBe` "(((False,0,1,5),5),((False,0,1,5),32))"
      (invariant forest'') `shouldBe` []

    -- ---------------------------------

    it "adds a new SrcSpan after an existing one, in a subtree." $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/D1.hs"

      let forest = mkTreeFromTokens toks

      -- removeToksForPos ((9,1),(9,14))
      let sspan = ss2gs ((9,1),(9,14))
      let (forest',_delTree) = removeSrcSpan forest (gs2f sspan)
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(7,18))\n|\n"++
               "+- ((9,1),(9,14))(2,-13)D\n|\n"++
               "`- ((11,1),(13,25))\n"
      (invariant forest') `shouldBe` []

      -- putToksForSpan test/testdata/Demote/D1.hs:6:21-24:[((((0,1),(0,3)),ITvarid "sq"),"sq")]
      let sspan2 = ss2gs ((6,21),(6,25))
      let tok2 = head $ drop 12 toks
      let (f2,_newSpan,_oldTree) = updateTokensForSrcSpan forest' (gs2ss sspan2) [tok2]
      (drawTreeEntry f2) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(7,18))\n|  |\n"++
               "|  +- ((1,1),(6,20))\n|  |\n"++
               "|  +- ((10000000006,21),(10000000006,23))\n|  |\n"++
               "|  `- ((6,26),(7,18))\n|\n"++
               "+- ((9,1),(9,14))(2,-13)D\n|\n"++
               "`- ((11,1),(13,25))\n"
      -- putToksAfterPos ((6,21),(6,41)) at PlaceIndent 1 4 2
      let sspan3 = ss2gs ((6,21),(6,41))
      let newToks = take 20 $ drop 5 toks

      -- let (fss,_tree) = getSrcSpanFor f2 (gs2f sspan3)
      let z = openZipperToSpan (gs2f sspan3) $ Z.fromTree f2
      -- retrieveTokens $ Z.toTree z
      -- (show $ Z.toTree z) `shouldBe` ""
      -- (drawTreeEntry $ Z.tree z) `shouldBe` ""
      -- (show z) `shouldBe` ""

      let childrenAsZ = go [] (Z.firstChild z)
           where
            go acc Nothing = acc
            go acc (Just zz) = go (acc ++ [zz]) (Z.next zz)
      (show $ map treeStartEnd $ map Z.tree childrenAsZ) `shouldBe`
             "[(((ForestLine False 0 0 1),1),((ForestLine False 0 0 6),20)),"++
              "(((ForestLine True 0 0 6),21),((ForestLine True 0 0 6),23)),"++
              "(((ForestLine False 0 0 6),26),((ForestLine False 0 0 7),18))]"

      let contains zn = (startPos <= nodeStart && endPos >= nodeEnd)
            where
              (startPos,endPos) = treeStartEnd $ Z.tree zn
              (nodeStart,nodeEnd) = (gs2f sspan3)

      (show $ filter contains childrenAsZ) `shouldBe` "[]"

--
      let z1 = openZipperToSpan (gs2f sspan3) $ Z.fromTree f2
      let (b1,m1,e1) = splitSubtree (Z.tree z1) (gs2f sspan3)
      (show (map treeStartEnd b1,map treeStartEnd m1,map treeStartEnd e1)) `shouldBe`
              "([(((ForestLine False 0 0 1),1),((ForestLine False 0 0 6),20))],"++
               "[(((ForestLine True 0 0 6),21),((ForestLine True 0 0 6),23)),"++
                "(((ForestLine False 0 0 6),26),((ForestLine False 0 0 7),18))],"++
               "[])"
      let (_b2,_m2,_e2) = splitSubToks (head m1) (gs2f sspan3)
      -- (show (b2,m2,e2)) `shouldBe` ""
--
      let fss = insertSrcSpan f2 (gs2f sspan3)
      (drawTreeEntry fss) `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(7,18))\n|  |\n"++
               "|  +- ((1,1),(6,20))\n|  |\n"++
               "|  +- ((6,21),(6,41))\n|  |  |\n"++
               "|  |  +- ((10000000006,21),(10000000006,23))\n|  |  |\n"++
               "|  |  `- ((6,26),(6,41))\n|  |\n"++
               "|  `- ((7,1),(7,18))\n|\n"++
               "+- ((9,1),(9,14))(2,-13)D\n|\n"++
               "`- ((11,1),(13,25))\n"


      let (forest'',sspan'') = addToksAfterSrcSpan f2 (gs2ss sspan3) (PlaceOffset 1 4 2) newToks
      (drawTreeEntry forest'') `shouldBe`
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(7,18))\n|  |\n"++
               "|  +- ((1,1),(6,20))\n|  |\n"++
               "|  +- ((6,21),(6,41))\n|  |  |\n"++
               "|  |  +- ((10000000006,21),(10000000006,23))\n|  |  |\n"++
               "|  |  `- ((6,26),(6,41))\n|  |\n"++
               "|  +- ((1000007,5),(1000010,7))\n|  |\n"++
               "|  `- ((7,1),(7,18))\n|\n"++
               "+- ((9,1),(9,14))(2,-13)D\n|\n"++
               "`- ((11,1),(13,25))\n"

      (showSrcSpanF $ ss2gs sspan'') `shouldBe` "(((False,0,1,7),5),((False,0,1,10),7))"
      -- (invariant forest'') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest''
      -- (showToks toksFinal) `shouldBe` ""
      (renderLinesFromLayoutTree forest'') `shouldBe` "module Demote.D1 where\n\n{-demote 'sq' to 'sumSquares'. This refactoring\n  affects module 'D1' and 'C1' -}\n\nsumSquares (x:xs) = sq   + sumSquares xs\n    sumSquares (x:xs) = sq x + sumSquares xs\n    sumSquares [] = 0\n\n    sq\n\nsumSquares [] = 0\n\npow = 2\n\nmain = sumSquares [1..4]\n\n"

    -- ---------------------------------

    it "adds a new SrcSpan after the last one" $ do
      (_t,toks)  <- parsedFileGhc "./test/testdata/TypeUtils/JustImports.hs"

      let forest = mkTreeFromTokens toks

      let toksTree = retrieveTokensInterim forest
      (GHC.showRichTokenStream toksTree) `shouldBe` "module JustImports where\n\n import Data.Maybe\n "
      (show toksTree) `shouldBe`
           "[((((1,1),(1,7)),ITmodule),\"module\"),"++
           "((((1,8),(1,19)),ITconid \"JustImports\"),\"JustImports\"),"++
           "((((1,20),(1,25)),ITwhere),\"where\"),"++
           "((((3,1),(3,1)),ITvocurly),\"\"),"++
           "((((3,1),(3,7)),ITimport),\"import\"),"++
           "((((3,8),(3,18)),ITqconid (\"Data\",\"Maybe\")),\"Data.Maybe\"),"++
           "((((4,1),(4,1)),ITsemi),\"\")]"

      (show toks) `shouldBe` (show toksTree)

      -- putToksAfterPos ((3,8),(3,18)) at PlaceOffset 1 0 1
      let sspan = ss2gs ((3,8),(3,18))
      newToks <- basicTokenise "import Data.List"

      {-
      let (startLoc,endLoc) = startEndLocIncComments' toks ((3,8),(3,18))
      (show (startLoc,endLoc)) `shouldBe` "((3,8),(3,18))"

      let f1 = insertSrcSpan forest (fs sspan)
      (drawTreeEntry f1) `shouldBe`
              "((1,1),(3,18))\n|\n"++
              "+- ((1,1),(3,7))\n|\n"++
              "`- ((3,8),(3,18))\n"

      let (fwithspan,tree) = getSrcSpanFor forest (gs2f sspan)
      (drawTreeEntry fwithspan) `shouldBe`
              "((1,1),(3,18))\n|\n"++
              "+- ((1,1),(3,7))\n|\n"++
              "`- ((3,8),(3,18))\n"
      -}

      let (forest',_sspan') = addToksAfterSrcSpan forest (gs2ss sspan) (PlaceOffset 1 0 1) newToks
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(3,18))\n|\n"++
              "+- ((1,1),(3,7))\n|\n"++
              "+- ((3,8),(3,18))\n|\n"++
              "`- ((1000004,1),(1000004,17))\n"

      (invariant forest') `shouldBe` []

      -- let toksFinal = retrieveTokensFinal forest'
      (renderLinesFromLayoutTree forest') `shouldBe` "module JustImports where\n\nimport Data.Maybe\nimport Data.List"

    -- ---------------------------------

    it "adds a SrcSpan after deleting, without extra tokens" $ do
      (_t,toks)  <- parsedFileGhc "./test/testdata/Demote/LetIn1.hs"
      let forest = mkTreeFromTokens toks

      -- removeToksForPos ((12,22),(12,27))
      let sspan = ss2gs ((12,22),(12,27))
      let (forest',_) = removeSrcSpan forest (gs2f sspan)
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(11,32))\n|\n"++
               "+- ((12,22),(12,27))(1,-9)D\n|\n"++
               "`- ((13,18),(17,23))\n"
      (invariant forest') `shouldBe` []

      -- putToksAfterPos ((11,27),(11,32)) at PlaceOffset 1 4 2
      newToks <- basicTokenise "where\n   pow=2\n"
      (show newToks) `shouldBe`
             "[((((0,1),(0,6)),ITwhere),\"where\"),"++
              "((((1,4),(1,4)),ITvocurly),\"\"),"++
              "((((1,4),(1,7)),ITvarid \"pow\"),\"pow\"),"++
              "((((1,7),(1,8)),ITequal),\"=\"),"++
              "((((1,8),(1,9)),ITinteger 2),\"2\"),"++
              "((((2,1),(2,1)),ITvccurly),\"\")]" -- ++AZ++ odd, ok.
      let sspan' = ss2gs ((11,27),(11,32))
      let position = PlaceOffset 1 4 2

      --
      let finsert = insertSrcSpan forest' (gs2f sspan')
      (drawTreeEntry finsert) `shouldBe`
              "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(11,32))\n|  |\n"++
               "|  +- ((1,1),(11,27))\n|  |\n"++
               "|  `- ((11,27),(11,32))\n|\n"++
               "+- ((12,22),(12,27))(1,-9)D\n|\n"++
               "`- ((13,18),(17,23))\n"

      let (fwithspan,_t) = getSrcSpanFor forest' (gs2f sspan')
      (drawTreeEntry fwithspan) `shouldBe`
              "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(11,32))\n|  |\n"++
               "|  +- ((1,1),(11,27))\n|  |\n"++
               "|  `- ((11,27),(11,32))\n|\n"++
               "+- ((12,22),(12,27))(1,-9)D\n|\n"++
               "`- ((13,18),(17,23))\n"

      let z = openZipperToSpan (gs2f sspan') $ Z.fromTree fwithspan
      let prevToks = retrievePrevLineToks z
      let prevToks'' = limitPrevToks prevToks (gs2ss sspan')
      (show prevToks'') `shouldBe`
          "RT [((((11,29),(11,32)),ITvarid \"pow\"),\"pow\"),"++
              "((((11,28),(11,29)),ITvarsym \"^\"),\"^\"),"++
              "((((11,27),(11,28)),ITvarid \"z\"),\"z\"),"++
              "((((11,26),(11,27)),ITequal),\"=\"),"++
              "((((11,25),(11,26)),ITvarid \"z\"),\"z\"),"++
              "((((11,22),(11,24)),ITvarid \"sq\"),\"sq\"),"++
              "((((11,22),(11,22)),ITsemi),\"\")]"
      let toks'' = reIndentToks position (unReverseToks prevToks'') newToks
      (show toks'') `shouldBe`
             "[((((12,26),(12,31)),ITwhere),\"where\"),((((13,29),(13,29)),ITvocurly),\"\"),((((13,29),(13,32)),ITvarid \"pow\"),\"pow\"),((((13,32),(13,33)),ITequal),\"=\"),((((13,33),(13,34)),ITinteger 2),\"2\"),((((14,26),(14,26)),ITvccurly),\"\"),((((15,1),(15,1)),ITsemi),\"\")]"
      --
      let (forest'',newSpan) = addToksAfterSrcSpan forest' (gs2ss sspan') position newToks

      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(17,23))\n|\n"++
               "+- ((1,1),(11,32))\n|  |\n"++
               "|  +- ((1,1),(11,27))\n|  |\n"++
               "|  +- ((11,27),(11,32))\n|  |\n"++
               "|  `- ((1000012,26),(1000013,34))\n|\n"++
               "+- ((12,22),(12,27))(1,-9)D\n|\n"++
               "`- ((13,18),(17,23))\n"

      (showSrcSpanF $ ss2gs newSpan) `shouldBe` "(((False,0,1,12),26),((False,0,1,13),34))"
      (invariant forest'') `shouldBe` []
      (renderLinesFromLayoutTree forest'') `shouldBe` "module Demote.LetIn1 where\n\n--A definition can be demoted to the local 'where' binding of a friend declaration,\n--if it is only used by this friend declaration.\n\n--Demoting a definition narrows down the scope of the definition.\n--In this example, demote the local  'pow' to 'sq'\n--This example also aims to test the demoting a local declaration in 'let'.\n\nsumSquares x y = let sq 0=0\n                     sq z=z^pow\n                         where\n                            pow=2\n                         \n\n                 in sq x + sq y\n\n\nanotherFun 0 y = sq y\n     where  sq x = x^2\n\n  "

    -- ---------------------------------

    it "adds a parameter, chasing a bug in MoveDef" $ do
      (t,toks)  <- parsedFileGhc "./test/testdata/LiftToToplevel/Zmapq.hs"

      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let forest = allocTokens parsed toks

{-
addParamsToDecls (pn,paramPNames,modifyToks)=(g, [f], True)
updateToks test/testdata/LiftToToplevel/Zmapq.hs:6:32:"(((False,0,0,6),32),((False,0,0,6),33))"
putToksForSpan test/testdata/LiftToToplevel/Zmapq.hs:6:32:
  (((False,0,0,6),32),((False,0,0,6),33))
  [((((0,1),(0,2)),IToparen),"("),
   ((((0,2),(0,3)),ITvarid "g"),"g"),((((0,4),(0,5)),ITvarid "f"),"f"),((((0,5),(0,6)),ITcparen),")")]
-}
      toks1 <- liftIO $ basicTokenise "(g f)"
      (show toks1) `shouldBe`
         "[((((0,1),(0,2)),IToparen),\"(\"),((((0,2),(0,3)),ITvarid \"g\"),\"g\"),"++
          "((((0,4),(0,5)),ITvarid \"f\"),\"f\"),((((0,5),(0,6)),ITcparen),\")\")]"

      let sspan1 = ss2gs ((6,32),(6,33))
      let (f2,_sspan1,_forest) = updateTokensForSrcSpan forest (gs2ss sspan1) (map markToken toks1)
      (invariant f2) `shouldBe` []

{-
addParamtoMatch:l=test/testdata/LiftToToplevel/Zmapq.hs:6:3-35

getToksForSpan test/testdata/LiftToToplevel/Zmapq.hs:6:5-6:
  ("(((False,0,0,6),5),((False,0,0,6),7))",[((((6,5),(6,7)),ITvarid "z'"),"z'")])

putToksForSpan test/testdata/LiftToToplevel/Zmapq.hs:6:5-6:
  (((False,0,0,6),5),((False,0,0,6),7))
    [((((6,6),(6,7)),ITvarid "f"),"f")]
-}
      toks2 <- liftIO $ basicTokenise "\n\n\n\n\n\n     f"
      (show toks2) `shouldBe` "[((((6,6),(6,7)),ITvarid \"f\"),\"f\")]"

      let sspan2 = ss2gs ((6,5),(6,7))
      let (f3,_sspan2,_f2) = updateTokensForSrcSpan f2 (gs2ss sspan2) (map markToken toks2)
      -- ++AZ++ removed when haskell-token-utils. ?? (showSrcSpanF $  sspan2) `shouldBe` "(((True,0,0,6),5),((True,0,0,6),6))"
      (invariant f3) `shouldBe` []

      let ppr3 = retrieveLinesFromLayoutTree f3
      (renderLines ppr3) `shouldBe`
         "module LiftToTopLevel.Zmapq where\n"++
         "\n"++
         "-- | Apply a generic query to the immediate children.\n"++
         "-- zmapQ :: GenericQ b -> Zipper a -> [b]\n"++
         "zmapQ f z = reverse $ downQ [] g z where\n"++
         "  g f  = query f z' : leftQ [] (g f)z'\n"++
         "\n"++
         "\n"++
         "downQ = undefined\n"++
         "query = undefined\n"++
         "leftQ = undefined\n"

{-
putToksAfterSpan test/testdata/LiftToToplevel/Zmapq.hs:6:5-6:
  (((False,0,0,6),5),((False,0,0,6),7)) at PlaceAdjacent:
    [(((6,5),(6,7)),ITvarid "z'","z'")]
addParamsToParentAndLiftedDecl: liftedDecls done
-}
      toks3 <- liftIO $ basicTokenise "\n\n\n\n\n\n    z'"
      (show toks3) `shouldBe` "[((((6,5),(6,7)),ITvarid \"z'\"),\"z'\")]"

      -- let sspan3 = ss2gs ((6,5),(6,7))
      let sspan3 = ss2gs ((6,5),(6,6))
      let (f4,_) = addToksAfterSrcSpan f3 (gs2ss sspan3) PlaceAdjacent (map markToken toks3)
      (invariant f4) `shouldBe` []

      -- (drawTreeCompact f4) `shouldBe`
      --    "0:((1,1),(12,1))\n"

      let ppr4 = retrieveLinesFromLayoutTree f4
      (renderLines ppr4) `shouldBe`
         "module LiftToTopLevel.Zmapq where\n"++
         "\n"++
         "-- | Apply a generic query to the immediate children.\n"++
         "-- zmapQ :: GenericQ b -> Zipper a -> [b]\n"++
         "zmapQ f z = reverse $ downQ [] g z where\n"++
         "  g f z'= query f z' : leftQ [] (g f)z'\n"++
         "\n"++
         "\n"++
         "downQ = undefined\n"++
         "query = undefined\n"++
         "leftQ = undefined\n"

    -- ---------------------------------

    it "adds a SrcSpan, chasing a bug in MoveDef" $ do
      (_t,toks)  <- parsedFileGhc "./test/testdata/MoveDef/Md1.hs"
      let forest = mkTreeFromTokens toks

      -- getToksForSpan test/testdata/MoveDef/Md1.hs:24:5-10:("(((False,0,0,24),5),((False,0,0,24),11))",
      let sspan1 = ss2gs ((24,5),(24,11))
      let (f1,_toks1) = getTokensFor True forest (gs2ss sspan1)

      (drawTreeEntry f1) `shouldBe`
              "((1,1),(40,17))\n|\n"++
              "+- ((1,1),(23,8))\n|\n"++
              "+- ((24,5),(24,11))\n|\n"++
              "`- ((26,1),(40,17))\n"
      (invariant f1) `shouldBe` []

      -- removeToksForPos ((24,5),(24,11))
      let (f2,_) = removeSrcSpan f1 (gs2f sspan1)
      (drawTreeEntry f2) `shouldBe`
              "((1,1),(40,17))\n|\n"++
               "+- ((1,1),(23,8))\n|\n"++
               "+- ((24,5),(24,11))(2,-10)D\n|\n"++
               "`- ((26,1),(40,17))\n"
      (invariant f2) `shouldBe` []

      -- removeToksForPos ((23,3),(23,8))
      let sspan3 = ss2gs ((23,3),(23,8))
      let (f3,_) = removeSrcSpan f2 (gs2f sspan3)
      (drawTreeEntry f3) `shouldBe`
              "((1,1),(40,17))\n|\n"++
               "+- ((1,1),(23,8))\n|  |\n"++
               "|  +- ((1,1),(22,14))\n|  |\n"++
               "|  `- ((23,3),(23,8))(1,-3)D\n|\n"++
               "+- ((24,5),(24,11))(2,-10)D\n|\n"++
               "`- ((26,1),(40,17))\n"
      (invariant f3) `shouldBe` []


      -- Context set, time for test

      -- putDeclToksAfterSpan test/testdata/MoveDef/Md1.hs:(22,1)-(24,10):("(((False,0,0,22),1),((False,0,0,24),11))",PlaceOffset 2 0 2,[((((1,6),(1,8)),ITvarid "zz"),"zz"),((((1,9),(1,10)),ITequal),"="),((((1,11),(1,12)),ITinteger 1),"1")])
      newToks <- basicTokenise "\n     zz = 1"
      (show newToks) `shouldBe` "[((((1,6),(1,8)),ITvarid \"zz\"),\"zz\"),((((1,9),(1,10)),ITequal),\"=\"),((((1,11),(1,12)),ITinteger 1),\"1\")]"

      let sspan4 = ss2gs ((22,1),(24,11))

--
      let z = openZipperToSpan (gs2f sspan4) $ Z.fromTree f3
      (drawTreeEntry $ Z.tree z) `shouldBe`
              "((1,1),(40,17))\n|\n"++
               "+- ((1,1),(23,8))\n|  |\n"++
               "|  +- ((1,1),(22,14))\n|  |\n"++
               "|  `- ((23,3),(23,8))(1,-3)D\n|\n"++
               "+- ((24,5),(24,11))(2,-10)D\n|\n"++
               "`- ((26,1),(40,17))\n"

      -- (show $ subForest (Z.tree z)) `shouldBe` ""

      -- let (b1,m1,e1) = doSplitTree (Z.tree z) (fs sspan4)
      let (b1,m1,e1) = splitSubtree (Z.tree z) (gs2f sspan4)
      (show (map treeStartEnd b1,map treeStartEnd m1,map treeStartEnd e1)) `shouldBe`
              "([],"++

              "[(((ForestLine False 0 0 1),1),((ForestLine False 0 0 23),8)),"++
               "(((ForestLine False 0 0 24),5),((ForestLine False 0 0 24),11))],"++

              "[(((ForestLine False 0 0 26),1),((ForestLine False 0 0 40),17))])"

      let (b2,m2,e2) = splitSubtree (head m1) (gs2f sspan4)
      (show (map treeStartEnd b2,map treeStartEnd m2,map treeStartEnd e2)) `shouldBe`
              "([],"++
              "[(((ForestLine False 0 0 1),1),((ForestLine False 0 0 22),14)),"++
               "(((ForestLine False 0 0 23),3),((ForestLine False 0 0 23),8))],"++
              "[])"

      let (Node (Entry ss _ toks2) _) = head m2

      (containsStart ss (gs2f sspan4),containsEnd ss (gs2f sspan4)) `shouldBe` (True,False)

      let (sspanStart,_sspanEnd) = gs2f sspan4

      let (_,toksb,toksm) = splitToks (forestSpanToSimpPos (nullForestPos,sspanStart)) toks2
      (show (head toksb,last toksb)) `shouldBe`
               "(((((1,1),(1,7)),ITmodule),\"module\"),"++
               "((((21,14),(21,17)),ITconid \"Int\"),\"Int\"))"
      (show (head toksm,last toksm)) `shouldBe`
               "(((((22,1),(22,1)),ITsemi),\"\"),"++
               "((((22,12),(22,14)),ITvarid \"zz\"),\"zz\"))"

      let (b3,m3,e3) = splitSubToks (head m2) (gs2f sspan4)
      (show (map treeStartEnd b3,map treeStartEnd m3,map treeStartEnd e3)) `shouldBe`
              "([(((ForestLine False 0 0 1),1),((ForestLine False 0 0 21),17))],"++
              -- ++AZ++ How does this get to (24,11), input only goes to (23,8)?
               "[(((ForestLine False 0 0 22),1),((ForestLine False 0 0 22),14))],"++
               "[])"

      -- TODO: span is deleted twice. Deal with it.
      let f3' = insertSrcSpan f3 (gs2f sspan4)
      (drawTreeEntry f3') `shouldBe`
              "((1,1),(40,17))\n|\n"++
              "+- ((1,1),(21,17))\n|\n"++
              "+- ((22,1),(24,11))\n|  |\n"++
              "|  +- ((22,1),(22,14))\n|  |\n"++
              "|  +- ((23,3),(23,8))(1,-3)D\n|  |\n"++
              "|  `- ((24,5),(24,11))(2,-10)D\n|\n"++
              "`- ((26,1),(40,17))\n"
      (invariant f3') `shouldBe` []

      let (fwithspan,tree) = getSrcSpanFor f3 (gs2f sspan4)
      (drawTreeEntry fwithspan) `shouldBe`
              "((1,1),(40,17))\n|\n"++
              "+- ((1,1),(21,17))\n|\n"++
              "+- ((22,1),(24,11))\n|  |\n"++
              "|  +- ((22,1),(22,14))\n|  |\n"++
              "|  +- ((23,3),(23,8))(1,-3)D\n|  |\n"++
              "|  `- ((24,5),(24,11))(2,-10)D\n|\n"++
              "`- ((26,1),(40,17))\n"

      let toks'' = placeToksForSpan fwithspan (gs2ss sspan4) tree (PlaceOffset 2 0 2) newToks
      let (startPos,endPos) = nonCommentSpan toks''
      let newSpan = ss2gs (startPos,endPos)
      -- (showGhc newSpan) `shouldBe` "f:24:1-6"

      let (forest',tree') = getSrcSpanFor f3 (gs2f sspan4)
      -- (show tree') `shouldBe` ""

      -- let (forest',newSpan') = addNewSrcSpanAndToksAfter f3 sspan4 newSpan (PlaceOffset 2 0 2) newToks
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(40,17))\n|\n"++
              "+- ((1,1),(21,17))\n|\n"++
              "+- ((22,1),(24,11))\n|  |\n"++
              "|  +- ((22,1),(22,14))\n|  |\n"++
              "|  +- ((23,3),(23,8))(1,-3)D\n|  |\n"++
              "|  `- ((24,5),(24,11))(2,-10)D\n|\n"++
              "`- ((26,1),(40,17))\n"


      -- let (ghcl,_c) = getGhcLoc newSpan
      let (ghcl,_c) = fst (gs2ss newSpan)
      let (ForestLine ch tr v l) = ghcLineToForestLine ghcl
      let newSpan' = insertForestLineInSrcSpan (ForestLine ch tr (v+1) l) newSpan
      let toks' = placeToksForSpan forest' (gs2ss sspan4) tree' (PlaceOffset 2 0 2) newToks
      let newNode = Node (Entry (gs2f newSpan') NoChange toks') []
      (show newNode) `shouldBe` "Node {rootLabel = Entry (((ForestLine False 0 1 24),1),((ForestLine False 0 1 24),7)) NoChange [((((24,1),(24,3)),ITvarid \"zz\"),\"zz\"),((((24,4),(24,5)),ITequal),\"=\"),((((24,6),(24,7)),ITinteger 1),\"1\"),((((25,1),(25,1)),ITsemi),\"\")], subForest = []}"

      let forest'' = insertNodeAfter tree' newNode forest'
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(40,17))\n|\n"++
              "+- ((1,1),(21,17))\n|\n"++
              "+- ((22,1),(24,11))\n|  |\n"++
              "|  +- ((22,1),(22,14))\n|  |\n"++
              "|  +- ((23,3),(23,8))(1,-3)D\n|  |\n"++
              "|  `- ((24,5),(24,11))(2,-10)D\n|\n"++
              "+- ((1000024,1),(1000024,7))\n|\n"++
              "`- ((26,1),(40,17))\n"
--

      let (f4,_newSpan4) = addToksAfterSrcSpan f3 (gs2ss sspan4) (PlaceOffset 2 0 2) newToks
      (drawTreeEntry f4) `shouldBe`
              "((1,1),(40,17))\n|\n"++
              "+- ((1,1),(21,17))\n|\n"++
              "+- ((22,1),(24,11))\n|  |\n"++
              "|  +- ((22,1),(22,14))\n|  |\n"++
              "|  +- ((23,3),(23,8))(1,-3)D\n|  |\n"++
              "|  `- ((24,5),(24,11))(2,-10)D\n|\n"++
              "+- ((1000024,1),(1000024,7))\n|\n"++
              "`- ((26,1),(40,17))\n"
      (invariant f4) `shouldBe` []

  -- ---------------------------------------------

  describe "retrievePrevLineToks" $ do
    it "Retrieves the previous non-empty line tokens from an open zipper" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/MoveDef/Demote.hs"
      let forest = mkTreeFromTokens toks

      --  removeToksForPos ((7,1),(7,6))
      let sspan = ss2gs ((7,1),(7,6))
      let (forest',_) = removeSrcSpan forest (gs2f sspan)
      (drawTreeEntry forest') `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"
      (invariant forest') `shouldBe` []

      --  putToksAfterPos ((4,14),(4,19))
      let sspan' = ss2gs ((4,14),(4,19))

      let finsert = insertSrcSpan forest' (gs2f sspan')
      (drawTreeEntry finsert) `shouldBe`
              "((1,1),(8,6))\n|\n"++
               "+- ((1,1),(4,19))\n|  |\n"++
               "|  +- ((1,1),(4,13))\n|  |\n"++
               "|  `- ((4,14),(4,19))\n|\n"++
               "+- ((7,1),(7,6))(1,-5)D\n|\n"++
               "`- ((8,1),(8,6))\n"
      -- (invariant forest'') `shouldBe` []

      -- Now we have the test case set up. We want prior toks from
      -- ((4,14),(4,19))
      let tspan = ss2gs ((4,14),(4,19))

      let z = openZipperToSpan (gs2f tspan) $ Z.fromTree finsert

      let toksPrev = retrievePrevLineToks z

      (GHC.showRichTokenStream (unReverseToks toksPrev)) `shouldBe` "module MoveDef.Demote where\n\n toplevel :: Integer -> Integer\n toplevel x = c * x"

  -- -------------------------------------------------------------------

  describe "reAlignOneLine" $ do
    it "does nothing if token lengths have not changed" $ do
      let toks = [mkToken GHC.ITsemi (1,1) "v1"
                 ,mkToken GHC.ITsemi (1,4) "v2"
                 ,mkToken GHC.ITsemi (1,7) "v3"
                 ]
      (showToks toks) `shouldBe` "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,6),((((1,4),(1,6)),ITsemi),\"v2\")),((1,7),(1,9),((((1,7),(1,9)),ITsemi),\"v3\"))]"
      (showToks $ reAlignOneLine toks) `shouldBe` "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,6),((((1,4),(1,6)),ITsemi),\"v2\")),((1,7),(1,9),((((1,7),(1,9)),ITsemi),\"v3\"))]"

    ------------------------------------

    it "spaces tokens out if one increases length" $ do
      let toks' = [mkToken GHC.ITsemi (1, 1) "v1"
                  ,mkToken GHC.ITsemi (1, 4) "v2"
                  ,mkToken GHC.ITsemi (1, 7) "v3"
                  ,mkToken GHC.ITsemi (1,10) "v4"
                  ]
      let tv2@(GHC.L l _,_) = head $ tail toks'
      let t2 = markToken $ matchTokenPos tv2 $ newNameTok False l (mkTestGhcName 1 Nothing "v2long")
      let toks = head toks':t2:drop 2 toks'
      (showToks toks) `shouldBe`
           "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,6),((((1,4),(1,6)),ITvarid \"v2long\"),\"v2long\")),((1,7),(1,9),((((1,7),(1,9)),ITsemi),\"v3\")),((1,10),(1,12),((((1,10),(1,12)),ITsemi),\"v4\"))]"

      (showToks $ reAlignOneLine toks) `shouldBe`
            "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,10),((((1,4),(1,10)),ITvarid \"v2long\"),\"v2long\")),((1,11),(1,13),((((1,11),(1,13)),ITsemi),\"v3\")),((1,14),(1,16),((((1,14),(1,16)),ITsemi),\"v4\"))]"

    ------------------------------------

    it "spaces tokens out if one increases length, maintaining the gap" $ do
      let toks' = [mkToken GHC.ITsemi (1, 1) "v1"
                  ,mkToken GHC.ITsemi (1, 4) "v2"
                  ,mkToken GHC.ITsemi (1,10) "v3"
                  ,mkToken GHC.ITsemi (1,13) "v4"
                  ]
      let tv2@(GHC.L l _,_) = head $ tail toks'
      let t2 = markToken $ matchTokenPos tv2 $ newNameTok False l (mkTestGhcName 1 Nothing "v2long")
      let toks = head toks':t2:drop 2 toks'
      (showToks toks) `shouldBe`
           "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,6),((((1,4),(1,6)),ITvarid \"v2long\"),\"v2long\")),((1,10),(1,12),((((1,10),(1,12)),ITsemi),\"v3\")),((1,13),(1,15),((((1,13),(1,15)),ITsemi),\"v4\"))]"

      (showToks $ reAlignOneLine toks) `shouldBe`
            "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,10),((((1,4),(1,10)),ITvarid \"v2long\"),\"v2long\")),((1,14),(1,16),((((1,14),(1,16)),ITsemi),\"v3\")),((1,17),(1,19),((((1,17),(1,19)),ITsemi),\"v4\"))]"

    ------------------------------------

    it "spaces tokens out if one decreases length" $ do
      let toks' = [mkToken GHC.ITsemi (1, 1) "v1"
                  ,mkToken GHC.ITsemi (1, 4) "v2"
                  ,mkToken GHC.ITsemi (1, 7) "v3"
                  ,mkToken GHC.ITsemi (1,10) "v4"
                  ]
      let tv2@(GHC.L l _,_) = head $ tail toks'
      let t2 = markToken $ matchTokenPos tv2 $ newNameTok False l (mkTestGhcName 1 Nothing "v")
      let toks = head toks':t2:drop 2 toks'
      (showToks toks) `shouldBe`
           "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,6),((((1,4),(1,6)),ITvarid \"v\"),\"v\")),((1,7),(1,9),((((1,7),(1,9)),ITsemi),\"v3\")),((1,10),(1,12),((((1,10),(1,12)),ITsemi),\"v4\"))]"

      (showToks $ reAlignOneLine toks) `shouldBe`
            "[((1,1),(1,3),((((1,1),(1,3)),ITsemi),\"v1\")),((1,4),(1,5),((((1,4),(1,5)),ITvarid \"v\"),\"v\")),((1,6),(1,8),((((1,6),(1,8)),ITsemi),\"v3\")),((1,9),(1,11),((((1,9),(1,11)),ITsemi),\"v4\"))]"


  -- ---------------------------------------------

  describe "invariant 1" $ do
    it "checks that a tree with empty tokens and empty subForest fails" $ do
      (invariant $ Node (Entry nonNullSpan NoChange ([]::[PosToken])) []) `shouldBe` ["FAIL: exactly one of toks or subforest must be empty: Node (Entry ((0,0),(1,0)) []) []"]

    -- -----------------------
    it "checks that a tree nonempty tokens and empty subForest passes" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      (invariant $ Node (Entry nonNullSpan NoChange (take 3 toks)) []) `shouldBe` []

    -- -----------------------
    it "checks that a tree with nonempty tokens and nonempty subForest fails" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"

      (invariant (Node (Entry (simpPosToForestSpan ((1,1),(1,7))) NoChange (take 1 toks)) [emptyTree])) `shouldBe`
             ["FAIL: exactly one of toks or subforest must be empty: Node (Entry ((1,1),(1,7)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]) [Node (Entry ((0,0),(1,0)) []) []]",
              "FAIL: subForest start and end does not match entry: Node (Entry ((1,1),(1,7)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]) [Node (Entry ((0,0),(1,0)) []) []]",
              "FAIL: exactly one of toks or subforest must be empty: Node (Entry ((0,0),(1,0)) []) []"]

    -- -----------------------
    it "checks that a tree with empty tokens and nonempty subForest passes" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let tree@(Node (Entry sspan _ _) _) = mkTreeFromTokens toks

      (invariant (Node (Entry sspan NoChange []) [tree])) `shouldBe` []

    -- -----------------------
    it "checks the subtrees too" $ do
      (_t,_toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"

      (invariant (Node (Entry nonNullSpan NoChange []) [emptyTree])) `shouldBe` ["FAIL: exactly one of toks or subforest must be empty: Node (Entry ((0,0),(1,0)) []) []"]

  -- ---------------------------------------------

  describe "invariant 2" $ do
    it "checks that a the subree fully includes the parent" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let tree1@(Node (Entry sspan _ _) _)  = mkTreeFromTokens toks
      let tree2@(Node (Entry sspan2 _ _) _) = mkTreeFromTokens (tail toks)
      let tree3@(Node (Entry sspan3 _ _) _) = mkTreeFromTokens (take 10 toks)
      let tree4 = mkTreeFromTokens (drop 10 toks)
      (showTree tree1) `shouldBe` "Node (Entry ((1,1),(21,14)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]..[((26,1),(26,1),((((26,1),(26,1)),ITsemi),\"\"))]) []"
      (showTree tree2) `shouldBe` "Node (Entry ((1,8),(21,14)) [((1,8),(1,17),((((1,8),(1,17)),ITconid \"TokenTest\"),\"TokenTest\"))]..[((26,1),(26,1),((((26,1),(26,1)),ITsemi),\"\"))]) []"
      (showTree tree3) `shouldBe` "Node (Entry ((1,1),(5,12)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]..[((5,11),(5,12),((((5,11),(5,12)),ITvarid \"x\"),\"x\"))]) []"

      (invariant (Node (Entry sspan2 NoChange []) [tree1])) `shouldBe` ["FAIL: subForest start and end does not match entry: Node (Entry ((1,8),(21,14)) []) [Node (Entry ((1,1),(21,14)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]..[((26,1),(26,1),((((26,1),(26,1)),ITsemi),\"\"))]) []]"]

      (invariant (Node (Entry sspan3 NoChange []) [tree1])) `shouldBe` ["FAIL: subForest start and end does not match entry: Node (Entry ((1,1),(5,12)) []) [Node (Entry ((1,1),(21,14)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]..[((26,1),(26,1),((((26,1),(26,1)),ITsemi),\"\"))]) []]"]

      (invariant (Node (Entry sspan NoChange []) [tree3,tree4])) `shouldBe` []

    -- -----------------------------------------------------------------

    it "checks that a the subree is in span order" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let (Node (Entry sspan _ _) _) = mkTreeFromTokens toks
      let tree1 = mkTreeFromTokens (take 10 toks)
      let tree2 = mkTreeFromTokens (take 10 $ drop 10 toks)
      let tree3 = mkTreeFromTokens (take 10 $ drop 20 toks)
      let tree4 = mkTreeFromTokens (drop 30 toks)

      (showForestSpan $ treeStartEnd tree1) `shouldBe` "((1,1),(5,12))"
      (showForestSpan $ treeStartEnd tree2) `shouldBe` "((6,3),(8,8))"
      (showForestSpan $ treeStartEnd tree3) `shouldBe` "((8,9),(13,4))"
      (showForestSpan $ treeStartEnd tree4) `shouldBe` "((13,5),(21,14))"

      (invariant (Node (Entry sspan NoChange []) [tree1,tree2,tree3,tree4])) `shouldBe` []
      (invariant (Node (Entry sspan NoChange []) [tree1,tree3,tree2,tree4])) `shouldBe` ["FAIL: subForest not in order: ((ForestLine False 0 0 13),4) not < ((ForestLine False 0 0 6),3):Node (Entry ((1,1),(21,14)) []) [Node (Entry ((1,1),(5,12)) [((1,1),(1,7),((((1,1),(1,7)),ITmodule),\"module\"))]..[((5,11),(5,12),((((5,11),(5,12)),ITvarid \"x\"),\"x\"))]) [],Node (Entry ((8,9),(13,4)) [((8,9),(8,10),((((8,9),(8,10)),ITequal),\"=\"))]..[((13,1),(13,4),((((13,1),(13,4)),ITvarid \"bab\"),\"bab\"))]) [],Node (Entry ((6,3),(8,8)) [((6,3),(6,8),((((6,3),(6,8)),ITwhere),\"where\"))]..[((8,7),(8,8),((((8,7),(8,8)),ITvarid \"b\"),\"b\"))]) [],Node (Entry ((13,5),(21,14)) [((13,5),(13,6),((((13,5),(13,6)),ITvarid \"a\"),\"a\"))]..[((26,1),(26,1),((((26,1),(26,1)),ITsemi),\"\"))]) []]"]

  -- ---------------------------------------------

  describe "invariant 3" $ do
    it "checks that all ForestSpans have the same version for start and end" $ do
      pending -- "write this test (and function)"

  -- ---------------------------------------------

  describe "mkTreeFromTokens" $ do
    it "creates a tree from an empty token list" $ do
      (show $ mkTreeFromTokens ([]::[PosToken])) `shouldBe` "Node {rootLabel = Entry (((ForestLine False 0 0 0),0),((ForestLine False 0 0 0),0)) NoChange [], subForest = []}"

    -- -----------------------

    it "creates a tree from a list of tokens" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let toks' = take 2 $ drop 5 toks
      let tree = mkTreeFromTokens toks'
      (show toks') `shouldBe` "[((((5,1),(5,4)),ITvarid \"bob\"),\"bob\"),((((5,5),(5,6)),ITvarid \"a\"),\"a\")]"
      (show tree) `shouldBe` "Node {rootLabel = Entry (((ForestLine False 0 0 5),1),((ForestLine False 0 0 5),6)) NoChange [((((5,1),(5,4)),ITvarid \"bob\"),\"bob\"),((((5,5),(5,6)),ITvarid \"a\"),\"a\")], subForest = []}"

  -- ---------------------------------------------

  describe "splitSubToks" $ do
    it "splits sub tokens" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Case/D.hs"
      let forest = mkTreeFromTokens toks

      let startPos = (ForestLine False 0 0 6,16)
      let endPos   = (ForestLine False 0 0 8,19)

      -- test/testdata/Case/D.hs:(6,16)-(8,19)}
      -- (showGhc $ f2gs (startPos,endPos)) `shouldBe` "foo:(6,16)-(8,18)"
      -- (show $ f2gs (startPos,endPos)) `shouldBe` "foo:(6,16)-(8,18)"
      (show $ f2ss (startPos,endPos)) `shouldBe` "((6,16),(8,19))"

      let (_b,m,_e) = splitSubToks forest (startPos,endPos)
      (show m) `shouldBe` "[Node {rootLabel = Entry (((ForestLine False 0 0 6),16),((ForestLine False 0 0 8),19)) NoChange "++
          "[((((6,16),(6,18)),ITdo),\"do\"),((((7,13),(7,37)),ITlineComment \"-- This is an odd result\"),\"-- This is an odd result\"),((((8,13),(8,13)),ITvocurly),\"\"),((((8,13),(8,16)),ITvarid \"bob\"),\"bob\"),((((8,17),(8,18)),ITvarid \"x\"),\"x\")], subForest = []}]"

  -- ---------------------------------------------

  describe "invariant 5" $ do
    it "checks that a tree with a null SrcSpan fails" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let toks' = take 2 $ drop 5 toks
      (invariant $ Node (Entry nullForestSpan NoChange toks') []) `shouldBe` ["FAIL: null SrcSpan in tree: Node (Entry ((0,0),(0,0)) [((5,1),(5,4),((((5,1),(5,4)),ITvarid \"bob\"),\"bob\")),((5,5),(5,6),((((5,5),(5,6)),ITvarid \"a\"),\"a\"))]) []"]

  -- ---------------------------------------------

{- NOTE: This test mus remain/be moved back to HaRe
  describe "syncAST" $ do
    it "updates an AST and a tree to have the same SrcSpan structure" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      let forest = mkTreeFromTokens toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let decl@(GHC.L l _) = head $ drop 1 decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(13,1)-(15,16)"
      let l = ss2gs ((13,1),(15,17))
      (showSrcSpan l) `shouldBe` "((13,1),(15,17))"

      let (forest',tree) = getSrcSpanFor forest (gs2f l)

      let toks' = retrieveTokensInterim tree
      let (forest'',sspan) = addNewSrcSpanAndToksAfter forest' (gs2ss l) (gs2ss l) (PlaceOffset 2 0 2) toks'
      (invariant forest'') `shouldBe` []
      (drawTreeEntry forest'') `shouldBe`
              "((1,1),(21,14))\n|\n"++
              "+- ((1,1),(10,10))\n|\n"++
              "+- ((13,1),(15,17))\n|\n"++
              "+- ((1000013,1),(1000015,17))\n|\n"++ -- our inserted span
              "`- ((19,1),(21,14))\n"
      (showSrcSpanF $ ss2gs sspan) `shouldBe` "(((False,0,1,13),1),((False,0,1,15),17))"

      let decl' = syncAST decl (ss2f sspan) --  forest''
          forest''' = forest''

      (showGhc decl') `shouldBe` "TokenTest.bab a b = let bar = 3 in b GHC.Num.+ bar"
      (take 90 $ SYB.showData SYB.Renamer 0 decl') `shouldBe` "\n(L {foo:(1048589,1)-(1048591,16)} \n (FunBind \n  (L {test/testdata/TokenTest.hs:1048589:1-"

      -- let toksFinal = retrieveTokensFinal forest'''
      (renderLinesFromLayoutTree forest''') `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n\n\n\n"
-}

  -- ---------------------------------------------

{- NOTE: This test mus remain/be moved back to HaRe. Maybe.
  describe "indentDeclToks" $ do
    it "adds a positive offset to a decl and toks" $ do
      (t,toks) <- parsedFileLayoutIn2
      let forest = mkTreeFromTokens toks

      let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed

      let Just decl@(GHC.L l _) = (locToExp (8,13) (12,43) renamed) :: Maybe (GHC.Located (GHC.HsExpr GHC.Name))

      (showGhc l) `shouldBe` "test/testdata/Renaming/LayoutIn2.hs:(8,14)-(12,42)"
      (showSrcSpan l) `shouldBe` "((8,14),(12,43))"

      (showGhc decl) `shouldBe`
           "case list of {\n"++
           "  (1 : xs) -> 1\n"++
           "  (2 : xs)\n"++
           "    | x GHC.Classes.< 10 -> 4\n"++
           "    where\n"++
           "        x = GHC.List.last xs\n"++
           "  otherwise -> 12 }"
      let (GHC.L _ (GHC.HsCase expr (GHC.MatchGroup matches typ))) = decl
      (showGhc expr) `shouldBe` "list"

      let (GHC.L m1l _) = head matches
      (showSrcSpan m1l) `shouldBe` "((8,28),(8,39))"
      let (m1,forest') = indentDeclToks syncAST (head matches) forest 4

      -- let toks' = retrieveTokensInterim forest'
      (invariant forest') `shouldBe` []
      (drawTreeEntry forest') `shouldBe`
              "((10000000001,1),(10000000012,43))\n|\n"++
              "+- ((1,1),(8,26))\n|\n"++
              "+- ((8,32),(8,43))\n|\n"++ -- Indented by 4
              "`- ((10,28),(12,43))\n"

      -- let toksFinal = retrieveTokensFinal forest'
      (renderLinesFromLayoutTree forest') `shouldBe` "module LayoutIn2 where\n\n--Layout rule applies after 'where','let','do' and 'of'\n\n--In this Example: rename 'list' to 'ls'.\n\nsilly :: [Int] -> Int\nsilly list = case list of      (1:xs) -> 1\n--There is a comment\n                           (2:xs)\n                             | x < 10    -> 4  where  x = last xs\n                           otherwise -> 12\n\n"

      let decl' = (GHC.L l (GHC.HsCase expr (GHC.MatchGroup (m1:(tail matches)) typ)))
      (showGhc decl') `shouldBe` "case list of {\n  (1 : xs) -> 1\n  (2 : xs)\n    | x GHC.Classes.< 10 -> 4\n    where\n        x = GHC.List.last xs\n  otherwise -> 12 }"
      (take 320 $ SYB.showData SYB.Renamer 0 decl') `shouldBe` "\n(L {test/testdata/Renaming/LayoutIn2.hs:(8,14)-(12,42)} \n (HsCase \n  (L {test/testdata/Renaming/LayoutIn2.hs:8:19-22} \n   (HsVar {Name: list})) \n  (MatchGroup \n   [\n    (L {foo:8:32-42} \n     (Match \n      [\n       (L {test/testdata/Renaming/LayoutIn2.hs:8:32-37} \n        (ParPat \n         (L {test/testdata/Renaming/L"
-}
    ------------------------------------
{- NOTE: This test mus remain/be moved back to HaRe.

    it "adds a negative offset to a decl and toks" $ do
      (t,toks) <- parsedFileLayoutIn2
      let forest = mkTreeFromTokens toks

      let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed

      let Just decl@(GHC.L l _) = (locToExp (8,13) (12,43) renamed) :: Maybe (GHC.Located (GHC.HsExpr GHC.Name))

      (showGhc l) `shouldBe` "test/testdata/Renaming/LayoutIn2.hs:(8,14)-(12,42)"
      (showSrcSpan l) `shouldBe` "((8,14),(12,43))"

      (showGhc decl) `shouldBe`
           "case list of {\n"++
           "  (1 : xs) -> 1\n"++
           "  (2 : xs)\n"++
           "    | x GHC.Classes.< 10 -> 4\n"++
           "    where\n"++
           "        x = GHC.List.last xs\n"++
           "  otherwise -> 12 }"
      let (GHC.L _ (GHC.HsCase expr (GHC.MatchGroup matches typ))) = decl
      (showGhc expr) `shouldBe` "list"

      let (GHC.L m1l _) = head matches
      (showSrcSpan m1l) `shouldBe` "((8,28),(8,39))"
      let (m1,forest') = indentDeclToks syncAST (head matches) forest (-2)

      -- let toks' = retrieveTokensInterim forest'
      (invariant forest') `shouldBe` []
      (drawTreeEntry forest') `shouldBe`
              "((10000000001,1),(10000000012,43))\n|\n"++
              "+- ((1,1),(8,26))\n|\n"++
              "+- ((8,26),(8,37))\n|\n"++ -- dedented by 2
              "`- ((10,28),(12,43))\n"

      -- let toksFinal = retrieveTokensFinal forest'
      (renderLinesFromLayoutTree forest') `shouldBe` "module LayoutIn2 where\n\n--Layout rule applies after 'where','let','do' and 'of'\n\n--In this Example: rename 'list' to 'ls'.\n\nsilly :: [Int] -> Int\nsilly list = case list of(1:xs) -> 1\n--There is a comment\n                           (2:xs)\n                             | x < 10    -> 4  where  x = last xs\n                           otherwise -> 12\n\n"

      let decl' = (GHC.L l (GHC.HsCase expr (GHC.MatchGroup (m1:(tail matches)) typ)))
      (showGhc decl') `shouldBe` "case list of {\n  (1 : xs) -> 1\n  (2 : xs)\n    | x GHC.Classes.< 10 -> 4\n    where\n        x = GHC.List.last xs\n  otherwise -> 12 }"
      (take 320 $ SYB.showData SYB.Renamer 0 decl') `shouldBe` "\n(L {test/testdata/Renaming/LayoutIn2.hs:(8,14)-(12,42)} \n (HsCase \n  (L {test/testdata/Renaming/LayoutIn2.hs:8:19-22} \n   (HsVar {Name: list})) \n  (MatchGroup \n   [\n    (L {foo:8:26-36} \n     (Match \n      [\n       (L {test/testdata/Renaming/LayoutIn2.hs:8:26-31} \n        (ParPat \n         (L {test/testdata/Renaming/L"

      -- Now to do it for the second item in the list
      let (GHC.L m2l _) = head $ drop 1 matches
      (showSrcSpan m2l) `shouldBe` "((10,28),(11,66))"
      let (m2,forest2) = indentDeclToks syncAST (head $ drop 1 matches) forest' (-2)

      -- (show forest2) `shouldBe` ""
      -- TODO: sort out this invariant failing
      -- (show forest) `shouldBe` "forest"
      -- (show forest') `shouldBe` "forest'"
      -- (show forest2) `shouldBe` "forest2"
      (invariant forest2) `shouldBe` []
      (drawTreeEntry forest2) `shouldBe`
              "((10000000001,1),(10000000012,43))\n|\n"++
              "+- ((1,1),(8,26))\n|\n"++
              "+- ((8,26),(8,37))\n|\n"++
              "`- ((10000000010,28),(10000000012,43))\n   |\n"++
              "   +- ((10,26),(11,64))\n   |\n"++
              "   `- ((12,28),(12,43))\n"

      -- let toksFinal2 = retrieveTokensFinal forest2
      (renderLinesFromLayoutTree forest2) `shouldBe` "module LayoutIn2 where\n\n--Layout rule applies after 'where','let','do' and 'of'\n\n--In this Example: rename 'list' to 'ls'.\n\nsilly :: [Int] -> Int\nsilly list = case list of(1:xs) -> 1\n--There is a comment\n                         (2:xs)\n                           | x < 10    -> 4  where  x = last xs\n                           otherwise -> 12\n\n"

      let decl2 = (GHC.L l (GHC.HsCase expr (GHC.MatchGroup (m1:m2:(tail $ tail matches)) typ)))
      (showGhc decl2) `shouldBe` "case list of {\n  (1 : xs) -> 1\n  (2 : xs)\n    | x GHC.Classes.< 10 -> 4\n    where\n        x = GHC.List.last xs\n  otherwise -> 12 }"
      (take 320 $ SYB.showData SYB.Renamer 0 decl2) `shouldBe` "\n(L {test/testdata/Renaming/LayoutIn2.hs:(8,14)-(12,42)} \n (HsCase \n  (L {test/testdata/Renaming/LayoutIn2.hs:8:19-22} \n   (HsVar {Name: list})) \n  (MatchGroup \n   [\n    (L {foo:8:26-36} \n     (Match \n      [\n       (L {test/testdata/Renaming/LayoutIn2.hs:8:26-31} \n        (ParPat \n         (L {test/testdata/Renaming/L"
-}
  -- ---------------------------------------------


  describe "reSequenceToks" $ do
    it "Modifies a token stream to cater for changes in length of a token after e.g. renaming" $ do
      pending -- "write this"

  -- ---------------------------------------------

  describe "getTreeFromCache" $ do
    it "get the appropriate tree from the token cache, based on the SrcSpan" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Demote/D1.hs"
      let tk = initTokenCache toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/Demote/D1.hs:11:1-7"
      let l = ss2gs ((11,1),(11,8))
      (showSrcSpan l) `shouldBe` "((11,1),(11,8))"

      let tk' = removeToksFromCache tk (gs2ss l)
      (drawTokenCache tk') `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(9,14))\n|\n"++
               "+- ((11,1),(11,8))(2,-7)D\n|\n"++
               "`- ((13,1),(13,25))\n"++
               "tree TId 1:\n"++
               "((100000011,1),(100000011,8))\n"
      let mainForest = (tkCache tk') Map.! mainTid
      -- let sspan = posToSrcSpan mainForest ((11,1),(11,8))
      let sspan = ss2gs ((11,1),(11,8))

      let tree1 = getTreeFromCache (gs2ss sspan) tk'
      (drawTreeEntry tree1) `shouldBe`
             "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(9,14))\n|\n"++
               "+- ((11,1),(11,8))(2,-7)D\n|\n"++
               "`- ((13,1),(13,25))\n"

      let sspan2 = insertForestLineInSrcSpan (ForestLine False 1 0 1) sspan
      -- (showGhc sspan2) `shouldBe` "f:(33554433,1)-(33554443,7)"
      (show $ gs2ss sspan2) `shouldBe` "((33554433,1),(33554443,8))"
      (showSrcSpanF sspan2) `shouldBe` "(((False,1,0,1),1),((False,1,0,11),8))"

      let tid = treeIdFromForestSpan $ gs2f sspan2
      (show tid) `shouldBe` "TId 1"

      let tree2 = getTreeFromCache (gs2ss sspan2) tk'
      (drawTreeEntry tree2) `shouldBe`
             "((100000011,1),(100000011,8))\n"


  -- ---------------------------------------------

  describe "replaceTreeInCache" $ do
    it "replace the appropriate tree from the token cache, based on the SrcSpan" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Demote/D1.hs"
      let tk = initTokenCache toks

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head decls
      -- (showGhc l) `shouldBe` "test/testdata/Demote/D1.hs:11:1-7"
      let l = ss2gs ((11,1),(11,8))
      (showSrcSpan l) `shouldBe` "((11,1),(11,8))"

      let tk' = removeToksFromCache tk (gs2ss l)
      (drawTokenCache tk') `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(9,14))\n|\n"++
               "+- ((11,1),(11,8))(2,-7)D\n|\n"++
               "`- ((13,1),(13,25))\n"++
               "tree TId 1:\n"++
               "((100000011,1),(100000011,8))\n"
      let mainForest = (tkCache tk') Map.! mainTid
      let sspan = ss2gs ((11,1),(11,8))

      let sspan2 = insertForestLineInSrcSpan (ForestLine False 1 0 1) sspan
      -- (showGhc sspan2) `shouldBe` "f:(33554433,1)-(33554443,7)"
      (show $ gs2ss sspan2) `shouldBe` "((33554433,1),(33554443,8))"
      (showSrcSpanF sspan2) `shouldBe` "(((False,1,0,1),1),((False,1,0,11),8))"

      let tree1 = mkTreeFromTokens (take 10 toks)
      let tk1 = replaceTreeInCache (gs2ss sspan) tree1 tk'
      (drawTokenCache tk1) `shouldBe`
             "tree TId 0:\n"++
             "((1,1),(6,17))\n"++
             "tree TId 1:\n"++
             "((100000011,1),(100000011,8))\n"

      let tk2 = replaceTreeInCache (gs2ss sspan2) tree1 tk'
      (drawTokenCache tk2) `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(9,14))\n|\n"++
               "+- ((11,1),(11,8))(2,-7)D\n|\n"++
               "`- ((13,1),(13,25))\n"++
               "tree TId 1:\n"++
               "((100000001,1),(100000006,17))\n"


  -- -----------------------------------

  describe "removeToksFromCache" $ do
    it "removes a SrcSpan from the main tree, and stashes it" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/Demote/WhereIn4.hs"
      let tk = initTokenCache toks

      -- removeToksForPos ((15,1),(15,17))
      let pos = ((15,1),(15,17))
      let mainForest = (tkCache tk) Map.! mainTid
      let sspan = ss2gs pos
      let tk' = removeToksFromCache tk (gs2ss sspan)

      (drawTokenCache tk') `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(18,23))\n|\n"++
               "+- ((1,1),(14,18))\n|\n"++
               "+- ((15,1),(15,17))(2,-16)D\n|\n"++
               "`- ((17,1),(18,23))\n"++
               "tree TId 1:\n"++
               "((100000015,1),(100000015,17))\n"

      let tree1 = (tkCache tk') Map.! (TId 1)

      (show $ retrieveTokensInterim tree1) `shouldBe` "[((((15,1),(15,1)),ITsemi),\"\"),((((15,1),(15,3)),ITvarid \"sq\"),\"sq\"),((((15,4),(15,7)),ITvarid \"pow\"),\"pow\"),((((15,8),(15,9)),ITvarid \"z\"),\"z\"),((((15,10),(15,11)),ITequal),\"=\"),((((15,12),(15,13)),ITvarid \"z\"),\"z\"),((((15,13),(15,14)),ITvarsym \"^\"),\"^\"),((((15,14),(15,17)),ITvarid \"pow\"),\"pow\"),((((15,19),(15,39)),ITlineComment \"--there is a comment\"),\"--there is a comment\")]"

      -- putToksForSpan test/testdata/Demote/WhereIn4.hs:100000015:14-16:[((((0,1),(0,2)),ITvarid "p"),"p")]

      -- let sspan3 = posToSrcSpan mainForest ((100000015,14),(100000015,17))
      let sspan3 = ss2gs
                        (((forestLineToGhcLine $ ForestLine False 1 0 15),14),
                         ((forestLineToGhcLine $ ForestLine False 1 0 15),17) )

      -- (showGhc sspan3) `shouldBe` "f:100000015:14-16"
      -- (showGhc sspan3) `shouldBe` "f:33554447:14-16"
      (show $ gs2ss sspan3) `shouldBe` "((33554447,14),(33554447,17))"
      (showSrcSpanF sspan3) `shouldBe` "(((False,1,0,15),14),((False,1,0,15),17))"
      let toks3 = [mkToken (GHC.ITvarid (GHC.mkFastString "p")) (0,1) "p"]
      (show toks3) `shouldBe` "[((((0,1),(0,2)),ITvarid \"p\"),\"p\")]"
      let (tk3,_newSpan) = putToksInCache tk' (gs2ss sspan3) toks3
      (drawTokenCache tk3) `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(18,23))\n|\n"++
               "+- ((1,1),(14,18))\n|\n"++
               "+- ((15,1),(15,17))(2,-16)D\n|\n"++
               "`- ((17,1),(18,23))\n"++
               "tree TId 1:\n"++
               "((100000015,1),(100000015,17))\n|\n"++
               "+- ((100000015,1),(100000015,14))\n|\n"++
               "`- ((10100000015,14),(10100000015,15))\n"++
               "tree TId 2:\n"++
               "((200000015,14),(200000015,17))\n"

      let (forest2,tree1') = getSrcSpanFor tree1 (gs2f sspan3)
      (show $ retrieveTokensInterim forest2) `shouldBe` "[((((15,1),(15,1)),ITsemi),\"\"),((((15,1),(15,3)),ITvarid \"sq\"),\"sq\"),((((15,4),(15,7)),ITvarid \"pow\"),\"pow\"),((((15,8),(15,9)),ITvarid \"z\"),\"z\"),((((15,10),(15,11)),ITequal),\"=\"),((((15,12),(15,13)),ITvarid \"z\"),\"z\"),((((15,13),(15,14)),ITvarsym \"^\"),\"^\"),((((15,14),(15,17)),ITvarid \"pow\"),\"pow\"),((((15,19),(15,39)),ITlineComment \"--there is a comment\"),\"--there is a comment\")]"

      (show $ retrieveTokensInterim tree1') `shouldBe` "[((((15,14),(15,17)),ITvarid \"pow\"),\"pow\"),((((15,19),(15,39)),ITlineComment \"--there is a comment\"),\"--there is a comment\")]"

      let tree3 = (tkCache tk3) Map.! (TId 1)
      (show $ retrieveTokensInterim tree3) `shouldBe` "[((((15,1),(15,1)),ITsemi),\"\"),((((15,1),(15,3)),ITvarid \"sq\"),\"sq\"),((((15,4),(15,7)),ITvarid \"pow\"),\"pow\"),((((15,8),(15,9)),ITvarid \"z\"),\"z\"),((((15,10),(15,11)),ITequal),\"=\"),((((15,12),(15,13)),ITvarid \"z\"),\"z\"),((((15,13),(15,14)),ITvarsym \"^\"),\"^\"),((((15,14),(15,15)),ITvarid \"p\"),\"p\"),((((15,19),(15,39)),ITlineComment \"--there is a comment\"),\"--there is a comment\")]"

  -- ---------------------------------------------
{- NOTE: should remain in HaRe tests
  describe "syncAstToLatestCache" $ do
    it "update the SrcSpans in a declaration to match the latest stash" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Demote/D1.hs"
      let tk = initTokenCache toks

      let renamed = fromJust $ GHC.tm_renamed_source t
      let decls = hsBinds renamed
      let decl@(GHC.L l _) = head decls
      (showGhc l) `shouldBe` "test/testdata/Demote/D1.hs:11:1-7"
      (showSrcSpan l) `shouldBe` "((11,1),(11,8))"

      let tk' = removeToksFromCache tk (gs2ss l)
      (drawTokenCache tk') `shouldBe`
               "tree TId 0:\n"++
               "((1,1),(13,25))\n|\n"++
               "+- ((1,1),(9,14))\n|\n"++
               "+- ((11,1),(11,8))(2,-7)D\n|\n"++
               "`- ((13,1),(13,25))\n"++
               "tree TId 1:\n"++
               "((100000011,1),(100000011,8))\n"

      let mainForest = (tkCache tk') Map.! mainTid
      let sspan = posToSrcSpan mainForest ((11,1),(11,8))

      let sspan2 = insertForestLineInSrcSpan (ForestLine False 1 0 1) sspan
      (showGhc sspan2) `shouldBe` "f:(33554433,1)-(33554443,7)"
      (showSrcSpanF sspan2) `shouldBe` "(((False,1,0,1),1),((False,1,0,11),8))"

      let (GHC.L ss' _) = syncAstToLatestCache tk' decl
      -- (showGhc ss') `shouldBe` "f:100000011:1-7"
      (showGhc ss') `shouldBe` "foo:33554443:1-7"
      (showSrcSpanF ss') `shouldBe` "(((False,1,0,11),1),((False,1,0,11),8))"
-}
  -- ---------------------------------------------

  describe "formatAfterDelete" $ do
    it "does not leave a blank line in toks after deleting" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/TokenTest.hs"
      -- let f1 = mkTreeFromTokens toks

      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
      let f1 = allocTokens parsed toks

      (renderLinesFromLayoutTree f1) `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\nbab a b =\n  let bar = 3\n  in     b + bar -- ^trailing comment\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n\n\n\n"

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- let decls = hsBinds renamed
      -- let (GHC.L l _) = head $ drop 1 decls
      -- (showGhc l) `shouldBe` "test/testdata/TokenTest.hs:(13,1)-(15,16)"
      let l = ss2gs ((13,1),(15,17))
      (showSrcSpan l) `shouldBe` "((13,1),(15,17))"

      let (f2,_) = removeSrcSpan f1 (gs2f l)

      -- let es = retrieveTokens' f2
      -- (show $ deleteGapsToks es) `shouldBe` ""
      (renderLinesFromLayoutTree f2) `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n\n\n\n"

      (show $ retrieveLinesFromLayoutTree f2) `shouldBe`
         "[(1 1 0 SOriginal ONone \"module TokenTest where\"),"++
          "(3 1 0 SOriginal ONone \"-- Test new style token manager\"),"++
          "(5 1 0 SOriginal ONone \"bob a b = x\"),"++
          -- "(6 3 0 SOriginal OGroup \"where x = 3\"),"++
          "(6 3 0 SOriginal ONone \"where x = 3\"),"++
          "(8 1 0 SOriginal ONone \"bib a b = x\"),"++
          "(9 3 0 SOriginal ONone \"where\"),"++
          -- "(10 5 0 SOriginal OGroup \"x = 3\"),"++
          "(10 5 0 SOriginal ONone \"x = 3\"),"++
          "(13 1 0 SOriginal ONone \"-- leading comment\"),"++
          "(14 1 0 SOriginal ONone \"foo x y =\"),"++
          "(15 3 0 SOriginal OGroup \"do c <- getChar\"),"++
          "(16 6 0 SOriginal OGroup \"return c\"),"++
          "(21 1 0 SOriginal ONone \"\")]"

      (renderLinesFromLayoutTree f2) `shouldBe` "module TokenTest where\n\n-- Test new style token manager\n\nbob a b = x\n  where x = 3\n\nbib a b = x\n  where\n    x = 3\n\n\n-- leading comment\nfoo x y =\n  do c <- getChar\n     return c\n\n\n\n\n"

  -- ---------------------------------------------

{- Not sure this test is relevant anymore, as it does not start with a proper token tree

  describe "deleteGapsToks" $ do
    it "closes the gap when finding a Deleted Entry" $ do
      (_t,toks) <- parsedFileLiftWhereIn1Ghc
      let f1 = mkTreeFromTokens toks

      -- removeToksForPos ((11,18),(12,32))
      -- |  +- ((11,18),(12,31))D
      let pos = ((11,18),(12,32))
      let sspan = posToSrcSpan f1 pos
      let (f2,_t2) = removeSrcSpan f1 (gs2f sspan)

      (drawTreeEntry f2) `shouldBe`
            "((1,1),(16,23))\n|\n"++
            "+- ((1,1),(10,17))\n|\n"++
            "+- ((11,18),(12,32))(1,-14)D\n|\n"++
            "`- ((13,18),(16,23))\n"

      -- putDeclToksAfterSpan test/testdata/LiftToToplevel/WhereIn1.hs:(9,1)-(13,22):("(((False,0,0,9),1),((False,0,0,13),23))",PlaceOffset 2 0 2,[((((1,19),(1,21)),ITvarid "sq"),"sq"),((((1,23),(1,26)),ITvarid "pow"),"pow"),((((1,27),(1,28)),ITinteger 0),"0"),((((1,28),(1,29)),ITequal),"="),((((1,30),(1,31)),ITinteger 0),"0"),((((2,19),(2,21)),ITvarid "sq"),"sq"),((((2,23),(2,26)),ITvarid "pow"),"pow"),((((2,27),(2,28)),ITvarid "z"),"z"),((((2,28),(2,29)),ITequal),"="),((((2,30),(2,31)),ITvarid "z"),"z"),((((2,31),(2,32)),ITvarsym "^"),"^"),((((2,32),(2,35)),ITvarid "pow"),"pow")])

      newToks <- liftIO $ basicTokenise "\n                  sq  pow 0= 0\n                  sq  pow z= z^pow"
      (show newToks) `shouldBe` "[((((1,19),(1,21)),ITvarid \"sq\"),\"sq\"),((((1,23),(1,26)),ITvarid \"pow\"),\"pow\"),((((1,27),(1,28)),ITinteger 0),\"0\"),((((1,28),(1,29)),ITequal),\"=\"),((((1,30),(1,31)),ITinteger 0),\"0\"),((((2,19),(2,21)),ITvarid \"sq\"),\"sq\"),((((2,23),(2,26)),ITvarid \"pow\"),\"pow\"),((((2,27),(2,28)),ITvarid \"z\"),\"z\"),((((2,28),(2,29)),ITequal),\"=\"),((((2,30),(2,31)),ITvarid \"z\"),\"z\"),((((2,31),(2,32)),ITvarsym \"^\"),\"^\"),((((2,32),(2,35)),ITvarid \"pow\"),\"pow\")]"

      let sspan2 = posToSrcSpan f1 ((9,1),(13,23))
      let pos2 = (PlaceOffset 2 0 2)
      let (f3,_newSpan) = addToksAfterSrcSpan f2 (gs2ss sspan2) pos2 newToks
      (drawTreeEntry f3) `shouldBe`
            "((1,1),(16,23))\n|\n"++
            "+- ((1,1),(1,37))\n|\n"++
            "+- ((9,1),(13,23))\n|  |\n"++
            "|  +- ((9,1),(10,17))\n|  |\n"++
            "|  +- ((11,18),(12,32))(1,-14)D\n|  |\n"++
            "|  `- ((13,18),(13,23))\n|\n"++
            "+- ((1000015,1),(1000016,17))\n|\n"++
            "`- ((15,1),(16,23))\n"

--
      -- Seems the problem is in addToksAfterSrcSpan

      let (fwithspan,tree) = getSrcSpanFor f2 (gs2f sspan2)

      let z = openZipperToSpan (gs2f sspan2) $ Z.fromTree fwithspan
      let prevToks = case (retrievePrevLineToks z) of
                   RT [] -> reverseToks $ retrieveTokensInterim tree
                   xs -> xs

      let prevToks' = limitPrevToks prevToks (gs2ss sspan2)
      -- let toks' = reIndentToks pos2 (unReverseToks prevToks') newToks

      -- Hmmm. This is the final position, after taking into account
      -- the deleted entry. BUT, we are putting it back into the
      -- original tree.
      (show $ last $ unReverseToks prevToks') `shouldBe` "((((13,22),(13,23)),ITinteger 2),\"2\")"

      -- What we actually need, but we are getting ((13,1),(13,3))
      let toks'' = placeToksForSpan fwithspan (gs2ss sspan2) tree pos2 newToks
      (show $ head toks'') `shouldBe` "((((15,1),(15,3)),ITvarid \"sq\"),\"sq\")"

--


      -- (showTree f3) `shouldBe` ""

      -- (GHC.showRichTokenStream $ retrieveTokensFinal f3) `shouldBe` ""

      -- let entries = retrieveTokens' f3

      -- (show entries) `shouldBe` ""

      -- (show $ deleteGapsToks entries) `shouldBe` ""
      -- (show $ deleteGapsToks' entries) `shouldBe` ""

      (renderLinesFromLayoutTree f3) `shouldBe` "module LiftToToplevel.WhereIn1 where\n\n --A definition can be lifted from a where or let to the top level binding group.\n --Lifting a definition widens the scope of the definition.\n\n --In this example, lift 'sq' in 'sumSquares'\n --This example aims to test add parameters to 'sq'.\n\n sumSquares x y = sq x + sq y\n            where\n                  pow=2\n\n sq  pow 0= 0\n sq  pow z= z^pow\n \n anotherFun 0 y = sq y\n      where  sq x = x^2\n "

-}
  -- ---------------------------------------------

  describe "calcEndGap" $ do
    it "Closes the gap when finding a Deleted Entry" $ do
      (_t,toks) <- parsedFileGhc "./test/testdata/LiftToToplevel/WhereIn1.hs"
      let f1 = mkTreeFromTokens toks

      let pos = ((11,18),(12,32))
      let sspan = ss2gs pos
      let (f2,_t2) = removeSrcSpan f1 (gs2f sspan)

      (drawTreeEntry f2) `shouldBe`
            "((1,1),(16,23))\n|\n"++
            "+- ((1,1),(10,17))\n|\n"++
            "+- ((11,18),(12,32))(1,-14)D\n|\n"++
            "`- ((13,18),(16,23))\n"

      (calcEndGap f2 (gs2f sspan)) `shouldBe` (1,-14)

  -- ---------------------------------------------

  describe "openZipperToSpan" $ do
    it "opens a zipper to a span, even if it has been added and is partly out of alignment" $ do
      (t,toks) <- parsedFileGhc "./test/testdata/Layout/LetExpr.hs"

      let parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t

      -- let renamed = fromJust $ GHC.tm_renamed_source t
      -- (SYB.showData SYB.Renamer 0 renamed) `shouldBe` ""

      -- let origSource = (GHC.showRichTokenStream $ bypassGHCBug7351 toks)

      let layout = allocTokens parsed toks
      (show $ retrieveTokens layout) `shouldBe` (show toks)
      (invariant layout) `shouldBe` []

      (drawTreeCompact layout) `shouldBe`
         "0:((3,1),(9,1))\n"++
         "1:((3,1),(3,7))\n"++
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


      let sspan = ss2gs ((6,11),(6,16))
      -- (showGhc sspan) `shouldBe` "f:6:11-15"
      (show $ gs2ss sspan) `shouldBe` "((6,11),(6,16))"

      newToks <- liftIO $ basicTokenise "\n               ff :: Int"
      (show newToks) `shouldBe` "[((((1,16),(1,18)),ITvarid \"ff\"),\"ff\"),((((1,19),(1,21)),ITdcolon),\"::\"),((((1,22),(1,25)),ITconid \"Int\"),\"Int\")]"

      let pos = (PlaceAbsCol 2 5 50)
      let (layout2,newSpan) = addToksAfterSrcSpan layout (gs2ss sspan) pos newToks

      -- (showGhc $ ss2gs newSpan) `shouldBe` "foo:1048584:5-13"
      (show $ ss2f newSpan) `shouldBe` "(((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),14))"
      (drawTreeCompact layout2) `shouldBe`
         "0:((3,1),(9,1))\n"++
         "1:((3,1),(3,7))\n"++
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
         "5:((1000008,5),(1000008,14))\n"++
         "4:((7,10),(7,15))\n"++
         "5:((7,10),(7,11))\n"++
         "5:((7,12),(7,13))\n"++
         "5:((7,14),(7,15))\n"++
         "1:((9,1),(9,1))\n"


      let treeAsList = getTreeSpansAsList layout2
      (show treeAsList) `shouldBe`
         "[(0,(((ForestLine False 0 0 3),1),((ForestLine False 0 0 9),1))),(1,(((ForestLine False 0 0 3),1),((ForestLine False 0 0 3),7))),(1,(((ForestLine False 0 0 3),8),((ForestLine False 0 0 3),22))),(1,(((ForestLine False 0 0 3),23),((ForestLine False 0 0 3),28))),(1,(((ForestLine False 0 0 5),1),((ForestLine False 0 0 7),15))),(2,(((ForestLine False 0 0 5),1),((ForestLine False 0 0 5),4))),(2,(((ForestLine False 0 0 5),5),((ForestLine False 0 0 7),15))),(3,(((ForestLine False 0 0 5),5),((ForestLine False 0 0 5),6))),(3,(((ForestLine False 0 0 5),7),((ForestLine False 0 0 7),15))),(4,(((ForestLine False 0 0 5),7),((ForestLine False 0 0 5),10))),(4,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 6),16))),(5,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 5),16))),(6,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 5),12))),(6,(((ForestLine False 0 0 5),13),((ForestLine False 0 0 5),16))),(7,(((ForestLine False 0 0 5),13),((ForestLine False 0 0 5),14))),(7,(((ForestLine False 0 0 5),15),((ForestLine False 0 0 5),16))),(5,(((ForestLine False 0 0 6),11),((ForestLine False 0 0 6),16))),(6,(((ForestLine False 0 0 6),11),((ForestLine False 0 0 6),12))),(6,(((ForestLine False 0 0 6),13),((ForestLine False 0 0 6),16))),(7,(((ForestLine False 0 0 6),13),((ForestLine False 0 0 6),14))),(7,(((ForestLine False 0 0 6),15),((ForestLine False 0 0 6),16))),(5,(((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),14))),(4,(((ForestLine False 0 0 7),10),((ForestLine False 0 0 7),15))),(5,(((ForestLine False 0 0 7),10),((ForestLine False 0 0 7),11))),(5,(((ForestLine False 0 0 7),12),((ForestLine False 0 0 7),13))),(5,(((ForestLine False 0 0 7),14),((ForestLine False 0 0 7),15))),(1,(((ForestLine False 0 0 9),1),((ForestLine False 0 0 9),1)))]"


      let ssWanted = (((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),7))
      (spanContains (((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),14)) ssWanted) `shouldBe` True
      (spanContains (((ForestLine False 0 0 1),1),((ForestLine False 0 0 9),1)) ssWanted) `shouldBe` True

          -- True if first span contains the second
      let myMatch (((ForestLine _ _ vs1 rs1),cs1),((ForestLine _ _ ve1 re1),ce1))
                  (((ForestLine _ _ vs2 rs2),cs2),((ForestLine _ _ ve2 re2),ce2))
            = vs1 == vs2 && ve1 == ve2 && ((rs1,cs1) <= (rs2,cs2)) && ((re1,ce1) >= (re2,ce2))

      -- let tl2 = dropWhile (\(_,s) -> not (spanContains s ssWanted)) $ reverse treeAsList
      let tl2 = dropWhile (\(_,s) -> not (myMatch s ssWanted)) $ reverse treeAsList
      (show tl2) `shouldBe`
          "[(5,(((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),14))),(7,(((ForestLine False 0 0 6),15),((ForestLine False 0 0 6),16))),(7,(((ForestLine False 0 0 6),13),((ForestLine False 0 0 6),14))),(6,(((ForestLine False 0 0 6),13),((ForestLine False 0 0 6),16))),(6,(((ForestLine False 0 0 6),11),((ForestLine False 0 0 6),12))),(5,(((ForestLine False 0 0 6),11),((ForestLine False 0 0 6),16))),(7,(((ForestLine False 0 0 5),15),((ForestLine False 0 0 5),16))),(7,(((ForestLine False 0 0 5),13),((ForestLine False 0 0 5),14))),(6,(((ForestLine False 0 0 5),13),((ForestLine False 0 0 5),16))),(6,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 5),12))),(5,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 5),16))),(4,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 6),16))),(4,(((ForestLine False 0 0 5),7),((ForestLine False 0 0 5),10))),(3,(((ForestLine False 0 0 5),7),((ForestLine False 0 0 7),15))),(3,(((ForestLine False 0 0 5),5),((ForestLine False 0 0 5),6))),(2,(((ForestLine False 0 0 5),5),((ForestLine False 0 0 7),15))),(2,(((ForestLine False 0 0 5),1),((ForestLine False 0 0 5),4))),(1,(((ForestLine False 0 0 5),1),((ForestLine False 0 0 7),15))),(1,(((ForestLine False 0 0 3),23),((ForestLine False 0 0 3),28))),(1,(((ForestLine False 0 0 3),8),((ForestLine False 0 0 3),22))),(1,(((ForestLine False 0 0 3),1),((ForestLine False 0 0 3),7))),(0,(((ForestLine False 0 0 3),1),((ForestLine False 0 0 9),1)))]"

      -- drop all higher indented values, all the way to the root

      let fff acc@((cd1,_cs):_) (v,sspan1) = if v < cd1 then (v,sspan1):acc
                                                        else acc

      let tl3 = foldl' fff [(head tl2)] tl2
      (show tl3) `shouldBe`
          "[(0,(((ForestLine False 0 0 3),1),((ForestLine False 0 0 9),1))),"++
           "(1,(((ForestLine False 0 0 5),1),((ForestLine False 0 0 7),15))),"++
           "(2,(((ForestLine False 0 0 5),5),((ForestLine False 0 0 7),15))),"++
           "(3,(((ForestLine False 0 0 5),7),((ForestLine False 0 0 7),15))),"++
           "(4,(((ForestLine False 0 0 5),11),((ForestLine False 0 0 6),16))),"++
           "(5,(((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),14)))]"

      ------------
{-
      let ff :: [ForestSpan] -> Entry -> [ForestSpan]
          ff acc entry = acc ++ [forestSpanFromEntry entry]
      (show $ F.foldl ff [] layout2) `shouldBe` ""
-}

      let zo = openZipperToSpanOrig (((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),7)) $ Z.fromTree layout2
      (show $ treeStartEnd (Z.tree zo)) `shouldBe` "(((ForestLine False 0 0 3),1),((ForestLine False 0 0 9),1))"

      let z = openZipperToSpanAdded (((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),7)) $ Z.fromTree layout2
      (show $ treeStartEnd (Z.tree z)) `shouldBe` "(((ForestLine False 0 1 8),5),((ForestLine False 0 1 8),14))"

-- ---------------------------------------------------------------------
-- Helper functions


-- fs :: GHC.SrcSpan -> ForestSpan
-- fs = gs2f

emptyTree :: Tree (Entry PosToken)
emptyTree = Node (Entry nonNullSpan NoChange []) []

mkTreeFromSubTrees :: [Tree (Entry PosToken)] -> Tree (Entry PosToken)
mkTreeFromSubTrees [] = Node (Entry nullForestSpan NoChange []) []
mkTreeFromSubTrees trees = Node (Entry sspan NoChange []) trees
  where
   (Node (Entry _ _ startToks) _) = head trees
   (Node (Entry _ _ endToks) _) = last trees
   startLoc = tokenPos $ head startToks
   endLoc   = tokenPosEnd $ last endToks -- SrcSpans count from start of token, not end
   sspan    = simpPosToForestSpan (startLoc,endLoc)

nonNullSpan :: ForestSpan
nonNullSpan = ((ForestLine False 0 0 0,0),(ForestLine False 0 0 1,0))


-- ---------------------------------------------------------------------
{-
-- liftD1FileName :: GHC.FastString
-- liftD1FileName = GHC.mkFastString "./test/testdata/LiftToToplevel/D1.hs"

parsedFileLiftD1Ghc :: IO (ParseResult,[PosToken])
parsedFileLiftD1Ghc = parsedFileGhc "./test/testdata/LiftToToplevel/D1.hs"

-- ---------------------------------------------------------------------

parsedFileLiftLetIn1Ghc :: IO (ParseResult,[PosToken])
parsedFileLiftLetIn1Ghc = parsedFileGhc "./test/testdata/LiftToToplevel/LetIn1.hs"

-- ---------------------------------------------------------------------

-- tokenTestFileName :: GHC.FastString
-- tokenTestFileName = GHC.mkFastString "./test/testdata/TokenTest.hs"

parsedFileTokenTestGhc :: IO (ParseResult,[PosToken])
parsedFileTokenTestGhc = parsedFileGhc "./test/testdata/TokenTest.hs"

-- ---------------------------------------------------------------------

-- dupDefDd1FileName :: GHC.FastString
-- dupDefDd1FileName = GHC.mkFastString "./test/testdata/DupDef/Dd1.hs"

parsedFileDupDefDd1 :: IO (ParseResult, [PosToken])
parsedFileDupDefDd1 = parsedFileGhc "./test/testdata/DupDef/Dd1.hs"

-- ---------------------------------------------------------------------

-- moveDefMd1FileName :: GHC.FastString
-- moveDefMd1FileName = GHC.mkFastString "./test/testdata/MoveDef/Md1.hs"

parsedFileMoveDefMd1 :: IO (ParseResult, [PosToken])
parsedFileMoveDefMd1 = parsedFileGhc "./test/testdata/MoveDef/Md1.hs"

-- ---------------------------------------------------------------------

-- demoteD1FileName  :: GHC.FastString
-- demoteD1FileName = GHC.mkFastString "./test/testdata/Demote/D1.hs"

parsedFileDemoteD1 :: IO (ParseResult, [PosToken])
parsedFileDemoteD1 = parsedFileGhc "./test/testdata/Demote/D1.hs"

-- ---------------------------------------------------------------------

-- demoteWherIn4FileName  :: GHC.FastString
-- demoteWherIn4FileName = GHC.mkFastString "./test/testdata/Demote/WhereIn4.hs"

parsedFileDemoteWhereIn4 :: IO (ParseResult, [PosToken])
parsedFileDemoteWhereIn4 = parsedFileGhc "./test/testdata/Demote/WhereIn4.hs"

-- ---------------------------------------------------------------------

-- liftWhereIn1FileName :: GHC.FastString
-- liftWhereIn1FileName = GHC.mkFastString "./test/testdata/LiftToToplevel/WhereIn1.hs"

parsedFileLiftWhereIn1Ghc :: IO (ParseResult,[PosToken])
parsedFileLiftWhereIn1Ghc = parsedFileGhc "./test/testdata/LiftToToplevel/WhereIn1.hs"

-- ---------------------------------------------------------------------

-- dd1FileName :: GHC.FastString
-- dd1FileName = GHC.mkFastString "./test/testdata/DupDef/Dd1.hs"

-- parsedFileDd1Ghc :: IO (ParseResult,[PosToken])
-- parsedFileDd1Ghc = parsedFileGhc "./test/testdata/DupDef/Dd1.hs"

-- ---------------------------------------------------------------------

-- layoutIn2FileName :: GHC.FastString
-- layoutIn2FileName = GHC.mkFastString "./test/testdata/Renaming/LayoutIn2.hs"

parsedFileLayoutIn2 :: IO (ParseResult, [PosToken])
parsedFileLayoutIn2 = parsedFileGhc "./test/testdata/Renaming/LayoutIn2.hs"

-- ---------------------------------------------------------------------

parsedFileLayoutLetExpr :: IO (ParseResult,[PosToken])
parsedFileLayoutLetExpr = parsedFileGhc "./test/testdata/Layout/LetExpr.hs"

-- ---------------------------------------------------------------------

-- parsedFileLayoutLetStmt :: IO (ParseResult,[PosToken])
-- parsedFileLayoutLetStmt = parsedFileGhc "./test/testdata/Layout/LetStmt.hs"

-- ---------------------------------------------------------------------

parsedFileWhere :: IO (ParseResult,[PosToken])
parsedFileWhere = parsedFileGhc "./test/testdata/Layout/Where.hs"

-- ---------------------------------------------------------------------

-- parsedFilePatBind :: IO (ParseResult,[PosToken])
-- parsedFilePatBind = parsedFileGhc "./test/testdata/Layout/PatBind.hs"

-- ---------------------------------------------------------------------

-- parsedFileMd1Ghc :: IO (ParseResult,[PosToken])
-- parsedFileMd1Ghc = parsedFileGhc "./test/testdata/MoveDef/Md1.hs"

-- ---------------------------------------------------------------------

-- parsedFileLayoutLet1 :: IO (ParseResult, [PosToken])
-- parsedFileLayoutLet1 = parsedFileGhc "./test/testdata/TypeUtils/LayoutLet1.hs"

-- ----------------------------------------------------

-}
