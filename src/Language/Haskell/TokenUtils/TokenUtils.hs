module Language.Haskell.TokenUtils.TokenUtils
  (
    replaceTokenInCache
  , replaceTokenForSrcSpan
  , invariant
  ) where

import Control.Exception
import Data.List
import Data.Tree

import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Utils

import qualified Data.Map as Map
import qualified Data.Tree.Zipper as Z

-- ---------------------------------------------------------------------

invariant :: a
invariant = assert False undefined

-- ---------------------------------------------------------------------

replaceTokenInCache :: (IsToken a) => TokenCache a -> Span -> a -> TokenCache a
replaceTokenInCache tk sspan tok = tk'
  where
   forest = getTreeFromCache sspan tk
   forest' = replaceTokenForSrcSpan forest sspan tok
   tk' = replaceTreeInCache sspan forest' tk

-- ---------------------------------------------------------------------

getTreeFromCache :: (IsToken a) => Span -> TokenCache a -> Tree (Entry a)
getTreeFromCache sspan tk = (tkCache tk) Map.! tid
  where
    tid = treeIdFromForestSpan $ srcSpanToForestSpan sspan

-- ---------------------------------------------------------------------

replaceTreeInCache :: (IsToken a) => Span -> Tree (Entry a) -> TokenCache a -> TokenCache a
replaceTreeInCache sspan tree tk = tk'
  where
    tid = treeIdFromForestSpan $ srcSpanToForestSpan sspan
    -- tree' = treeIdIntoTree tid tree
    tree' = putTidInTree tid tree
    tk' = tk {tkCache = Map.insert tid tree' (tkCache tk) }

putTidInTree :: (IsToken a) => TreeId -> Tree (Entry a) -> Tree (Entry a)
putTidInTree tid (Node (Deleted fspan pg eg) subs) = (Node (Deleted fs' pg eg) subs)
  where fs' = treeIdIntoForestSpan tid fspan
putTidInTree tid (Node (Entry fspan lay toks) subs) = tree'
  where
    subs' = map (putTidInTree tid) subs
    fs' = treeIdIntoForestSpan tid fspan
    tree' = Node (Entry fs' lay toks) subs'

-- ---------------------------------------------------------------------

-- |Replace a single token in a token tree, without changing the
-- structure of the tree
-- NOTE: the GHC.SrcSpan may have been used to select the appropriate
-- forest in the first place, and is required to select the correct
-- span in the tree, due to the ForestLine annotations that may be present

-- TODO: work at the token level, not the sspan level
-- TODO: Use start of token span only, with length 1.
replaceTokenForSrcSpan :: (IsToken a) => Tree (Entry a) -> Span -> a -> Tree (Entry a)
replaceTokenForSrcSpan forest sspan tok = forest'
  where
    -- (GHC.L tl _,_) = tok
    tl = getSpan tok
    -- First open to the sspan, making use of any Forestline annotations
    z = openZipperToSpanDeep (srcSpanToForestSpan sspan) $ Z.fromTree forest

    -- Then drill down to the specific subtree containing the token
    -- z' = openZipperToSpan (srcSpanToForestSpan tl) z
    z' = z -- No, pass in original token span as sspan.

    -- Note: with LayoutTree, the full tree matching the AST has been
    -- built, still need to drill down to the nearest enclosing span
    (tspan,lay,toks) = case Z.tree z' of
       (Node (Entry ss ly tks) []) -> (ss,ly,tks)
       (Node (Entry _ _ _nullToks) _sub) -> error $ "replaceTokenForSrcSpan:tok pos" ++ (showForestSpan $ sf sspan) ++ " expecting tokens, found: " ++ (show $ Z.tree z')
       (Node (Deleted _ _ _) _sub)       -> error $ "replaceTokenForSrcSpan:tok pos" ++ (showForestSpan $ sf sspan) ++ " expecting Entry, found: " ++ (show $ Z.tree z')

    ((row,col),_) = forestSpanToSimpPos $ srcSpanToForestSpan tl
    toks' = replaceTokNoReAlign toks (row,col) tok

    zf = Z.setTree (Node (Entry tspan lay toks') []) z'
    forest' = Z.toTree zf


-- ---------------------------------------------------------------------

-- |Open a zipper so that its focus has the given SrcSpan in its
-- subtree, or the location where the SrcSpan should go, if it is not
-- in the tree.
-- In the case of an 'Above' layout with the same SrcSpan below,
-- return that instead
openZipperToSpanDeep
  :: (IsToken a)
  => ForestSpan
     -> Z.TreePos Z.Full (Entry a)
     -> Z.TreePos Z.Full (Entry a)
openZipperToSpanDeep sspan z = zf
  where
    z' = openZipperToSpan sspan z

    zf = case Z.tree z' of
           (Node (Entry _ (Above _ _ _ _) _) _) ->
                case getChildrenAsZ z' of
                  []  -> z'
                  [x] -> if (treeStartEnd (Z.tree x) == sspan) then x else z'
                  _   -> z'
           _ -> z'


-- ---------------------------------------------------------------------

-- |Open a zipper so that its focus has the given SrcSpan in its
-- subtree, or the location where the SrcSpan should go, if it is not
-- in the tree
openZipperToSpan ::
  (IsToken a)
  => ForestSpan
     -> Z.TreePos Z.Full (Entry a)
     -> Z.TreePos Z.Full (Entry a)
openZipperToSpan sspan z
  | hasVersions = openZipperToSpanAdded sspan z
  | otherwise   = openZipperToSpanOrig sspan z
  where
    (vs,_ve) = forestSpanVersions sspan
    hasVersions = vs /= 0


-- ---------------------------------------------------------------------

-- |Open a zipper so that its focus has the given SrcSpan in its
-- subtree, or the location where the SrcSpan should go, if it is not
-- in the tree
openZipperToSpanOrig ::
  (IsToken a)
  => ForestSpan
     -> Z.TreePos Z.Full (Entry a)
     -> Z.TreePos Z.Full (Entry a)
openZipperToSpanOrig sspan z
  = if (treeStartEnd (Z.tree z) == sspan) || (Z.isLeaf z)
      then z
      else z'
        where
          -- go through all of the children to find the one that
          -- either is what we are looking for, or contains it

          -- childrenAsZ = go [] (Z.firstChild z)
          childrenAsZ = getChildrenAsZ z
          z' = case (filter contains childrenAsZ) of
            [] -> z -- Not directly in a subtree, this is as good as
                    -- it gets
            [x] -> -- exactly one, drill down
                   openZipperToSpan sspan x

            xx  -> case (filter (\zt -> (treeStartEnd $ Z.tree zt) == sspan) xx) of 
                    [] -> -- more than one matches, see if we can get
                          -- rid of the ones that have been lengthened
                          case (filter (not .forestSpanLenChanged . treeStartEnd . Z.tree) xx) of
                            [] -> z -- we tried...
                            [w] -> openZipperToSpan sspan w
                            -- ww -> error $ "openZipperToSpan:can't resolve:(sspan,ww)="++(show (sspan,ww))
                            ww -> -- more than one candidate, break
                                  -- the tie on version match
                                  case (filter (\zt -> matchVersions sspan zt) ww) of
                                     [v] -> openZipperToSpan sspan v
                                     _   -> error $ "openZipperToSpan:can't resolve:(sspan,ww)="++(show (sspan,map (\zt -> treeStartEnd $ Z.tree zt) ww))
                    [y] -> openZipperToSpan sspan y
                    yy -> -- Multiple, check if we can separate out by
                          -- version
                          case (filter (\zt -> (fst $ forestSpanVersions $ treeStartEnd $ Z.tree zt) == (fst $ forestSpanVersions sspan)) xx) of
                           -- [] -> z
                           [] -> error $ "openZipperToSpan:no version match:(sspan,yy)=" ++ (show (sspan,yy)) -- ++AZ++
                           [w] -> openZipperToSpan sspan w
                           _ww -> error $ "openZipperToSpan:multiple version match:" ++ (show (sspan,yy)) -- ++AZ++

          contains zn = spanContains (treeStartEnd $ Z.tree zn) sspan

          matchVersions span1 z2 = isMatch
            where
              span2 = treeStartEnd $ Z.tree z2
              isMatch = forestSpanVersions span1 == forestSpanVersions span2

-- ---------------------------------------------------------------------

-- |Open a zipper to a SrcSpan that has been added in the tree, and
-- thus does not necessarily fall in the logical hierarchy of the tree
openZipperToSpanAdded ::
  (IsToken a)
  => ForestSpan
     -> Z.TreePos Z.Full (Entry a)
     -> Z.TreePos Z.Full (Entry a)
openZipperToSpanAdded sspan z = zf
  where
    treeAsList = getTreeSpansAsList $ Z.tree z

    -- True if first span contains the second
    myMatch (((ForestLine _ _ vs1 rs1),cs1),((ForestLine _ _ ve1 re1),ce1))
            (((ForestLine _ _ vs2 rs2),cs2),((ForestLine _ _ ve2 re2),ce2))
      = vs1 == vs2 && ve1 == ve2 && ((rs1,cs1) <= (rs2,cs2)) && ((re1,ce1) >= (re2,ce2))
    tl2 = dropWhile (\(_,s) -> not (myMatch s sspan)) $ reverse treeAsList

    fff [] _ = []
    fff acc@((cd,_cs):_) (v,sspan') = if v < cd then (v,sspan'):acc
                                               else acc

    tl3 = foldl' fff [(head tl2)] tl2
    -- tl3 now contains the chain of ForestSpans to open in order in the zipper

    zf = foldl' (flip openZipperToSpanOrig) z $ map snd tl3

-- ---------------------------------------------------------------------

getTreeSpansAsList :: (IsToken a) => Tree (Entry a) -> [(Int,ForestSpan)]
getTreeSpansAsList = getTreeSpansAsList' 0

getTreeSpansAsList' :: (IsToken a) => Int -> Tree (Entry a) -> [(Int,ForestSpan)]
getTreeSpansAsList' level (Node (Deleted sspan  _pg _eg )  _  )   = [(level,sspan)]
getTreeSpansAsList' level (Node (Entry sspan _lay _toks) ts0) = (level,sspan)
                       : (concatMap (getTreeSpansAsList' (level + 1)) ts0)


-- ---------------------------------------------------------------------

getChildrenAsZ :: Z.TreePos Z.Full a -> [Z.TreePos Z.Full a]
getChildrenAsZ z = go [] (Z.firstChild z)
  where
    go acc Nothing = acc
    go acc (Just zz) = go (acc ++ [zz]) (Z.next zz)

-- ---------------------------------------------------------------------

-- |Replace a single token in the token stream by a new token, without
-- adjusting the layout.
-- Note1: does not re-align, else other later replacements may fail.
-- Note2: must keep original end col, to know what the inter-token gap
--        was when re-aligning
replaceTokNoReAlign:: (IsToken a) => [a] -> SimpPos -> a -> [a]
replaceTokNoReAlign toks pos newTok =
    toks1 ++ [newTok'] ++ toksRest
   where
      (toks1,toks2) = break (\t -> tokenPos t >= pos && tokenLen t > 0) toks
      toksRest = if (null toks2) then [] else (gtail "replaceTokNoReAlign" toks2)
      oldTok  =  if (null toks2) then newTok else (ghead "replaceTokNoReAlign" toks2)
      -- newTok' = markToken $ matchTokenPos oldTok newTok
      newTok' = matchTokenPos oldTok newTok

-- ---------------------------------------------------------------------

-- |Transfer the location information from the first param to the second
matchTokenPos :: (IsToken a) => a -> a -> a
matchTokenPos t1 t2 = putSpan t2 (getSpan t1)

{-
-- |Transfer the location information from the first param to the second
matchTokenPos :: PosToken -> PosToken -> PosToken
matchTokenPos (GHC.L l _,_) (GHC.L _ t,s) = (GHC.L l t,s)
-}

-- ---------------------------------------------------------------------

