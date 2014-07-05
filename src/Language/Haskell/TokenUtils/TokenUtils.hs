module Language.Haskell.TokenUtils.TokenUtils
  (
  -- * Creating
    initTokenCache
  , initTokenCacheLayout
  , mkTreeFromTokens
  , mkTreeFromSpanTokens

  -- * Operations at 'TokenCache' level
  , Positioning (..)
  , ReversedToks(..)
  , reverseToks
  , unReverseToks
  , reversedToks
  , putToksInCache
  , replaceTokenInCache
  , removeToksFromCache
  , replaceTokenForSrcSpan
  , invariant
  , getSrcSpanFor
  , indentDeclToks

  , addToksAfterSrcSpan
  , addOffsetToSpan
  , reIndentToks

  -- *
  , retrieveTokensInterim
  , getTokensForNoIntros
  , getTokensFor
  , getTokensBefore
  -- , retrieveTokensFinal
  , reAlignMarked

  -- *
  , splitOnNewLn
  , getIndentOffset
  , newLnToken
  , startEndLocIncComments'
  , forestSpanToGhcPos

  -- *
  , nullForestSpan
  , nullForestPos


  -- * should be in utils
  , simpPosToForestSpan

  -- *
  , showTree
  , showToks

  -- * Exposed for testing only
  , addNewSrcSpanAndToksAfter
  , openZipperToSpan
  , openZipperToSpanAdded
  , retrievePrevLineToks
  , limitPrevToks
  , insertSrcSpan
  , insertLenChangedInSrcSpan
  , insertVersionsInSrcSpan
  , updateTokensForSrcSpan
  , removeSrcSpan
  , containsStart
  , containsMiddle
  , containsEnd
  , splitSubtree
  -- , splitForestOnSpan
  , insertNodeAfter
  , splitSubToks
  , placeToksForSpan
  , reAlignOneLine
  , calcEndGap
  , getTreeSpansAsList
  , openZipperToSpanOrig
  ) where

import Control.Exception
import Data.Bits
import Data.List
import Data.Tree

import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Utils

import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Tree.Zipper as Z

-- ---------------------------------------------------------------------

-- |Keep track of when tokens are reversed, to avoid confusion
data ReversedToks a = RT [a]
                    deriving (Show)

reverseToks :: (IsToken a) => [a] -> ReversedToks a
reverseToks toks = RT $ reverse toks

unReverseToks :: (IsToken a) => ReversedToks a -> [a]
unReverseToks (RT toks) = reverse toks

reversedToks :: (IsToken a) => ReversedToks a -> [a]
reversedToks (RT toks) = toks


-- |How new SrcSpans should be inserted in the Token tree, relative to
-- the prior span
data Positioning = PlaceAdjacent -- ^Only a single space between the
                   -- end of the prior span and the new one
                 | PlaceAbsolute !Int !Int -- ^Start at the specified
                   -- line and col
                 | PlaceAbsCol !Int !Int !Int -- ^Line offset and
                                              -- absolute Col. Mainly
                                              -- for forcing start at
                                              -- left margin, number
                                              -- of lines to add at
                                              -- the end
                 | PlaceOffset !Int !Int !Int -- ^Line and Col offset for
                   -- start, num lines to add at the end
                   -- relative to the indent level of the prior span
                 | PlaceIndent !Int !Int !Int -- ^Line and Col offset for
                   -- start, num lines to add at the end
                   -- relative to the indent level of the prior line
                 deriving (Show)

-- ---------------------------------------------------------------------

initTokenCache :: (IsToken a) => [a] -> TokenCache a
initTokenCache toks = TK (Map.fromList [((TId 0),(mkTreeFromTokens toks))]) (TId 0)

initTokenCacheLayout :: (IsToken a) => Tree (Entry a) -> TokenCache a
initTokenCacheLayout tree = TK (Map.fromList [((TId 0),tree)]) (TId 0)

-- ---------------------------------------------------------------------

-- |Make a tree representing a particular set of tokens
mkTreeFromTokens :: (IsToken a) => [a] -> Tree (Entry a)
mkTreeFromTokens []   = Node (Entry nullForestSpan NoChange []) []
mkTreeFromTokens toks = Node (Entry sspan NoChange toks) []
  where
   (startLoc',endLoc') = nonCommentSpan toks
   sspan    = if (startLoc',endLoc') == ((0,0),(0,0))
     then error $ "mkTreeFromTokens:null span for:" ++ (show toks)
     else simpPosToForestSpan (startLoc',endLoc')

-- ---------------------------------------------------------------------

-- |Make a tree representing a particular set of tokens
mkTreeFromSpanTokens :: (IsToken a) => ForestSpan -> [a] -> Tree (Entry a)
mkTreeFromSpanTokens sspan toks = Node (Entry sspan NoChange toks) []

-- ---------------------------------------------------------------------

forestSpanStart :: ForestSpan -> ForestPos
forestSpanStart (start,_) = start

forestSpanEnd :: ForestSpan -> ForestPos
forestSpanEnd (_,end) = end

nullForestSpan :: ForestSpan
nullForestSpan = (nullForestPos,nullForestPos)

nullForestPos :: ForestPos
nullForestPos = (ForestLine False 0 0 0,0)

-- ---------------------------------------------------------------------
{-
simpPosToForestSpan :: (SimpPos,SimpPos) -> ForestSpan
simpPosToForestSpan ((sr,sc),(er,ec))
    = ((ghcLineToForestLine sr,sc),(ghcLineToForestLine er,ec))
-}
-- ---------------------------------------------------------------------

-- |Utility function to either return True or throw an error to report the problem
invariantOk :: (IsToken a) => Tree (Entry a) -> Bool
invariantOk forest = ok
  where
    inv = invariant forest
    ok = case inv of
           [] -> True
           _  -> error $ "Token Tree invariant fails:" ++ (intercalate "\n" inv)

-- ---------------------------------------------------------------------
-- |Check the invariant for the token cache. Returns list of any errors found.
-- Invariants:
--   1. For each tree, either the rootLabel has a SrcSpan only, or the subForest /= [].
--   2a. The trees making up the subForest of a given node fully include the parent SrcSpan.
--        i.e. the leaves contain all the tokens for a given SrcSpan.
--   2b. The subForest is in SrcSpan order
--   3. A given SrcSpan can only appear (or be included) in a single tree of the forest.
--   4. The parent link for all sub-trees does exist, and actually points to the parent. 
--   5. There are no nullForestSpan entries in the tree
-- NOTE: the tokens may extend before or after the SrcSpan, due to comments only
-- NOTE2: this will have to be revisited when edits to the tokens are made
invariant :: (IsToken a) => Tree (Entry a) -> [String]
invariant forest = rsub
  where
    rsub = F.foldl checkOneTree [] [forest]

    checkOneTree :: (IsToken a) => [String] -> Tree (Entry a) -> [String]
    checkOneTree acc tree = acc ++ r
      where
        r = checkNode [] tree

    checkNode :: (IsToken a) => [String] -> Tree (Entry a) -> [String]
    checkNode _acc (Node (Deleted _sspan _ _) []) = []
    checkNode _acc node@(Node (Deleted _sspan _ _) _sub)
         = ["FAIL: deleted node with subtree: " ++ (prettyshow node)]
    checkNode acc node@(Node (Entry sspan _lay toks) sub) = acc ++ r ++ rinc ++ rsubs ++ rnull
      where
        r = if (     null toks  && not (null sub)) ||
               (not (null toks) &&      null sub)
              then []
              else ["FAIL: exactly one of toks or subforest must be empty: " ++ (prettyshow node)]
        rsubs = foldl' checkNode [] sub

        rinc = checkInclusion node

        rnull = if (sspan == nullForestSpan)
                 then ["FAIL: null SrcSpan in tree: " ++ (prettyshow node)]
                 else []

    -- |Check invariant 2, assuming 1 ok
    --  NOTE: check that the subtree spans do not go outside the node
    --  span, they do not need to completely fill it, because some may
    --  have been removed during manipulation
    checkInclusion      (Node _                    []) = []
    checkInclusion      (Node (Deleted _ _ _)       _) = []
    checkInclusion node@(Node (Entry _sspan _lay _toks)  sub) = rs ++ rseq
      where
        (start,end) = treeStartEnd node
        subs = map treeStartEnd sub
        (sstart, _) = ghead "invariant" subs
        (_, send) = last subs
        -- Do not count any custom added srcspans at the end for this
        -- test
        -- TODO: is this a reasonable approach?

        rs = if ((start <= sstart) &&
                ((end >= send) || (forestPosVersionSet send) || (forestPosAstVersionSet send)))
                || (forestPosLenChanged start)
                || (forestPosLenChanged sstart)
                || (forestPosLenChanged send)

               then []
               else ["FAIL: subForest start and end does not match entry: " ++ (prettyshow node)]
               -- else ["FAIL: subForest start and end does not match entry: " ++ (show node)]

        rseq = checkSequence node subs

        checkSequence :: (IsToken a) => Tree (Entry a) -> [ForestSpan] -> [String]
        checkSequence _ [] = []
        checkSequence _ [_x] = []
        checkSequence node' ((_s1,e1):s@(s2,_e2):ss)
          = r ++ checkSequence node' (s:ss)
          where
            -- r = if e1 <= s2
            r = if (before e1 s2) || (sizeChanged e1) {- ++AZ++ -} || (sizeChanged s2)
                 then []
                 else ["FAIL: subForest not in order: " ++
                        show e1 ++ " not < " ++ show s2 ++
                        ":" ++ prettyshow node']

            -- |Compare end of one span with beginning of another
            before (ForestLine _chs _trs ve er,ec) (ForestLine _che _tre vs sr,sc)
              = case (ve /= 0, vs /= 0) of
                 (False, False) -> (er,ec) <= (sr,sc) -- e.g. (10,3) <= (11,5)
                 (False, True)  -> True               -- e.g. (10,3) <= (100011,5)
                 (True, False)  -> True               -- e.g. (100010,3) <= (11,5)
                 (True, True)   -> if vs < ve         -- both have version, lowest wins 
                                    then False
                                    else True

            sizeChanged (ForestLine ch _ _ _,_) = ch

{-
     cs    ce
     True  _ -> True
     False _ -> before

-}

-- ---------------------------------------------------------------------

showTree :: (IsToken a) => Tree (Entry a) -> String
showTree = prettyshow

-- |Represent a tree in a more concise/pretty way
prettyshow :: (IsToken a) => Tree (Entry a) -> String
prettyshow (Node (Deleted sspan _pg eg) _nullSubs)
  = "Node (Deleted " ++ (showForestSpan sspan) ++ " " ++ (show eg) ++ ")"
prettyshow (Node (Entry sspan _lay toks) sub)
  = "Node (Entry " ++ (showForestSpan sspan) ++ " "
     ++ (prettyToks toks) ++ ") "
     -- ++ show (map prettyshow sub)
     ++ "[" ++ intercalate "," (map prettyshow sub) ++ "]"

prettyToks :: (IsToken a) => [a] -> String
prettyToks [] = "[]"
prettyToks toks@[_x] = showToks toks
prettyToks toks@[_t1,_t2] = showToks toks
prettyToks toks = showToks [ghead "prettyToks" toks] ++ ".." ++ showToks [last toks]

showToks :: (IsToken a) => [a] -> String
showToks toks = show $ map doOne toks
  where
    doOne tok = (s,e,tok)
      where (s,e) = getSpan tok

-- ---------------------------------------------------------------------

getTreeFromCache :: (IsToken a) => SimpSpan -> TokenCache a -> Tree (Entry a)
getTreeFromCache sspan tk = (tkCache tk) Map.! tid
  where
    tid = treeIdFromForestSpan $ ss2f sspan

-- ---------------------------------------------------------------------

replaceTreeInCache :: (IsToken a) => SimpSpan -> Tree (Entry a) -> TokenCache a -> TokenCache a
replaceTreeInCache sspan tree tk = tk'
  where
    tid = treeIdFromForestSpan $ ss2f sspan
    tree' = putTidInTree tid tree
    tk' = tk {tkCache = Map.insert tid tree' (tkCache tk) }

-- ---------------------------------------------------------------------

-- TODO: get rid of one of the following 2, it is a duplicate
putTidInTree :: (IsToken a) => TreeId -> Tree (Entry a) -> Tree (Entry a)
putTidInTree tid (Node (Deleted fspan pg eg) subs) = (Node (Deleted fs' pg eg) subs)
  where fs' = treeIdIntoForestSpan tid fspan
putTidInTree tid (Node (Entry fspan lay toks) subs) = tree'
  where
    subs' = map (putTidInTree tid) subs
    fs' = treeIdIntoForestSpan tid fspan
    tree' = Node (Entry fs' lay toks) subs'

treeIdIntoTree :: (IsToken a) => TreeId -> Tree (Entry a) -> Tree (Entry a)
treeIdIntoTree tid (Node (Entry fspan lay toks) subTree) = tree'
  where
    fs' = treeIdIntoForestSpan tid fspan
    tree' = Node (Entry fs' lay toks) subTree
treeIdIntoTree tid (Node (Deleted fspan pg eg) subTree) = tree'
  where
    fs' = treeIdIntoForestSpan tid fspan
    tree' = Node (Deleted fs' pg eg) subTree

-- ---------------------------------------------------------------------

stash :: (IsToken a) => TokenCache a -> Tree (Entry a) -> TokenCache a
stash tk oldTree = tk'
  where
    (TId lastTreeId) = tkLastTreeId tk
    lastTreeId' = TId (lastTreeId + 1)
    oldTree' = treeIdIntoTree lastTreeId' oldTree
    cache' = Map.insert lastTreeId' oldTree' (tkCache tk)
    tk' = tk {tkLastTreeId = lastTreeId', tkCache = cache' }

-- ---------------------------------------------------------------------

-- |Replace a single token in a token tree, without changing the
-- structure of the tree
-- NOTE: the GHC.SrcSpan may have been used to select the appropriate
-- forest in the first place, and is required to select the correct
-- span in the tree, due to the ForestLine annotations that may be present

-- TODO: work at the token level, not the sspan level
-- TODO: Use start of token span only, with length 1.
replaceTokenForSrcSpan :: (IsToken a) => Tree (Entry a) -> SimpSpan -> a -> Tree (Entry a)
replaceTokenForSrcSpan forest sspan tok = forest'
  where
    -- (GHC.L tl _,_) = tok
    tl = getSpan tok
    -- First open to the sspan, making use of any Forestline annotations
    z = openZipperToSpanDeep (ss2f sspan) $ Z.fromTree forest

    -- Then drill down to the specific subtree containing the token
    -- z' = openZipperToSpan (srcSpanToForestSpan tl) z
    z' = z -- No, pass in original token span as sspan.

    -- Note: with LayoutTree, the full tree matching the AST has been
    -- built, still need to drill down to the nearest enclosing span
    (tspan,lay,toks) = case Z.tree z' of
       (Node (Entry ss ly tks) []) -> (ss,ly,tks)
       (Node (Entry _ _ _nullToks) _sub) -> error $ "replaceTokenForSrcSpan:tok pos" ++ (showForestSpan $ ss2f sspan) ++ " expecting tokens, found: " ++ (show $ Z.tree z')
       (Node (Deleted _ _ _) _sub)       -> error $ "replaceTokenForSrcSpan:tok pos" ++ (showForestSpan $ ss2f sspan) ++ " expecting Entry, found: " ++ (show $ Z.tree z')

    ((row,col),_) = tl
    toks' = replaceTokNoReAlign toks (row,col) tok

    zf = Z.setTree (Node (Entry tspan lay toks') []) z'
    forest' = Z.toTree zf

-- ---------------------------------------------------------------------

replaceTokenInCache :: (IsToken a) => TokenCache a -> SimpSpan -> a -> TokenCache a
replaceTokenInCache tk sspan tok = tk'
  where
   forest = getTreeFromCache sspan tk
   forest' = replaceTokenForSrcSpan forest sspan tok
   tk' = replaceTreeInCache sspan forest' tk

-- ---------------------------------------------------------------------

putToksInCache :: (IsToken a) => TokenCache a -> SimpSpan -> [a] -> (TokenCache a,SimpSpan)
putToksInCache tk sspan toks = (tk'',newSpan)
  where
   forest = getTreeFromCache sspan tk
   (forest',newSpan,oldTree) = updateTokensForSrcSpan forest sspan toks
   tk' = replaceTreeInCache sspan forest' tk
   tk'' = stash tk' oldTree

-- ---------------------------------------------------------------------

removeToksFromCache :: (IsToken a) => TokenCache a -> SimpSpan -> TokenCache a
removeToksFromCache tk sspan = tk''
  where
    forest = getTreeFromCache sspan tk
    (forest',oldTree) = removeSrcSpan forest (ss2f sspan)
    tk' = replaceTreeInCache sspan forest' tk
    tk'' = stash tk' oldTree

-- ---------------------------------------------------------------------

-- | Removes a ForestSpan and its tokens from the forest.
removeSrcSpan :: (IsToken a) => Tree (Entry a) -> ForestSpan
  -> (Tree (Entry a),Tree (Entry a)) -- ^Updated forest, removed span
removeSrcSpan forest sspan = (forest'', delTree)
  where
    forest' = insertSrcSpan forest sspan -- Make sure span is actually
                                         -- in the tree
    z = openZipperToSpan sspan $ Z.fromTree forest'
    zp = gfromJust "removeSrcSpan" $ Z.parent z

    ((pg,_),eg) = calcPriorAndEndGap forest' sspan

    pt = Z.tree zp

    subTree = map (\t -> if (treeStartEnd t == sspan) then (Node (Deleted sspan pg eg) []) else t) $ subForest pt

    z' = Z.setTree (pt { subForest = subTree}) zp
    forest'' = Z.toTree z'

    delTree = Z.tree z
    -- forest'' = error $ "removeSrcSpan: forest'=" ++ drawTreeCompact forest'

-- ---------------------------------------------------------------------

-- |Add a new SrcSpan and Tokens after a given one in the token stream
-- and forest. This will be given a unique SrcSpan in return, which
-- specifically indexes into the forest.
addNewSrcSpanAndToksAfter :: (IsToken a)
  => Tree (Entry a) -- ^The forest to update
  -> SimpSpan -- ^The new span comes after this one
  -> SimpSpan -- ^Existing span for the tokens
  -> Positioning
  -> [a]  -- ^The new tokens belonging to the new SrcSpan
  -> (Tree (Entry a) -- Updated forest with the new span
     , SimpSpan   ) -- ^Unique SrcSpan allocated in the forest to
                    -- identify this span in its position
addNewSrcSpanAndToksAfter forest oldSpan newSpan pos toks = (forest'',newSpan')
  where
    (forest',tree) = getSrcSpanForDeep forest (ss2f oldSpan)

    (ghcl,_c) = getStartLoc newSpan
    (ForestLine ch tr v l) = ghcLineToForestLine ghcl
    newSpan' = insertForestLineInSpan (ForestLine ch tr (v+1) l) newSpan

    toks' = placeToksForSpan forest' oldSpan tree pos toks

    newNode = Node (Entry (ss2f newSpan') NoChange toks') []

    forest'' = insertNodeAfter tree newNode forest'

-- ---------------------------------------------------------------------

placeToksForSpan :: (IsToken a)
  => Tree (Entry a)
  -> SimpSpan
  -> Tree (Entry a)
  -> Positioning
  -> [a]
  -> [a]
placeToksForSpan forest oldSpan tree pos toks = toks'
  where
    z = openZipperToSpanDeep (ss2f oldSpan) $ Z.fromTree forest
    prevToks = case (retrievePrevLineToks z) of
                 RT [] -> reverseToks $ retrieveTokensInterim tree
                 xs -> xs

    prevToks' = limitPrevToks prevToks oldSpan
    toks' = reIndentToks pos (unReverseToks prevToks') toks
    -- toks' = error $ "placeToksForSpan: prevToks'=" ++ (show prevToks')
    -- toks' = error $ "placeToksForSpan: prevToks=" ++ (show prevToks)

-- ---------------------------------------------------------------------

limitPrevToks :: (IsToken a) => ReversedToks a -> SimpSpan -> ReversedToks a
limitPrevToks prevToks sspan = reverseToks prevToks''
  where
    ((ForestLine _ _ _ startRow,_startCol),(ForestLine _ _ _ endRow,_)) = ss2f sspan

    -- Make sure the toks do not extend past where we are
    prevToks' = dropWhile (\t -> tokenRow t > endRow) $ unReverseToks prevToks

    -- Only use the toks for the given oldspan
    -- prevToks'' = dropWhile (\t -> tokenPos t < (startRow,startCol)) prevToks'
    prevToks'' = dropWhile (\t -> tokenRow t < startRow) prevToks'

-- ---------------------------------------------------------------------

-- |Add new tokens after the given SrcSpan, constructing a new SrcSpan
-- in the process
addToksAfterSrcSpan :: (IsToken a)
  => Tree (Entry a)  -- ^TokenTree to be modified
  -> SimpSpan -- ^Preceding location for new tokens
  -> Positioning
  -> [a] -- ^New tokens to be added
  -> (Tree (Entry a), SimpSpan) -- ^ updated TokenTree and SrcSpan location for
                               -- the new tokens in the TokenTree
addToksAfterSrcSpan forest oldSpan pos toks = (forest',newSpan')
  where
    (fwithspan,tree) = getSrcSpanForDeep forest (ss2f oldSpan)

    toks'' = placeToksForSpan fwithspan oldSpan tree pos toks

    (startPos,endPos) = nonCommentSpan toks''

    newSpan = (startPos,endPos)

    (forest',newSpan') = addNewSrcSpanAndToksAfter forest oldSpan newSpan pos toks

-- ---------------------------------------------------------------------

-- |For a span about to be deleted, calculate the gap between the end
-- of the span being deleted and the start of the next one, at a token
-- level.
calcPriorAndEndGap :: (IsToken a) => Tree (Entry a) -> ForestSpan -> (SimpPos,SimpPos)
calcPriorAndEndGap tree sspan = (pg,eg)
  where
    ((spanStartRow,spanStartCol),(spanRow,spanCol)) = forestSpanToSimpPos sspan
    (spanStart,spanEnd) = sspan
    entries = retrieveTokens' tree
    -- NOTE: the entries are the fringe of the tree, the sspan in
       -- question may be represented by several entries
    (before,rest)    = span  (\e -> (forestSpanStart $ forestSpanFromEntry e) < spanStart) entries
    (rafter,rmiddle) = break (\e -> (forestSpanEnd $ forestSpanFromEntry e) <= spanEnd) $ reverse rest
    _middle = reverse rmiddle
    after = reverse rafter
    -- last element of before should be the sspan we care about, first
    -- of after is the one we are looking for.

    -- NOTE: `after` may contain zero or more Deleted segments in the
    -- front. These get merged later in mergeDeletes
    (tokRow,tokCol) = if null after
        then (spanRow + 2,spanCol)
        else (r,c)
            where
                (r,c) = case ghead ("calcEndGap:after="++(show after)) after of
                    (Entry _ _ toks) -> (tokenRow t,tokenCol t)
                        where t = ghead "calcEndGap" toks
                    (Deleted ss _ _) -> fst $ forestSpanToSimpPos ss

    eg = (tokRow - spanRow, tokCol - spanCol)
    -- eg = error $ "calcEndGap: (sspan,(before,middle,after))=" ++ (show (sspan,(_before,middle,after)))

    (tokRowPg,tokColPg) = if null before
        then (spanStartRow - 1,spanStartCol)
        else (r,c)
            where
                (r,c) = case glast ("calcEndGap:before="++(show before)) before of
                    (Entry _ _ toks) -> (tokenRow t,tokenCol t)
                        where t = glast "calcEndGap pg" toks
                    (Deleted ss _ _) -> snd $ forestSpanToSimpPos ss

    -- TODO: what about comments before the span? spanStartRow may be off
    pg = (spanStartRow - tokRowPg, spanStartCol - tokColPg)


-- ---------------------------------------------------------------------

-- TODO: delete this, superseded by calcPriorAndEndGap
-- |For a span about to be deleted, calculate the gap between the end
-- of the span being deleted and the start of the next one, at a token
-- level.
calcEndGap :: (IsToken a) => Tree (Entry a) -> ForestSpan -> SimpPos
calcEndGap tree sspan = gap
  where
    (_sspanStart,(spanRow,spanCol)) = forestSpanToSimpPos sspan
    (spanStart,spanEnd) = sspan
    entries = retrieveTokens' tree
    -- NOTE: the entries are the fringe of the tree, the sspan in
       -- question may be represented by several entries
    (_before,rest)   = span  (\e -> (forestSpanStart $ forestSpanFromEntry e) < spanStart) entries
    (rafter,rmiddle) = break (\e -> (forestSpanEnd $ forestSpanFromEntry e) <= spanEnd) $ reverse rest
    _middle = reverse rmiddle
    after = reverse rafter
    -- last element of before should be the sspan we care about, first
    -- of after is the one we are looking for.

    -- NOTE: `after` may contain zero or more Deleted segments in the
    -- front. These get merged later in mergeDeletes
    (tokRow,tokCol) = if null after
        then (spanRow + 2,spanCol)
        else (r,c)
            where
                (r,c) = case ghead ("calcEndGap:after="++(show after)) after of
                    (Entry _ _ toks) -> (tokenRow t,tokenCol t)
                        where t = ghead "calcEndGap" toks
                    (Deleted ss _ _) -> fst $ forestSpanToSimpPos ss

    gap = (tokRow - spanRow, tokCol - spanCol)
    -- gap = error $ "calcEndGap: (sspan,(before,middle,after))=" ++ (show (sspan,(_before,middle,after)))

-- ---------------------------------------------------------------------

-- |Replace the tokens for a given SrcSpan with new ones. The SrcSpan
-- will be inserted into the tree if it is not already there.
-- If the SrcSpan changes size, replace the SrcSpan with a new one
-- (marked), and return it, as well as the old one
-- TODO: What about trailing comments? Preserve or replace?
updateTokensForSrcSpan :: (IsToken a)
  => Tree (Entry a) -> SimpSpan -> [a] -> (Tree (Entry a),SimpSpan,Tree (Entry a))
updateTokensForSrcSpan forest sspan toks = (forest'',newSpan,oldTree)
  where
    (forest',tree@(Node (Entry _s _ _) _)) = getSrcSpanFor forest (ss2f sspan)
    prevToks = retrieveTokensInterim tree

    endComments   = reverse $ takeWhile isWhiteSpaceOrIgnored $ reverse toks
    startComments = takeWhile isWhiteSpaceOrIgnored $ toks

    newTokStart = if (null prevToks)
                   then mkZeroToken
                   else ghead "updateTokensForSrcSpan.1" prevToks

    toks'' = if (not (null startComments) || not (null endComments))
      then -- toks have comments, discard originals
           reIndentToks (PlaceAbsolute (tokenRow newTokStart) (tokenCol newTokStart)) prevToks toks
      else -- Must reuse any pre-existing start or end comments, and
           -- resync the tokens across all three.
        let
           origEndComments   = reverse $ takeWhile isWhiteSpaceOrIgnored $ reverse prevToks
           origStartComments = takeWhile isWhiteSpaceOrIgnored $ prevToks

           ((startRow,startCol),_) = forestSpanToGhcPos $ ss2f sspan
           core = reIndentToks (PlaceAbsolute startRow startCol) prevToks toks
           trail = if (null origEndComments)
            then []
            else addOffsetToToks (lineOffset,colOffset) origEndComments
              where
                lineOffset = 0 -- tokenRow (head origEndComments) - tokenRow (head origEndComments)
                colOffset = 0 -- tokenCol (head origEndComments)

           toks' = origStartComments ++ core ++ trail
        in toks'

    (startPos,endPos) = nonCommentSpan toks''

    -- if the original sspan had a ForestLine version, preserve it
    (((ForestLine _chs _trs vs _),_),(ForestLine _che _tre ve _,_)) = ss2f sspan
    -- Note: adding one to end version, so invariant won't fail
    -- newSpan = insertVersionsInSrcSpan vs ve $ posToSrcSpan forest (startPos,endPos) 
    newSpan = insertLenChangedInSrcSpan True True
            $ insertVersionsInSrcSpan vs ve $ (startPos,endPos)

    zf = openZipperToNode tree $ Z.fromTree forest'

    zf' = Z.setTree (Node (Entry (ss2f newSpan) NoChange toks'') []) zf
    forest'' = Z.toTree zf'

    oldTree = tree

-- ---------------------------------------------------------------------

-- |Get the tokens preceding a given 'SrcSpan'
getTokensBefore :: (IsToken a) => Tree (Entry a) -> SimpSpan -> (Tree (Entry a),ReversedToks a)
getTokensBefore forest sspan = (forest', prevToks')
  where
    (forest',tree@(Node (Entry _s _ _) _)) = getSrcSpanFor forest (ss2f sspan)

    z = openZipperToSpan (ss2f sspan) $ Z.fromTree forest'

    prevToks = case (retrievePrevLineToks z) of
                 RT [] -> reverseToks $ retrieveTokensInterim tree
                 xs -> xs

    (_,rtoks) = break (\t->tokenPos t < (getStartLoc sspan)) $ reversedToks prevToks
    prevToks' = RT rtoks

-- ---------------------------------------------------------------------

-- |Get the (possible cached) tokens for a given source span, and
-- cache their being fetched.
-- NOTE: The SrcSpan may be one introduced by HaRe, rather than GHC.
getTokensFor :: (IsToken a) => Bool -> Tree (Entry a) -> SimpSpan -> (Tree (Entry a),[a])
getTokensFor checkInvariant forest sspan = (forest'', tokens)
  where
     forest' = if (not checkInvariant) || invariantOk forest
                -- short circuit eval
               then forest
               else error $ "getTokensFor:invariant failed:" ++ (show $ invariant forest)
     (forest'',tree) = getSrcSpanFor forest' (ss2f sspan)

     tokens = retrieveTokensInterim tree

-- ---------------------------------------------------------------------

-- |Get the (possible cached) tokens for a given source span, and
-- cache their being fetched.
-- NOTE: The SrcSpan may be one introduced by HaRe, rather than GHC.
getTokensForNoIntros :: (IsToken a) => Bool -> Tree (Entry a) -> SimpSpan -> (Tree (Entry a),[a])
getTokensForNoIntros checkInvariant forest sspan = (forest', tokens')
  where
    (forest',tokens) = getTokensFor checkInvariant forest sspan
    -- (lead,rest) = break (not . isWhiteSpaceOrIgnored) tokens
    (lead,rest) = break (not . isIgnoredNonComment) tokens
    tokens' = (filter (not . isIgnored) lead) ++ rest

-- ---------------------------------------------------------------------

-- |Starting from a point in the zipper, retrieve all tokens backwards
-- until the line changes for a non-comment/non-empty token or
-- beginning of file.
retrievePrevLineToks :: (IsToken a) => Z.TreePos Z.Full (Entry a) -> ReversedToks a
retrievePrevLineToks z = RT res' -- error $ "retrievePrevLineToks:done notWhite=" ++ (show (done notWhite)) -- ++AZ++
  where
    -- Assuming the zipper has been opened to the span we care about,
    -- we will start with the tokens in the current tree, and work
    -- back.
    -- prevToks = retrieveTokens $ Z.tree z
    prevToks = retrieveTokensInterim $ Z.tree z

    -- Next one is the usual one
    -- res' = reverse $ (concat (go z)) ++ prevToks
    res' =  reverse $ concat $ reverse (prevToks : (go z))

    -- TODO:  ++AZ++ what is this actually doing?
    go :: (IsToken a) => Z.TreePos Z.Full (Entry a) -> [[a]]
    go zz
      | not (Z.isRoot zz) = toks : (go $ gfromJust "retrievePrevLineToks" (Z.parent zz))
      | otherwise = [toks]
      where
        toks = concat $ reverse $ map retrieveTokensInterim $ Z.before zz
        -- toks = concat $ map retrieveTokensInterim $ Z.before zz


-- ---------------------------------------------------------------------
{-
posToSpan ::  (SimpPos,SimpPos) -> Span
posToSpan (s,e) = Span s e
-}
-- ---------------------------------------------------------------------

reIndentToks :: (IsToken a) => Positioning -> [a] -> [a] -> [a]
reIndentToks _ _ [] = []
reIndentToks pos prevToks toks = toks''
  where
    newTokStart = ghead "reIndentToks.1"
                $ dropWhile (\tok -> isComment tok || isEmpty tok) $ toks

    firstTok = ghead "reIndentToks.2" toks
    lastTok  = glast "reIndentToks.1" prevToks

    lastNonCommentTok = ghead "reIndentToks.3"
                      $ dropWhile (\tok -> isComment tok || isEmpty tok) $ reverse prevToks

    prevOffset = getIndentOffset prevToks (tokenPos (glast "reIndentToks.2" prevToks))

    (lastTokEndLine,_) = tokenPosEnd lastTok


    (lineOffset,colOffset,endNewlines) = case pos of
      PlaceAdjacent -> (lineOffset',colOffset',0)
        where
          colStart  = (tokenColEnd (lastTok)) + 1
          lineStart = (tokenRow    (lastTok))

          lineOffset' = lineStart - (tokenRow firstTok)
          colOffset'  = colStart  - (tokenCol newTokStart)

      PlaceAbsolute row col -> (lineOffset', colOffset', 0)
        where
          lineOffset' = row - (tokenRow firstTok)
          colOffset'  = col - (tokenCol firstTok)

      PlaceAbsCol rowIndent col numLines -> (lineOffset', colOffset', numLines)
        where
          colOffset'  = col - (tokenCol firstTok)
          lineStart = (tokenRow (lastTok)) -- + 1

          lineOffset' = rowIndent + lineStart - (tokenRow firstTok)

      PlaceOffset rowIndent colIndent numLines -> (lineOffset',colOffset',numLines)
        where
          -- TODO: Should this not be prevOffset?
          colStart  = tokenCol $ ghead "reIndentToks.4"
                    $ dropWhile isWhiteSpaceOrIgnored prevToks

          lineStart = (tokenRow (lastTok)) -- + 1

          lineOffset' = rowIndent + lineStart - (tokenRow firstTok)
          colOffset'  = colIndent + colStart  - (tokenCol newTokStart)
          -- colOffset'  = error $ "reIndentToks:placeOffset lineOffset=" ++ show lineOffset

      PlaceIndent rowIndent colIndent numLines -> (lineOffset',colOffset',numLines)
        where
          colStart = prevOffset
          lineStart = if ((isComment lastTok) && (tokenRow lastNonCommentTok /= lastTokEndLine))
              then (tokenRow (lastTok)) + 1
              else (tokenRow (lastTok))

          lineOffset' = rowIndent + lineStart - (tokenRow firstTok)
          colOffset'  = colIndent + colStart  - (tokenCol newTokStart) + 1 -- ++AZ++ Why +1?

    toks'  = addOffsetToToks (lineOffset,colOffset) toks
    toks'' = if endNewlines > 0
               then toks' ++ [(newLinesToken (endNewlines - 1) $ glast "reIndentToks.3" toks')]
               else toks'

-- ---------------------------------------------------------------------

-- | indent the tree and tokens by the given offset, and sync the AST
-- to the tree too.
indentDeclToks :: (IsToken a,HasLoc t) --(SYB.Data t)
  => (t -> ForestSpan -> t)
  -> t -- ^The AST (or fragment)
  -> Tree (Entry a)    -- ^Existing token tree
  -> Int           -- ^ (signed) number of columns to indent/dedent
  -> (t, Tree (Entry a)) -- ^Updated AST and tokens
indentDeclToks syncAST decl forest offset = (decl',forest'')
  where
    -- sspan = posToSpan (getLoc decl,getLocEnd decl)
    sspan = getSpan decl

    -- make sure the span is in the forest
    (forest',tree) = getSrcSpanFor forest (ss2f sspan)

    z = openZipperToSpan (ss2f sspan) $ Z.fromTree forest'

    tree' = go tree
    -- The invariant will fail if we do not propagate this change
    -- upward. But it needs to sync with the AST, which we do not have
    -- the upward version of.
    -- Instead, set the lengthChanged flag, in the parent.

    -- sss = forestSpanFromEntry entry
    -- sss' = insertLenChangedInForestSpan True sss
    -- tree'' = Node (putForestSpanInEntry entry sss') subs

    markLenChanged (Node entry subs) = (Node entry' subs)
      where
        sss = forestSpanFromEntry entry
        sss' = insertLenChangedInForestSpan True sss
        entry' = putForestSpanInEntry entry sss'

    z' = Z.setTree tree' z
    -- forest'' = Z.toTree (Z.setTree tree'' z)

    forest'' = case Z.parent z' of
                Nothing  -> Z.toTree (Z.setTree (markLenChanged $ Z.tree z' ) z' )
                Just z'' -> Z.toTree (Z.setTree (markLenChanged $ Z.tree z'') z'')


    -- decl' = syncAST decl (addOffsetToSpan off sspan) tree
    decl' = syncAST decl (ss2f $ addOffsetToSpan off sspan)

    off = (0,offset)

    -- Pretty sure this could be a fold of some kind
    go (Node (Deleted ss pg eg) sub) = (Node (Deleted (addOffsetToForestSpan off ss) pg eg) sub)
    go (Node (Entry ss lay [])  sub) = (Node (Entry (addOffsetToForestSpan off ss) lay []) (map go sub))
    go (Node (Entry ss lay toks) []) = (Node (Entry (addOffsetToForestSpan off ss) lay (addOffsetToToks off toks)) [])
    go n = error $ "indentDeclToks:strange node:" ++ (show n)

-- ---------------------------------------------------------------------

addOffsetToSpan :: (Int,Int) -> SimpSpan -> SimpSpan
addOffsetToSpan (lineOffset,colOffset) sspan = sspan'
  where
   ((sl,sc),(el,ec)) = sspan
   sspan' = ((sl+lineOffset,sc+colOffset),(el+lineOffset,ec+colOffset))

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
-- |Retrieve a path to the tree containing a ForestSpan from the forest,
-- inserting it if not already present
getSrcSpanFor :: (IsToken a) => Tree (Entry a) -> ForestSpan -> (Tree (Entry a), Tree (Entry a))
getSrcSpanFor forest sspan = (forest',tree)
  where
    forest' = insertSrcSpan forest sspan -- Will NO-OP if already
                                         -- there
    z = openZipperToSpan sspan $ Z.fromTree forest'
    tree = Z.tree z

-- ---------------------------------------------------------------------
-- |Retrieve a path to the tree containing a ForestSpan from the forest,
-- inserting it if not already present.
-- In the case where there is a nested series of spans as in an
-- 'Above' layout, return the deepest one
getSrcSpanForDeep :: (IsToken a)
 => Tree (Entry a) -> ForestSpan -> (Tree (Entry a), Tree (Entry a))
getSrcSpanForDeep forest sspan = (forest',tree)
  where
    forest' = insertSrcSpan forest sspan -- Will NO-OP if already
                                         -- there
    z = openZipperToSpanDeep sspan $ Z.fromTree forest'

    tree = Z.tree z

-- ---------------------------------------------------------------------
-- |Insert a ForestSpan into the forest, if it is not there already.
-- Assumes the forest was populated with the tokens containing the
-- ForestSpan already
insertSrcSpan :: (IsToken a) => Tree (Entry a) -> ForestSpan -> Tree (Entry a)
insertSrcSpan forest sspan = forest'
  where
    z = openZipperToSpan sspan $ Z.fromTree forest
    forest' = if treeStartEnd (Z.tree z) == sspan
      then forest -- Already in, exactly
      -- else error $ "insertSrcSpan:span not in tree " ++ show sspan

      else if (Z.isLeaf z)
        then  -- TODO: This should be in splitSubToks
          let
            -- If we are at a leaf, retrieve the toks
            (Entry _ _ toks) = Z.label z

            (tokStartPos,tokEndPos) = forestSpanToSimpPos sspan

            -- Tokens here, must introduce sub-spans with split, taking
            -- cognizance of start and end comments
            -- TODO: does startEndLocIncComments' give the same boundary
            --       if approached from one side as the other?
            (startLoc,endLoc) = startEndLocIncComments' toks (tokStartPos,tokEndPos)

            (startToks,middleToks,endToks) = splitToks (startLoc,endLoc) toks

            tree1 = if (nonCommentSpan startToks == ((0,0),(0,0)))
                       then []
                       else [mkTreeFromTokens startToks]
            tree2 = [mkTreeFromSpanTokens sspan middleToks]

            tree3 = if (nonCommentSpan endToks == ((0,0),(0,0)))
                       then []
                       else [mkTreeFromTokens endToks]

            subTree = tree1 ++ tree2 ++ tree3
            subTree' = filter (\t -> treeStartEnd t /= nullForestSpan) subTree
            -- (Entry sspan2 _ _) = Z.label z
            sspan2 = case Z.label z of
              (Entry ss _ _) -> ss
              (Deleted ss _ _) -> ss


            -- z' = Z.setTree (Node (Entry sspan2 NoChange []) subTree') z
            z' = case Z.label z of
              (Entry _ _ _) -> Z.setTree (Node (Entry sspan2 NoChange []) subTree') z
              (Deleted _ _ _) -> Z.setTree (Node (Entry sspan2 NoChange []) subTreeD) z
                where
                  (tb,tm,te) = splitSubToks (Z.tree z) sspan
                  subTreeD = tb ++ tm ++ te

            forest'' = Z.toTree z'
          in forest''
        else
          let
            (before,middle,end) = doSplitTree (Z.tree z) sspan
            newTree = case middle of
                        [x] -> x
                        _xs -> (Node (Entry sspan NoChange []) middle)
            subTree' = before ++ [newTree] ++ end
            (Entry sspan2 _ _) = Z.label z

            z' = Z.setTree (Node (Entry sspan2 NoChange []) subTree') z
            forest'' = Z.toTree z'
          in
            forest''


-- ---------------------------------------------------------------------

doSplitTree :: (IsToken a)
  => Tree (Entry a) -> ForestSpan
  -> ([Tree (Entry a)], [Tree (Entry a)], [Tree (Entry a)])
doSplitTree tree@(Node (Deleted _ss _ _)   []) sspan = splitSubToks tree sspan -- ++AZ+ What is correct?
doSplitTree tree@(Node (Entry _ss _ _toks) []) sspan = splitSubToks tree sspan
doSplitTree tree                               sspan = (b'',m'',e'')
 -- error $ "doSplitTree:(sspan,tree,(b1,m1,e1))=" ++ (show (sspan,tree,(b1,m1,e1)))
  where
    (b1,m1,e1) = splitSubtree tree sspan
    (b,m,e) = case m1 of
      [] -> -- NOTE: This may have happened through a span being
            --       deleted from the tree
            -- Hence, correct solution is to kick it up a level and
            -- rebuild using tokens etc
             error $ "doSplitTree:no middle:(tree,sspan,b1,m1,e1)=" ++ (show (tree,sspan,b1,m1,e1))
      [x] -> -- only one tree
             doSplitTree x sspan

      xx  -> -- more than one tree
        (b',m',e')
          where
            (bb,mb,_eb) = case (doSplitTree (ghead "doSplitTree.2" xx) sspan) of
                           (x,y,[]) -> (x,y,[])
                           xxx -> error $ "doSplitTree:eb populated:" ++ (show (sspan,tree,xxx))


            ( [],me,ee) = doSplitTree (glast "doSplitTree.2" xx) sspan

            mm = tail $ init xx -- xx = (head xx) ++ mm ++ (last xx)

            b' = bb
            m' = mb ++ mm ++ me
            e' = ee
    (b'',m'',e'') = (b1++b,m,e++e1)


-- ---------------------------------------------------------------------

-- TODO: The Bool is horrible
mkTreeListFromTokens :: (IsToken a) => [a] -> ForestSpan -> Bool -> [Tree (Entry a)]
mkTreeListFromTokens  [] _sspan _ = []
mkTreeListFromTokens toks sspan useOriginalSpan = res
  where
   (Node (Entry tspan NoChange treeToks) sub) = mkTreeFromTokens toks

   ((ForestLine chs ts vs  _, _),(ForestLine che te ve  _, _)) = sspan
   ((ForestLine   _  _  _ ls,cs),(ForestLine   _  _  _ le,ce)) = tspan

   span' = ((ForestLine chs ts vs ls, cs),(ForestLine che te ve le, ce))

   res = if nonCommentSpan toks == ((0,0),(0,0))
     then []
     else if useOriginalSpan
            then [(Node (Entry sspan NoChange treeToks) sub)]
            else [(Node (Entry span' NoChange treeToks) sub)]

-- ---------------------------------------------------------------------

splitSubToks :: (IsToken a)
  => Tree (Entry a)
  -> (ForestPos, ForestPos)
  -> ([Tree (Entry a)], [Tree (Entry a)], [Tree (Entry a)])
splitSubToks n@(Node (Deleted (treeStart,treeEnd) _pg _eg) []) (sspanStart,sspanEnd) = (b',m',e')
  where
    egs = (0,0) -- TODO: calculate this
    ege = (0,0) -- TODO: calculate this
    pg = 0      -- TODO: calculate this
    b' = if sspanStart > treeStart
           then [Node (Deleted (treeStart,treeStart) pg egs) []]
           else []
    m' = [n]
    e' = if treeEnd > sspanEnd
           then [Node (Deleted (sspanEnd,treeEnd) pg ege) []]
           else []

splitSubToks tree sspan = (b',m',e')
                          -- error $ "splitSubToks:(sspan,tree)=" ++ (show (sspan,tree))
  where
    (Node (Entry ss@(treeStart,treeEnd) _lay toks)  []) = tree
    (sspanStart,sspanEnd) = sspan
    -- TODO: ignoring comment boundaries to start

    -- There are three possibilities
    --  1. The span starts only in these tokens
    --  2. The span starts and ends in these tokens
    --  3. The span ends only in these tokens
    (b',m',e') = case (containsStart ss sspan,containsEnd ss sspan) of
      (True, False) -> (b'',m'',e'') -- Start only
                       -- error $ "splitSubToks:StartOnly:(sspan,tree,(b'',m''))=" ++ (show (sspan,tree,(b'',m'')))
        where
         (_,toksb,toksm) = splitToks (forestSpanToSimpPos (nullForestPos,sspanStart)) toks
--         b'' = if (emptyList toksb) then [] else [Node (Entry (treeStart, sspanEnd) toksb) []]
         b'' = if (null toksb || nonCommentSpan toksb == ((0,0),(0,0)))
                 then []
                 else [mkTreeFromTokens toksb] -- Need to get end from actual toks
         m'' = let
                (ForestLine _ch _ts _v le,ce) = sspanEnd
                tl =
                  if (treeStart == sspanStart) -- Eq does not compare all flags
                    then mkTreeListFromTokens toksm (treeStart, treeEnd) False
                    else mkTreeListFromTokens toksm (sspanStart,treeEnd) False
                _tl' = if null tl
                 then []
                 else [Node (Entry (st,(ForestLine ch ts v le,ce)) lay tk) []]
                   where [Node (Entry (st,(ForestLine ch ts v _l,_c)) lay tk) []] = tl
               in
                -- tl'
                tl
         e'' = []

      (True, True) -> (b'',m'',e'') -- Start and End
        where
         (toksb,toksm,tokse) = splitToks (forestSpanToSimpPos (sspanStart,sspanEnd)) toks
         b'' = mkTreeListFromTokens toksb (treeStart,  sspanStart) False
         m'' = mkTreeListFromTokens toksm (sspanStart, sspanEnd)   True
         e'' = mkTreeListFromTokens tokse (sspanEnd,   treeEnd)    False

      (False,True) -> (b'',m'',e'') -- End only
        where
         (_,toksm,tokse) = splitToks (forestSpanToSimpPos (nullForestPos,sspanEnd)) toks
         b'' = []
         m'' = let -- If the last span is changed, make sure it stays
                   -- as it was
                tl = mkTreeListFromTokens toksm (treeStart,sspanEnd) False
                tl' = if null tl
                 then []
                 else [Node (Entry (st,sspanEnd) lay tk) []]
                   where [Node (Entry (st,_en) lay tk) []] = mkTreeListFromTokens toksm (treeStart,sspanEnd) False
                in
                 tl'
         e'' = mkTreeListFromTokens tokse (sspanEnd,treeEnd) False

      (False,False) -> if (containsMiddle ss sspan)
                        then ([],[tree],[])
                        else error $ "splitSubToks: error (ss,sspan)=" ++ (show (ss,sspan))

-- ---------------------------------------------------------------------

-- |True if the start of the second param lies in the span of the first
containsStart :: ForestSpan -> ForestSpan -> Bool
containsStart (nodeStart,nodeEnd) (startPos,_endPos)
  = (startPos >= nodeStart && startPos <= nodeEnd)

-- |True if the start of the second param lies before the first, and
-- ends after or on the second
containsMiddle :: ForestSpan -> ForestSpan -> Bool
containsMiddle   (nodeStart,nodeEnd) (startPos,endPos)
  = (startPos <= nodeStart) && (endPos >= nodeEnd)

-- |True if the end of the second param lies in the span of the first
containsEnd :: ForestSpan -> ForestSpan -> Bool
containsEnd   (nodeStart,nodeEnd) (_startPos,endPos)
  = (endPos >= nodeStart && endPos <= nodeEnd)

-- ---------------------------------------------------------------------

-- |Split a given tree into a possibly empty part that lies before the
-- srcspan, the part that is wholly included in the srcspan and the
-- part the lies outside of it at the end.
splitSubtree :: (IsToken a)
  => Tree (Entry a) -> ForestSpan
  -> ([Tree (Entry a)], [Tree (Entry a)], [Tree (Entry a)])
splitSubtree tree sspan = (before,middle,end)
                          -- error $ "splitSubtree:(sspan,tree,middle',end')=" ++ (show (sspan,tree,middle',end'))
  where
    containsStart'  t = containsStart  (treeStartEnd t) sspan
    containsMiddle' t = containsMiddle (treeStartEnd t) sspan
    containsEnd'    t = containsEnd    (treeStartEnd t) sspan

    cond t = containsStart' t || containsMiddle' t || containsEnd' t

    (Node _entry children) = tree
    (before,rest)   = break (\x -> cond x) children
    (endr,middler)  = break (\x -> cond x) $ reverse rest

    (middle,end) = (reverse middler,reverse endr)

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

-- |Insert a new node after the designated one in the tree
insertNodeAfter :: (IsToken a)
  => Tree (Entry a) -> Tree (Entry a) -> Tree (Entry a) -> Tree (Entry a)
insertNodeAfter oldNode newNode forest = forest'
  where
    zf = openZipperToNodeDeep oldNode $ Z.fromTree forest

    zp = gfromJust ("insertNodeAfter:" ++ (show (oldNode,newNode,forest))) $ Z.parent zf
    tp = Z.tree zp

    -- now go through the children of the parent tree, and find the
    -- right spot for the new node
    (f,s) = break (\t -> treeStartEnd t == treeStartEnd oldNode) $ subForest tp
    (f',s') = (f++[ghead "insertNodeAfter" s],tail s) -- break does not include the found point
    subForest' = f' ++ [newNode] ++ s'

    tp' = tp { subForest = subForest' }
    forest' = Z.toTree $ Z.setTree tp' zp

-- ---------------------------------------------------------------------

-- |Open a zipper so that its focus is the given node
--  NOTE: the node must already be in the tree
openZipperToNode :: (IsToken a)
  => Tree (Entry a)
     -> Z.TreePos Z.Full (Entry a)
     -> Z.TreePos Z.Full (Entry a)
openZipperToNode (Node (Entry sspan _ _) _) z
  = openZipperToSpan sspan z
openZipperToNode (Node (Deleted sspan _ _) _) z
  = openZipperToSpan sspan z

-- |Open a zipper so that its focus is the given node
--  NOTE: the node must already be in the tree
openZipperToNodeDeep :: (IsToken a)
  => Tree (Entry a)
     -> Z.TreePos Z.Full (Entry a)
     -> Z.TreePos Z.Full (Entry a)
openZipperToNodeDeep (Node (Entry sspan _ _) _) z
  = openZipperToSpanDeep sspan z
openZipperToNodeDeep (Node (Deleted sspan _ _) _) z
  = openZipperToSpanDeep sspan z

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

-- ---------------------------------------------------------------------

-- |Retrieve all the tokens at the leaves of the tree, in order. No
-- adjustments are made to address gaps or re-alignment of the tokens
retrieveTokensInterim :: (IsToken a) => Tree (Entry a) -> [a]
retrieveTokensInterim forest = monotonicLineToks $ stripForestLines {-  reAlignMarked -}
                             $ concat $ map (\t -> F.foldl accum [] t) [forest]
  where
    accum :: [a] -> (Entry a) -> [a]
    accum acc (Entry _ _ [])   = acc
    accum acc (Entry _ _ toks) = acc ++ toks
    accum acc (Deleted _ _ _)  = acc


retrieveTokens' :: (IsToken a) => Tree (Entry a) -> [Entry a]
retrieveTokens' forest = mergeDeletes $ concat $ map (\t -> F.foldl accum [] t) [forest]
  where
    accum :: [Entry a] -> Entry a -> [Entry a]
    accum acc   (Entry _ _ [])    = acc
    accum acc e@(Entry _ _ _toks) = acc ++ [e]
    accum acc e@(Deleted _ _ _)   = acc ++ [e]

-- |Merge adjacent Deleted entries
mergeDeletes :: (IsToken a) => [Entry a] -> [Entry a]
mergeDeletes [] = []
mergeDeletes [x] = [x]
mergeDeletes ((Deleted ss1 pg1 (r1,_)):(Deleted ss2 _ (r2,c2)):xs) = (Deleted ss pg1 o):xs
  where
    (start,_) = ss1
    (_, end) = ss2
    ss = (start,end)
    o = (r1+r2,c2)
mergeDeletes (x:xs) = x:mergeDeletes xs

-- ---------------------------------------------------------------------

-- | sort out line numbering so that they are always monotonically
-- increasing.
monotonicLineToks :: (IsToken a) => [a] -> [a]
monotonicLineToks toks = goMonotonicLineToks (0,0) toks

goMonotonicLineToks :: (IsToken a) => SimpPos -> [a] -> [a]
goMonotonicLineToks _ [] = []
goMonotonicLineToks _ [t] = [t]
goMonotonicLineToks (orow,ocol) (t1:t2:ts)
  = t1:goMonotonicLineToks offset' (t2':ts)
  where
    offset' = if (tokenRow t1 - orow) > (tokenRow t2)
               then (orow + (tokenRow t1) - tokenRow t2 + 1, ocol)
               else (orow,ocol)

    -- t1' = increaseSrcSpan (orow,ocol) t1
    t2' = increaseSrcSpan offset'     t2

-- ---------------------------------------------------------------------
{- No longer used
-- |Retrieve all the tokens at the leaves of the tree, in order.
-- Marked tokens are re-aligned, and gaps are closed.
retrieveTokensFinal :: (IsToken a) => Tree (Entry a) -> [a]
retrieveTokensFinal forest = monotonicLineToks $ stripForestLines $ reAlignMarked
                      $ deleteGapsToks $ retrieveTokens' forest
-}
-- ---------------------------------------------------------------------

reAlignMarked :: (IsToken a) => [a] -> [a]
reAlignMarked toks = concatMap alignOne $ groupTokensByLine toks
  where
    -- alignOne toksl = unmarked ++ (reAlignToks marked)
    alignOne toksl = unmarked ++ (reAlignOneLine marked)
     where
       (unmarked,marked) = break isMarked toksl

-- ---------------------------------------------------------------------

-- | Some tokens are marked if they belong to identifiers which have
-- been renamed. When the renaming takes place, no layout adjustment
-- is done. This function adjusts the spacing for the rest of the line
-- to match as far as possible the original spacing, except for the
-- name change.
reAlignOneLine :: (IsToken a) => [a] -> [a]
reAlignOneLine toks = go (0,0) toks
  where
    go _ [] = []
    go (l,c) (t:ts) = (increaseSrcSpan (l,c) t') : (go (l,c') ts)
      where
        (t',dc) = adjustToken t
        c' = c + dc

    adjustToken tt
      | tokenLen tt == 0 = (tt,0)
      | otherwise = (tt',deltac)
      where
        ((sl,sc),(el,ec)) = getSpan tt
        deltac = (tokenLen tt) - (ec - sc)
        newPos = ((sl,sc),(el,ec+deltac))
        tt' = putSpan tt newPos

-- ---------------------------------------------------------------------

applyOffsetToTreeShallow :: (IsToken a) => (Int,Int) -> Tree (Entry a) -> Tree (Entry a)
applyOffsetToTreeShallow (ro,co) (Node (Entry sspan lay toks) subs)
  = (Node (Entry sspan' lay toks') subs')
  where
    sspan' = addOffsetToForestSpan (ro,co) sspan
    toks' = addOffsetToToks (ro,co) toks
    subs' = subs
applyOffsetToTreeShallow _ n@(Node (Deleted _ _ _) _) = n

-- ---------------------------------------------------------------------

addOffsetToForestSpan :: (Int,Int) -> ForestSpan -> ForestSpan
addOffsetToForestSpan (lineOffset,colOffset) fspan = fspan'
  where
    ((ForestLine sch str sv sl,sc),(ForestLine ech etr ev el,ec)) = fspan
    fspan' = ((ForestLine sch str sv (sl+lineOffset),sc+colOffset),
              (ForestLine ech etr ev (el+lineOffset),ec+colOffset))

-- ---------------------------------------------------------------------

stripForestLines :: (IsToken a) => [a] -> [a]
stripForestLines toks = map doOne toks
  where
    doOne tok = tok'
      where
       l = getSpan tok
       tok' = putSpan tok l'
       ((ForestLine _ _ _ ls,_),(_,_)) = ss2f l
       l' = insertForestLineInSpan (ForestLine False 0 0 ls) l

-- ---------------------------------------------------------------------

insertVersionsInSrcSpan :: Int -> Int -> SimpSpan -> SimpSpan
insertVersionsInSrcSpan vs ve ss = ss'
  where
    ((sl,sc),(el,ec)) = ss
    (chs,che) = forestSpanLenChangedFlags $ ss2f ss
    (trs,tre) = forestSpanAstVersions $ ss2f ss
    lineStart = forestLineToGhcLine (ForestLine chs trs vs sl)
    lineEnd   = forestLineToGhcLine (ForestLine che tre ve el)
    ss' = ((lineStart,sc),(lineEnd,ec))

-- ---------------------------------------------------------------------

insertLenChangedInSrcSpan :: Bool -> Bool -> SimpSpan -> SimpSpan
insertLenChangedInSrcSpan chs che ss = ss'
  where
    ((sl,sc),(el,ec)) = ss
    sl' = if chs then sl .|. forestLenChangedMask
                 else sl .&. (complement forestLenChangedMask)

    el' = if che then el .|. forestLenChangedMask
                 else el .&. (complement forestLenChangedMask)

    ss' = ((sl',sc),(el',ec))


-- ---------------------------------------------------------------------

-- | Replace any ForestLine flags already in a SrcSpan with the given ones
insertForestLineInSpan :: ForestLine -> SimpSpan -> SimpSpan
insertForestLineInSpan fl@(ForestLine ch tr v _l) ss = ss'
  where
    lineStart = forestLineToGhcLine fl
    ((_,cs),(ForestLine _ _ _ le,ce)) = ss2f ss
    lineEnd   = forestLineToGhcLine (ForestLine ch tr v le)
    ss' = ((lineStart,cs),(lineEnd,ce))

-- ---------------------------------------------------------------------


-- |Strip out the version markers
forestSpanToGhcPos :: ForestSpan -> (SimpPos,SimpPos)
forestSpanToGhcPos ((fls,sc),(fle,ec))
  = ((forestLineToGhcLine fls,sc),(forestLineToGhcLine fle,ec))

-- ---------------------------------------------------------------------

-- | Get the indent of the line before, taking into account in-line
-- 'where', 'let', 'in' and 'do' tokens
getIndentOffset :: (IsToken a) => [a] -> SimpPos -> Int
getIndentOffset [] _pos     = 1
getIndentOffset _toks (0,0) = 1
getIndentOffset toks pos
  = let (ts1, ts2) = break (\t->tokenPos t >= pos) toks
    in if (null ts2)
         then error "haskell-token-utils error: position does not exist in the token stream!"
         else let (sl,_) = splitOnNewLn $ reverse ts1
                -- sl is the reversed tokens of the previous line
                  (sls,_) = break isWhereOrLet $ filter (\t -> tokenLen t > 0) sl
                  firstTok = (glast "getIndentOffset" sls)
              in if startLayout firstTok
                  then if (length sls > 1)
                          then tokenOffset (last $ init sls)
                          else 4 + tokenOffset firstTok
                  else tokenOffset firstTok

      where
        tokenOffset t = (tokenCol t) - 1

        startLayout tok = isDo tok || isIn tok || isLet tok || isWhere tok

-- ---------------------------------------------------------------------

splitOnNewLn :: (IsToken a) => [a] -> ([a],[a])
splitOnNewLn toks = go [] toks
  -- ++AZ++ : TODO: is this simpler? : (toks1,toks2)=break (\x' -> tokenRow x /= tokenRow x') rtoks

  where
    go [] [] = ([],[])
    go ss [] = (ss,[])
    go [] xs = go [head xs] (tail xs)
    go ss xs
      | onSameLn (glast "splitOnNewLn" ss) (head xs) = go (ss ++ [head xs]) (tail xs)
      | otherwise = (ss,xs)

-- ---------------------------------------------------------------------

onSameLn :: (IsToken a) => a -> a -> Bool
onSameLn t1 t2 = r1 == r2
  where
    ((r1,_),_) = getSpan t1
    ((r2,_),_) = getSpan t2
-- ---------------------------------------------------------------------

newLnToken :: (IsToken a) => a -> a
newLnToken tok = newLinesToken 1 tok

-- ---------------------------------------------------------------------


newLinesToken :: (IsToken a) => Int -> a -> a
newLinesToken jump tok = tok'
  where
   ((sl,_),_) = getSpan tok
   nl = sl + jump
   tok' = putSpan mkZeroToken ((nl,1),(nl,1))

-- ---------------------------------------------------------------------


