module Language.Haskell.TokenUtils.TokenUtils
  (
  -- * Creating
    initTokenCache
  , initTokenCacheLayout
  , mkTreeFromTokens
  , mkTreeFromSpanTokens

  -- * Operations at 'TokenCache' level
  , Positioning (..)
  , putToksInCache
  , replaceTokenInCache
  , replaceTokenForSrcSpan
  , invariant

  -- *
  , nullForestSpan
  , nullForestPos
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

nullForestSpan :: ForestSpan
nullForestSpan = (nullForestPos,nullForestPos)

nullForestPos :: ForestPos
nullForestPos = (ForestLine False 0 0 0,0)

-- ---------------------------------------------------------------------

simpPosToForestSpan :: (SimpPos,SimpPos) -> ForestSpan
simpPosToForestSpan ((sr,sc),(er,ec))
    = ((ghcLineToForestLine sr,sc),(ghcLineToForestLine er,ec))

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

putToksInCache :: (IsToken a) => TokenCache a -> Span -> [a] -> (TokenCache a,Span)
putToksInCache tk sspan toks = (tk'',newSpan)
  where
   forest = getTreeFromCache sspan tk
   (forest',newSpan,oldTree) = updateTokensForSrcSpan forest sspan toks
   tk' = replaceTreeInCache sspan forest' tk
   tk'' = stash tk' oldTree

-- ---------------------------------------------------------------------

-- |Replace the tokens for a given SrcSpan with new ones. The SrcSpan
-- will be inserted into the tree if it is not already there.
-- If the SrcSpan changes size, replace the SrcSpan with a new one
-- (marked), and return it, as well as the old one
-- TODO: What about trailing comments? Preserve or replace?
updateTokensForSrcSpan :: (IsToken a)
  => Tree (Entry a) -> Span -> [a] -> (Tree (Entry a),Span,Tree (Entry a))
updateTokensForSrcSpan forest sspan toks = (forest'',newSpan,oldTree)
  where
    (forest',tree@(Node (Entry _s _ _) _)) = getSrcSpanFor forest (srcSpanToForestSpan sspan)
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

           ((startRow,startCol),_) = forestSpanToGhcPos $ srcSpanToForestSpan sspan
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
    (((ForestLine _chs _trs vs _),_),(ForestLine _che _tre ve _,_)) = srcSpanToForestSpan sspan
    -- Note: adding one to end version, so invariant won't fail
    -- newSpan = insertVersionsInSrcSpan vs ve $ posToSrcSpan forest (startPos,endPos) 
    newSpan = insertLenChangedInSrcSpan True True
            $ insertVersionsInSrcSpan vs ve $ posToSpan (startPos,endPos)

    zf = openZipperToNode tree $ Z.fromTree forest'

    zf' = Z.setTree (Node (Entry (srcSpanToForestSpan newSpan) NoChange toks'') []) zf
    forest'' = Z.toTree zf'

    oldTree = tree

-- ---------------------------------------------------------------------

posToSpan ::  (SimpPos,SimpPos) -> Span
posToSpan (s,e) = Span s e

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

stripForestLines :: (IsToken a) => [a] -> [a]
stripForestLines toks = map doOne toks
  where
    doOne tok = tok'
      where
       l = getSpan tok
       tok' = putSpan tok l'
       ((ForestLine _ _ _ ls,_),(_,_)) = srcSpanToForestSpan l
       l' = insertForestLineInSpan (ForestLine False 0 0 ls) l

-- ---------------------------------------------------------------------

insertVersionsInSrcSpan :: Int -> Int -> Span -> Span
insertVersionsInSrcSpan vs ve ss = ss'
  where
    Span (sl,sc) (el,ec) = ss
    (chs,che) = forestSpanLenChangedFlags $ srcSpanToForestSpan ss
    (trs,tre) = forestSpanAstVersions $ srcSpanToForestSpan ss
    lineStart = forestLineToGhcLine (ForestLine chs trs vs sl)
    lineEnd   = forestLineToGhcLine (ForestLine che tre ve el)
    ss' = Span (lineStart,sc) (lineEnd,ec)

-- ---------------------------------------------------------------------

insertLenChangedInSrcSpan :: Bool -> Bool -> Span -> Span
insertLenChangedInSrcSpan chs che ss = ss'
  where
    Span (sl,sc) (el,ec) = ss
    sl' = if chs then sl .|. forestLenChangedMask
                 else sl .&. (complement forestLenChangedMask)

    el' = if che then el .|. forestLenChangedMask
                 else el .&. (complement forestLenChangedMask)

    ss' = Span (sl',sc) (el',ec)


-- ---------------------------------------------------------------------

-- | Replace any ForestLine flags already in a SrcSpan with the given ones
insertForestLineInSpan :: ForestLine -> Span -> Span
insertForestLineInSpan fl@(ForestLine ch tr v _l) ss = ss'
  where
    lineStart = forestLineToGhcLine fl
    ((_,cs),(ForestLine _ _ _ le,ce)) = srcSpanToForestSpan ss
    lineEnd   = forestLineToGhcLine (ForestLine ch tr v le)
    ss' = Span (lineStart,cs) (lineEnd,ce)

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
        {-
        startLayout ((GHC.L _ (GHC.ITdo)),_)    = True
        startLayout ((GHC.L _ (GHC.ITin)),_)    = True
        startLayout ((GHC.L _ (GHC.ITlet)),_)   = True
        startLayout ((GHC.L _ (GHC.ITwhere)),_) = True
        startLayout _  = False
        -}

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
    Span (r1,_) _ = getSpan t1
    Span (r2,_) _ = getSpan t2
-- ---------------------------------------------------------------------

newLnToken :: (IsToken a) => a -> a
newLnToken tok = newLinesToken 1 tok

-- ---------------------------------------------------------------------


newLinesToken :: (IsToken a) => Int -> a -> a
newLinesToken jump tok = tok'
  where
   Span (sl,_) _ = getSpan tok
   nl = sl + jump
   tok' = putSpan mkZeroToken (Span (nl,1) (nl,1))
{-
   l' =  case l of
     GHC.RealSrcSpan ss ->
       let
         loc = GHC.mkSrcLoc (GHC.srcSpanFile ss) (jump + GHC.srcSpanEndLine ss) 1
       in
         GHC.mkSrcSpan loc loc
     _ -> l
-}

-- ---------------------------------------------------------------------

