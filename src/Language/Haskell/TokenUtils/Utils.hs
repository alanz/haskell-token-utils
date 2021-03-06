{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
module Language.Haskell.TokenUtils.Utils
  (
    splitToks

  , ghead
  , glast
  , gtail
  , gfromJust

  , addEndOffsets
  , calcLastTokenPos
  , makeOffset
  , makeLeaf
  , makeLeafFromToks
  , splitToksIncComments
  , makeGroup
  , makeGroupLayout
  , makeSpanFromTrees
  , mkGroup
  , subTreeOnly
  , splitToksForList
  , placeAbove
  , allocList
  , strip
  , startEndLocIncComments'

  -- * ForestSpan conversions
  -- , sf
  -- , srcSpanToForestSpan
  -- , fs
  -- , forestSpanToSrcSpan
  , simpPosToForestSpan
  , ss2f
  , forestSpanToSimpPos
  , f2ss

  -- * ForestSpans
  , treeIdFromForestSpan
  , forestSpanVersions
  , forestSpanAstVersions
  , forestSpanLenChangedFlags
  , forestSpanVersionNotSet
  , forestPosVersionSet
  , forestPosAstVersionSet
  , forestPosVersionNotSet
  , forestSpanLenChanged
  , forestPosLenChanged
  , treeIdIntoForestSpan
  , spanContains
  -- , insertForestLineInSpan
  , insertVersionsInForestSpan
  , insertLenChangedInForestSpan
  , forestSpanFromEntry
  , putForestSpanInEntry
  , forestSpanVersionSet

  -- *
  , treeStartEnd
  , groupTokensByLine
  , tokenRow
  , tokenCol
  , tokenColEnd
  , tokenPos
  , tokenPosEnd
  , increaseSrcSpan
  , srcPosToSimpPos
  , addOffsetToToks

  , decorate

  -- * Spans
  -- , spanStartEnd
  , combineSpans
  , nonCommentSpan
  , getStartLoc
  , getEndLoc

  -- * drawing the various trees
  , drawTreeEntry
  , drawForestEntry
  , showLayout
  , drawTreeCompact
  , drawTreeWithToks
  , showForestSpan
  , drawTokenCache
  , drawTokenCacheDetailed
  , divideComments
  ) where

import Control.Exception
import Data.List
import Data.Tree

-- import Language.Haskell.TokenUtils.DualTree
-- import Language.Haskell.TokenUtils.Layout
-- import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | Split the token stream into three parts: the tokens before the
-- startPos, the tokens between startPos and endPos, and the tokens
-- after endPos.
-- Note: The startPos and endPos refer to the startPos of a token only.
--       So a single token will have the same startPos and endPos
--    NO^^^^


-- splitToks::(SimpPos,SimpPos)->[PosToken]->([PosToken],[PosToken],[PosToken])
splitToks::(IsToken a) => (SimpPos,SimpPos)->[a]->([a],[a],[a])
splitToks (startPos, endPos) toks =
  let (toks1,toks2)   = break (\t -> tokenPos t >= startPos) toks
      (toks21,toks22) = break (\t -> tokenPos t >=   endPos) toks2
  in
    (toks1,toks21,toks22)


-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: [Char] -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"

-- ---------------------------------------------------------------------

addEndOffsets :: (IsToken a) => LayoutTree a -> [a] -> LayoutTree a
addEndOffsets tree toks = go tree
  where
    go (t@(Node (Entry _ _ _toks) [])) = t
    go (  (Node (Entry s (Above so p1 (r,c) _eo) []) subs))
        = (Node (Entry s (Above so p1 (r,c) eo') []) (map go subs))
      where
        (_,m,_) = splitToks ((r,c),(99999,1)) toks
        eo' = case m of
                []  -> None
                [_] -> None
                xs  -> if ro' /= 0 then FromAlignCol off
                                   else SameLine co'
                  where
                   off@(ro',co') = case (dropWhile isEmpty xs) of
                     []    -> (tokenRow y - r, tokenCol y - c) where y = head $ tail xs
                     (y:_) -> (tokenRow y - r, tokenCol y - c)
    go (  (Node (Entry s l []) subs)) = (Node (Entry s l []) (map go subs))
    go n = error $ "addEndOffsets:strange node:" ++ (show n)

-- ---------------------------------------------------------------------

calcLastTokenPos :: (IsToken a) => [a] -> (Int,Int)
calcLastTokenPos toks = (rt,ct)
  where
    (rt,ct) = case (dropWhile isEmpty (reverse toks)) of
             []    -> (0,0)
             (x:_) -> (tokenRow x,tokenCol x + tokenLen x)

-- ---------------------------------------------------------------------

makeOffset :: RowOffset -> ColOffset -> EndOffset
makeOffset 0   0 = None
makeOffset 0  co = SameLine co
makeOffset ro co = FromAlignCol (ro,co)

-- ---------------------------------------------------------------------

-- | Split the given tokens into the ones that occur prior to the start
-- of the list and ones that occur after
splitToksForList :: (IsToken a,HasLoc b) => [b] -> [a] -> ([a],[a],[a])
splitToksForList [] toks = ([],[],toks)
splitToksForList xs toks = splitToksIncComments (getLoc s, getLocEnd e) toks
  where
    s = head xs
    e = last xs

-- ---------------------------------------------------------------------

-- | Split the given tokens to include the comments belonging to the span.
splitToksIncComments :: (IsToken a)
  => (SimpPos, SimpPos)
  -> [a]
  -> ([a], [a], [a]) -- before,included,after
splitToksIncComments pos toks = splitToks pos' toks
  where
    pos' = startEndLocIncComments' toks pos

-- ---------------------------------------------------------------------

-- | Get the start&end location of t in the token stream, then extend
-- the start and end location to cover the preceding and following
-- comments.
--
{-
-- In this routine, 'then','else','do' and 'in' are treated as comments.
startEndLocIncComments::(SYB.Data t) => [PosToken] -> t -> (SimpPos,SimpPos)
startEndLocIncComments toks t = startEndLocIncComments' toks (getStartEndLoc t)
-}

startEndLocIncComments' :: (IsToken a) => [a] -> (SimpPos,SimpPos) -> (SimpPos,SimpPos)
startEndLocIncComments' toks (startLoc,endLoc) =
  let
    (begin,middle,end) = splitToks (startLoc,endLoc) toks

    notIgnored tt = not (isWhiteSpaceOrIgnored tt)

    (leadinr,leadr) = break notIgnored $ reverse begin
    leadr' = filter (\t -> not (isEmpty t)) leadr
    prevLine  = if (null leadr') then 0 else (tokenRow $ ghead "startEndLocIncComments'1" leadr')
    firstLine = if (null middle) then 0 else (tokenRow $ ghead "startEndLocIncComments'1" middle)
    (_nonleadComments,leadComments') = divideComments prevLine firstLine $ reverse leadinr
    leadComments = dropWhile (\tt -> (isEmpty tt)) leadComments'

    (trail,trailrest) = break notWhiteSpace end
    trail' = filter (\t -> not (isEmpty t)) trail
    lastLine = if (null middle)
        then      0
        else (tokenRow $ glast "startEndLocIncComments'2" middle)
    nextLine = if (null trailrest)
        then 100000
        else (tokenRow $ ghead "startEndLocIncComments'2" trailrest)
    (trailComments,_) =  divideComments lastLine nextLine trail'

    middle' = leadComments ++ middle ++ trailComments
  in
    if (null middle')
      then ((0,0),(0,0))
      else ((tokenPos $ ghead "startEndLocIncComments 4" middle'),(tokenPosEnd $ last middle'))

-- ---------------------------------------------------------------------

-- |Split a set of comment tokens into the ones that belong with the startLine
-- and those that belong with the endLine
divideComments :: (IsToken a) => Int -> Int -> [a] -> ([a],[a])
divideComments startLine endLine toks = (first,second)
  where
    groups = groupBy groupByAdjacent toks
    groupLines = map (\ts -> ((tokenRow $ ghead "divideComments" ts,tokenRow $ glast "divideComments" ts),ts)) groups
    groupLines' = [((startLine,startLine),[])] ++ groupLines ++ [((endLine,endLine),[])]
    groupGaps = go [] groupLines'
    -- groupGaps is now a list of gaps followed by the tokens. The
    -- last gap has an empty token list, since there is one more gap
    -- than token groups

    -- e.g [(0,[comments1]),(3,[comments2]),(1,[]) captures
    --  ---------------------
    --      b + bar -- ^trailing comment
    --
    --
    -- -- leading comment
    -- foo x y =
    -- ----------------------

    biggest = maximum $ map fst groupGaps

    (firsts,seconds) = break (\(g,_) -> g >= biggest) groupGaps

    first = concatMap snd firsts
    second = concatMap snd seconds

    -- Helpers
    groupByAdjacent :: (IsToken a) => a -> a -> Bool
    groupByAdjacent a b = 1 + tokenRow a == tokenRow b

    go :: (IsToken a) => [(Int,[a])] -> [((Int,Int),[a])] -> [(Int,[a])]
    go acc []  = acc
    go acc [_x] = acc
    go acc (((_s1,e1),_t1):b@((s2,_e2),t2):xs) = go (acc ++ [((s2 - e1),t2)] ) (b:xs)

-- ---------------------------------------------------------------------

placeAbove :: (IsToken a) => EndOffset -> (Row,Col) -> (Row,Col) -> [LayoutTree a] -> LayoutTree a
placeAbove _ _ _ [] = error "placeAbove []"
placeAbove so p1 p2 ls = Node (Entry loc (Above so p1 p2 None) []) ls
  where
    loc = makeSpanFromTrees ls

-- ---------------------------------------------------------------------

makeGroup :: (IsToken a) => [LayoutTree a] -> LayoutTree a
makeGroup [x] = x
makeGroup ls  = makeGroupLayout NoChange ls

makeGroupLayout :: (IsToken a) => Layout -> [LayoutTree a] -> LayoutTree a
makeGroupLayout lay ls = Node (Entry loc lay []) ls
  where
    loc = makeSpanFromTrees ls

-- ---------------------------------------------------------------------

makeSpanFromTrees :: [LayoutTree a] -> ForestSpan
makeSpanFromTrees ls
  = case ls of
      [] -> ss2f nullSpan
      _  -> combineSpans (getTreeLoc $ head ls) (getTreeLoc $ last ls)

nullSpan :: SimpSpan
nullSpan = ((0,0),(0,0))


getStartLoc :: SimpSpan -> SimpPos
getStartLoc = fst

getEndLoc :: SimpSpan -> SimpPos
getEndLoc = snd

-- ---------------------------------------------------------------------

subTreeOnly :: (IsToken a) => [LayoutTree a] -> [LayoutTree a]
subTreeOnly [(Node _ sub)] = sub
subTreeOnly xs = xs

-- ---------------------------------------------------------------------

getTreeLoc :: LayoutTree a -> ForestSpan
getTreeLoc (Node (Entry   l _ _) _) = l
getTreeLoc (Node (Deleted l _ _) _) = l

-- ---------------------------------------------------------------------

mkGroup :: (IsToken a) => SimpSpan -> Layout -> [LayoutTree a] -> LayoutTree a
mkGroup sspan lay subs = Node (Entry (ss2f sspan) lay []) subs


-- TODO: Move this into the main Utils
makeLeaf :: (IsToken a) => SimpSpan -> Layout -> [a] -> LayoutTree a
makeLeaf sspan lay toks = Node (Entry (ss2f sspan) lay toks) []

-- ---------------------------------------------------------------------
{-
sf :: Span -> ForestSpan
sf = srcSpanToForestSpan

fs :: ForestSpan -> Span
fs = forestSpanToSrcSpan

srcSpanToForestSpan :: Span -> ForestSpan
srcSpanToForestSpan sspan = ((ghcLineToForestLine startRow,startCol),(ghcLineToForestLine endRow,endCol))
  where
    (Span (startRow,startCol) (endRow,endCol)) = sspan

forestSpanToSrcSpan :: ForestSpan -> Span
forestSpanToSrcSpan ((fls,sc),(fle,ec)) = sspan
  where
    lineStart = forestLineToGhcLine fls
    lineEnd   = forestLineToGhcLine fle
    locStart = (lineStart, sc)
    locEnd   = (lineEnd,   ec)
    sspan = Span locStart locEnd
-}

-- ---------------------------------------------------------------------

-- |Gets the version numbers
forestSpanVersions :: ForestSpan -> (Int,Int)
forestSpanVersions ((ForestLine _ _ sv _,_),(ForestLine _ _ ev _,_)) = (sv,ev)

-- |Gets the AST tree numbers
forestSpanAstVersions :: ForestSpan -> (Int,Int)
forestSpanAstVersions ((ForestLine _ trs _ _,_),(ForestLine _ tre _ _,_)) = (trs,tre)

-- |Gets the SpanLengthChanged flags
forestSpanLenChangedFlags :: ForestSpan -> (Bool,Bool)
forestSpanLenChangedFlags ((ForestLine chs _ _ _,_),(ForestLine che _ _ _,_)) = (chs,che)

{- moved to haskell-token-utils
-- |Checks if the version is non-zero in either position
forestSpanVersionSet :: ForestSpan -> Bool
forestSpanVersionSet ((ForestLine _ _ sv _,_),(ForestLine _ _ ev _,_)) = sv /= 0 || ev /= 0
-}

-- |Checks if the version is zero in both positions
forestSpanVersionNotSet :: ForestSpan -> Bool
forestSpanVersionNotSet ((ForestLine _ _ sv _,_),(ForestLine _ _ ev _,_)) = sv == 0 && ev == 0

-- |Checks if the version is non-zero
forestPosVersionSet :: ForestPos -> Bool
forestPosVersionSet (ForestLine _ _ v _,_) = v /= 0

-- |Checks if the AST version is non-zero
forestPosAstVersionSet :: ForestPos -> Bool
forestPosAstVersionSet (ForestLine _ tr _ _,_) = tr /= 0

-- |Checks if the version is zero
forestPosVersionNotSet :: ForestPos -> Bool
forestPosVersionNotSet (ForestLine _ _ v _,_) = v == 0

forestSpanLenChanged :: ForestSpan -> Bool
forestSpanLenChanged (s,e) = (forestPosLenChanged s) || (forestPosLenChanged e)

forestPosLenChanged :: ForestPos -> Bool
forestPosLenChanged (ForestLine ch _ _ _,_) = ch

-- |Puts a TreeId into a forestSpan
treeIdIntoForestSpan :: TreeId -> ForestSpan -> ForestSpan
treeIdIntoForestSpan (TId sel) ((ForestLine chs _ sv sl,sc),(ForestLine che _ ev el,ec))
  = ((ForestLine chs sel sv sl,sc),(ForestLine che sel ev el,ec))

-- ---------------------------------------------------------------------

-- |Does the first span contain the second? Takes cognisance of the
-- various flags a ForestSpan can have.
-- NOTE: This function relies on the Eq instance for ForestLine
spanContains :: ForestSpan -> ForestSpan -> Bool
spanContains span1 span2 = (startPos <= nodeStart && endPos >= nodeEnd)
    where
        -- TODO: This looks like a no-op?
        (tvs,_tve) = forestSpanVersions $ span1
        (nvs,_nve) = forestSpanVersions $ span2
        (startPos,endPos)   = insertVersionsInForestSpan tvs tvs span1
        (nodeStart,nodeEnd) = insertVersionsInForestSpan nvs nvs span2

-- ---------------------------------------------------------------------
{-
-- | Replace any ForestLine flags already in a Span with the given ones
insertForestLineInSpan :: ForestLine -> Span -> Span
insertForestLineInSpan fl@(ForestLine ch tr v _l) ss = ss'
  where
    Span (lineStart,sc) (_,ec) = ss
    -- lineStart = forestLineToGhcLine fl
    (_,(ForestLine _ _ _ le,_)) = srcSpanToForestSpan ss
    lineEnd   = forestLineToGhcLine (ForestLine ch tr v le)
    ss' = Span (lineStart,sc) (lineEnd,ec)
-}
-- ---------------------------------------------------------------------

insertVersionsInForestSpan :: Int -> Int -> ForestSpan -> ForestSpan
insertVersionsInForestSpan vsNew veNew ((ForestLine chs trs _vs ls,cs),(ForestLine che tre _ve le,ce))
  = ((ForestLine chs trs vsNew ls,cs),(ForestLine che tre veNew le,ce))

-- ---------------------------------------------------------------------

insertLenChangedInForestSpan :: Bool -> ForestSpan -> ForestSpan
insertLenChangedInForestSpan chNew ((ForestLine _chs trs vs ls,cs),(ForestLine _che tre ve le,ce))
  = ((ForestLine chNew trs vs ls,cs),(ForestLine chNew tre ve le,ce))

-- ---------------------------------------------------------------------

makeLeafFromToks :: (IsToken a) => [a] -> [LayoutTree a]
makeLeafFromToks [] = []
makeLeafFromToks toks = [Node (Entry loc NoChange toks) []]
  where
    loc = sspan

    (startLoc',endLoc') = nonCommentSpanLayout toks
    sspan    = if (startLoc',endLoc') == ((0,0),(0,0))
      then error $ "mkLeafFromToks:null span for:" ++ (show toks)
      else simpPosToForestSpan (startLoc',endLoc')

-- ---------------------------------------------------------------------

-- |Extract the start and end position of a span, without any leading
-- or trailing comments
nonCommentSpanLayout :: (IsToken a) => [a] -> (SimpPos,SimpPos)
nonCommentSpanLayout [] = ((0,0),(0,0))
nonCommentSpanLayout toks = (startPos,endPos)
  where
    stripped = dropWhile isComment $ toks
    (startPos,endPos) = case stripped of
      -- [] -> ((0,0),(0,0))
      [] -> (tokenPos $ head toks,tokenPosEnd $ last toks)
      _ -> (tokenPos startTok,tokenPosEnd endTok)
       where
        startTok = ghead "nonCommentSpan.1" $ dropWhile isComment $ toks
        endTok   = ghead "nonCommentSpan.2" $ dropWhile isComment $ reverse toks

-- ---------------------------------------------------------------------

-- |Extract the start and end position of a span, without any leading
-- or trailing comments
nonCommentSpan :: (IsToken a) => [a] -> (SimpPos,SimpPos)
nonCommentSpan [] = ((0,0),(0,0))
nonCommentSpan toks = (startPos,endPos)
  where
    stripped = dropWhile isIgnoredNonComment $ toks
    (startPos,endPos) = case stripped of
      [] -> ((0,0),(0,0))
      _ -> (tokenPos startTok,tokenPosEnd endTok)
       where
        startTok = ghead "nonCommentSpan.1" $ dropWhile isIgnoredNonComment $ toks
        endTok   = ghead "nonCommentSpan.2" $ dropWhile isIgnoredNonComment $ reverse toks

-- ---------------------------------------------------------------------

-- | ForestSpan version of GHC combineSrcSpans
combineSpans :: ForestSpan -> ForestSpan -> ForestSpan
combineSpans fs1 fs2 = fs'
  where
    [lowFs,highFs] = sort [fs1,fs2]
    ((ForestLine  chls  trls  vls  lls ,cls),(ForestLine _chle _trle _vle _lle,_cle)) = lowFs
    ((ForestLine _chhs _trhs _vhs _lhs,_chs),(ForestLine  chhe  trhe  vhe  lhe, che)) = highFs

    fs' = ((ForestLine chls trls vls lls,cls),(ForestLine chhe trhe vhe lhe,che))


simpPosToForestSpan :: (SimpPos,SimpPos) -> ForestSpan
simpPosToForestSpan ((sr,sc),(er,ec))
    = ((ghcLineToForestLine sr,sc),(ghcLineToForestLine er,ec))

ss2f :: (SimpPos, SimpPos) -> ForestSpan
ss2f = simpPosToForestSpan

-- --------------------------------------------------------------------

forestSpanFromEntry :: Entry a -> ForestSpan
forestSpanFromEntry (Entry   ss _ _) = ss
forestSpanFromEntry (Deleted ss _ _) = ss

putForestSpanInEntry :: Entry a -> ForestSpan -> Entry a
putForestSpanInEntry (Entry   _ss lay toks) ssnew = (Entry ssnew lay toks)
putForestSpanInEntry (Deleted _ss pg eg) ssnew = (Deleted ssnew pg eg)

-- ---------------------------------------------------------------------

-- |Strip out the version markers
forestSpanToSimpPos :: ForestSpan -> (SimpPos,SimpPos)
forestSpanToSimpPos ((ForestLine _ _ _ sr,sc),(ForestLine _ _ _ er,ec)) = ((sr,sc),(er,ec))

f2ss :: ForestSpan -> (SimpPos, SimpPos)
f2ss = forestSpanToSimpPos

-- |Checks if the version is non-zero in either position
forestSpanVersionSet :: ForestSpan -> Bool
forestSpanVersionSet ((ForestLine _ _ sv _,_),(ForestLine _ _ ev _,_)) = sv /= 0 || ev /= 0

-- ---------------------------------------------------------------------

-- |Get the start and end position of a Tree
-- treeStartEnd :: Tree Entry -> (SimpPos,SimpPos)
-- treeStartEnd (Node (Entry sspan _) _) = (getGhcLoc sspan,getGhcLocEnd sspan)
treeStartEnd :: Tree (Entry a) -> ForestSpan
treeStartEnd (Node (Entry sspan _ _) _) = sspan
treeStartEnd (Node (Deleted sspan _ _) _) = sspan

-- ---------------------------------------------------------------------

groupTokensByLine :: (IsToken a) => [a] -> [[a]]
groupTokensByLine xs = groupBy toksOnSameLine xs

toksOnSameLine :: (IsToken a) => a -> a -> Bool
toksOnSameLine t1 t2 = tokenRow t1 == tokenRow t2


-- ---------------------------------------------------------------------

tokenRow :: (IsToken a) => a -> Int
tokenRow tok = r
  where ((r,_),_) = getSpan tok

tokenCol :: (IsToken a) => a -> Int
tokenCol tok = c
  where ((_,c),_) = getSpan tok

tokenColEnd :: (IsToken a) => a -> Int
tokenColEnd tok = c
  where (_,(_,c)) = getSpan tok

tokenPos :: IsToken a => a -> SimpPos
tokenPos tok = startPos
  where (startPos,_) = getSpan tok

tokenPosEnd :: IsToken a => a -> SimpPos
tokenPosEnd tok = endPos
  where (_,endPos) = getSpan tok

instance (IsToken t) => Ord (LayoutTree t) where
  compare (Node a _) (Node b _) = compare (forestSpanFromEntry a) (forestSpanFromEntry b)

instance (IsToken a) => Eq (Entry a) where
  (Entry fs1 lay1 toks1) == (Entry fs2 lay2 toks2)
    = fs1 == fs2 && lay1 == lay2
   && (show toks1) == (show toks2)

  (Deleted fs1 pg1 lay1) == (Deleted fs2 pg2 lay2)
    = fs1 == fs2 && pg1 == pg2 && lay1 == lay2

  (==) _ _ = False

instance HasLoc (Entry a) where
  getLoc (Entry fs _ _)   = getLoc fs
  getLoc (Deleted fs _ _) = getLoc fs

  getLocEnd (Entry fs _ _)   = getLocEnd fs
  getLocEnd (Deleted fs _ _) = getLocEnd fs

  putSpan e ss = putForestSpanInEntry e (ss2f ss)

instance HasLoc ForestSpan where
  getLoc fs    = fst (forestSpanToSimpPos fs)
  getLocEnd fs = snd (forestSpanToSimpPos fs)

  putSpan _f s  = simpPosToForestSpan s

-- ---------------------------------------------------------------------

srcPosToSimpPos :: (Int,Int) -> (Int,Int)
srcPosToSimpPos (sr,c) = (l,c)
  where
    (ForestLine _ _ _ l) = ghcLineToForestLine sr

-- ---------------------------------------------------------------------

strip :: (IsToken a) => [LayoutTree a] -> [LayoutTree a]
strip ls = filter (not . emptyNode) ls
  where
    emptyNode (Node (Entry _ _ []) []) = True
    emptyNode _                        = False

-- ---------------------------------------------------------------------

allocList :: (IsToken a,HasLoc b)
   => [b]
   -> [a]
   -> (b -> [a] -> [LayoutTree a])
   -> [LayoutTree a]
allocList xs toksIn allocFunc = r
  where
    (s2,listToks,toks2') = splitToksForList xs toksIn
    (layout,toks2) = (allocAll xs listToks,toks2')

    allocAll xs' toks = res
      where
        (declLayout,tailToks) = foldl' doOne ([],toks) xs'

        res = strip $ declLayout ++ (makeLeafFromToks tailToks)

        doOne (acc,toksOne) x = r1
          where
            l = (getLoc x,getLocEnd x)
            (s1,funcToks,toks') = splitToksIncComments l toksOne
            layout' = (makeLeafFromToks s1) ++ [makeGroup (strip $ allocFunc x funcToks)]
            r1 = (acc ++ (strip layout'),toks')

    r = strip $ (makeLeafFromToks s2) ++ [makeGroup $ strip $ layout] ++ (makeLeafFromToks toks2)

-- ---------------------------------------------------------------------

{-
spanStartEnd :: Span -> (SimpPos,SimpPos)
spanStartEnd (Span start end) = (start,end)
-}
-- ---------------------------------------------------------------------

treeIdFromForestSpan :: ForestSpan -> TreeId
treeIdFromForestSpan ((ForestLine _ tr _ _,_),(ForestLine _ _ _ _,_)) = TId tr

-- ---------------------------------------------------------------------
-- | Neat 2-dimensional drawing of a tree.
drawTreeEntry :: Tree (Entry a) -> String
drawTreeEntry  = unlines . drawEntry

-- | Neat 2-dimensional drawing of a forest.
drawForestEntry :: Forest (Entry a) -> String
drawForestEntry  = unlines . map drawTreeEntry

drawEntry :: Tree (Entry a) -> [String]
drawEntry (Node (Deleted sspan  _pg eg )  _ ) = [(showForestSpan sspan) ++ (show eg) ++ "D"]
drawEntry (Node (Entry sspan lay _toks) ts0) = ((showForestSpan sspan) ++ (showLayout lay)): drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shft "`- " "   " (drawEntry t)
    drawSubTrees (t:ts) =
        "|" : shft "+- " "|  " (drawEntry t) ++ drawSubTrees ts

    shft first other = zipWith (++) (first : repeat other)

showLayout :: Layout -> String
showLayout NoChange       = ""
showLayout (Above so p1 (r,c) eo) = "(Above "++ show so ++ " " ++ show p1 ++ " " ++ show (r,c) ++ " " ++ show eo ++ ")"
-- showLayout (Offset r c)   = "(Offset " ++ show r ++ " " ++ show c ++ ")"

-- ---------------------------------------------------------------------

drawTreeCompact :: Tree (Entry a) -> String
drawTreeCompact = unlines . drawTreeCompact' 0

drawTreeCompact' :: Int -> Tree (Entry a) -> [String]
drawTreeCompact' level (Node (Deleted sspan _pg eg )  _  ) = [(show level) ++ ":" ++ (showForestSpan sspan) ++ (show eg) ++ "D"]
drawTreeCompact' level (Node (Entry sspan lay _toks) ts0) = ((show level) ++ ":" ++ (showForestSpan sspan) ++ (showLayout lay))
                                                          : (concatMap (drawTreeCompact' (level + 1)) ts0)

showForestSpan :: ForestSpan -> String
showForestSpan ((sr,sc),(er,ec))
  = show ((flToNum sr,sc),(flToNum er,ec))
  where
    flToNum (ForestLine ch tr v l) = (if ch then 10000000000::Integer else 0)
                                   + ((fromIntegral tr) * 100000000::Integer)
                                   + ((fromIntegral v)  *   1000000::Integer)
                                   + (fromIntegral l)

-- ---------------------------------------------------------------------

drawTreeWithToks :: (IsToken a) => Tree (Entry a) -> String
drawTreeWithToks = unlines . drawTreeWithToks' 0

drawTreeWithToks' :: (IsToken a) => Int -> Tree (Entry a) -> [String]
drawTreeWithToks' level (Node (Deleted sspan _pg eg )  _  )
   = [(showLevel level) ++ ":" ++ (showForestSpan sspan) ++ (show eg) ++ "D"]
drawTreeWithToks' level (Node (Entry sspan lay toks) ts0)
   = ((showLevel level) ++ ":" ++ (showForestSpan sspan) ++ (showLayout lay) ++ (showFriendlyToks toks))
--    = ((showLevel level) ++ ":" ++ (showForestSpan sspan) ++ (showLayout lay) ++ (show toks))
       : (concatMap (drawTreeWithToks' (level + 1)) ts0)

showLevel :: Int -> String
showLevel level = take level (repeat ' ')

-- ---------------------------------------------------------------------

-- |Call drawTreeEntry on the entire token cache
drawTokenCache :: (IsToken a) => TokenCache a -> String
drawTokenCache tk = Map.foldlWithKey' doOne "" (tkCache tk)
  where
    doOne :: String -> TreeId -> Tree (Entry a) -> String
    doOne s key val = s ++ "tree " ++ (show key) ++ ":\n"
                        ++ (drawTreeEntry val)

-- ---------------------------------------------------------------------

-- |Call drawTreeEntry on the entire token cache
drawTokenCacheDetailed :: (IsToken a) => TokenCache a -> String
drawTokenCacheDetailed tk = Map.foldlWithKey' doOne "" (tkCache tk)
  where
    doOne :: (IsToken a) => String -> TreeId -> Tree (Entry a) -> String
    doOne s key val = s ++ "tree " ++ (show key) ++ ":\n"
                        ++ (show val)

-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------

-- |Add a constant line and column offset to a span of tokens
addOffsetToToks :: (IsToken a) => SimpPos -> [a] -> [a]
addOffsetToToks (r,c) toks = map (\t -> increaseSrcSpan (r,c) t) toks

-- ---------------------------------------------------------------------

-- Allocate all the tokens to the given tree
decorate :: (IsToken a) => LayoutTree a -> [a] -> LayoutTree a
decorate tree toks = go toks tree
  where
    go :: (IsToken a) => [a] -> LayoutTree a -> LayoutTree a
    go ts (Node e subs) = r
      where
        -- b = map (\t -> let f = treeStartEnd t in (getLoc f, getLocEnd f)) subs

        doOne :: (IsToken a) => ([a],[LayoutTree a]) -> LayoutTree a -> ([a],[LayoutTree a])
        doOne (ts1,acc) tree1 = (ts1',acc')
          where
            ss = treeStartEnd tree1
            (before,middle,after) = splitToksIncComments (getLoc ss,getLocEnd ss) ts1
            ts1' = after
            tree' = case tree1 of
              Node (Entry ss1 lo []) [] -> [Node (Entry ss1 lo middle) []]
              _ -> [go middle tree1]
            acc' = acc ++ makeLeafFromToks before ++ tree'

        (end,subs') = foldl' doOne (ts,[]) subs
        -- (end,subs') = doOne (ts,[]) (head subs)
        subs'' = subs' ++ makeLeafFromToks end
        r = case makeLeafFromToks end of
          []  -> Node e subs'
          trs -> Node (Entry (ss,se) NoChange []) (subs'++trs)
                where
                  (ss,_) = treeStartEnd (ghead "decorate" subs')
                  (_,se) = treeStartEnd (glast "decorate" trs)

    -- go ts tr = error $ "decorate:not processing :" ++ show (ts,tr)


-- ---------------------------------------------------------------------

-- |Shift the whole token by the given offset
increaseSrcSpan :: (IsToken a) => SimpPos -> a -> a
increaseSrcSpan (lineAmount,colAmount) posToken
    = putSpan posToken newL
    where
        newL = ((startLine + lineAmount, startCol + colAmount),
                (endLine + lineAmount, endCol + colAmount))

        ((startLine, startCol),(endLine,endCol)) = getSpan posToken



-- ---------------------------------------------------------------------
