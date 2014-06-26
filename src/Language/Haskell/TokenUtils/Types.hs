{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
module Language.Haskell.TokenUtils.Types
  (
    Entry(..)
  , TokenCache(..)
  , TreeId(..)
  , mainTid
  , ForestSpan
  , ForestPos
  , ForestLine(..)
  , RowOffset
  , ColOffset
  , Row
  , Col
  , SimpPos
  , Layout(..)
  , EndOffset(..)
  , Located(..)
  , Span(..)
  , nullSpan
  , TokenLayout
  , LayoutTree
  , forestSpanFromEntry
  , putForestSpanInEntry
  , forestSpanToSimpPos
  , forestSpanVersionSet
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
  , ghcLineToForestLine
  , forestLineToGhcLine
  , forestLenChangedMask

  , IsToken(..)
  , notWhiteSpace
  , isWhiteSpaceOrIgnored
  , isIgnored
  , isIgnoredNonComment
  , isWhereOrLet
  , showFriendlyToks
  , HasLoc(..)
  , Allocatable(..)
  ) where

import Control.Exception
import Data.Bits
import Data.List
import Data.Tree

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | An entry in the data structure for a particular srcspan.
data Entry a =
   -- |Entry has
   --   * the source span contained in this Node
   --   * how the sub-elements nest
   --   * the tokens for the SrcSpan if subtree is empty
   Entry !ForestSpan
         !Layout
         ![a]
   -- |Deleted has
   --   * the source span has been deleted
   --   * prior gap in lines
   --   * the gap between this span end and the
   --     start of the next in the fringe of the
   --     tree.
   | Deleted !ForestSpan
             !RowOffset
             !SimpPos
 deriving (Show)

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


type RowOffset = Int
type ColOffset = Int
type Row       = Int
type Col       = Int

type SimpPos = (Int,Int) -- Line, column

data Layout = Above EndOffset (Row,Col) (Row,Col) EndOffset
            -- ^ Initial offset from token before the
            -- stacked list of items, the (r,c) of the first
            -- non-comment token, the (r,c) of the end of the last non-comment
            -- token in the stacked list to be able to calculate the
            -- (RowOffset,ColOffset) between the last token and the
            -- start of the next item.
            | NoChange
            deriving (Show,Eq)

data EndOffset = None
               | SameLine ColOffset
               | FromAlignCol (RowOffset, ColOffset)
               deriving (Show,Eq)

-- ---------------------------------------------------------------------

data ForestLine = ForestLine
                  { flSpanLengthChanged :: !Bool -- ^The length of the
                                                 -- span may have
                                                 -- changed due to
                                                 -- updated tokens.
                  , flTreeSelector  :: !Int
                  , flInsertVersion :: !Int
                  , flLine          :: !Int
                  } -- deriving (Eq)

instance Eq ForestLine where
  -- TODO: make this undefined, and patch all broken code to use the
  --       specific fun here directly instead.
  (ForestLine _ s1 v1 l1) == (ForestLine _ s2 v2 l2) = s1 == s2 && v1 == v2 && l1 == l2

instance Show ForestLine where
  show s = "(ForestLine " ++ (show $ flSpanLengthChanged s)
         ++ " " ++ (show $ flTreeSelector s)
         ++ " " ++ (show $ flInsertVersion s)
         ++ " " ++ (show $ flLine s)
         ++ ")"

-- instance Outputable ForestLine where
--   ppr fl = text (show fl)
instance Ord ForestLine where
  -- Use line as the primary comparison, but break any ties with the
  -- version
  -- Tree is ignored, as it is only a marker on the topmost element
  -- Ignore sizeChanged flag, it will only be relevant in the
  -- invariant check
  compare (ForestLine _sc1 _ v1 l1) (ForestLine _sc2 _ v2 l2) =
         if (l1 == l2)
           then compare v1 v2
           else compare l1 l2


-- ---------------------------------------------------------------------

type ForestPos = (ForestLine,Int)


-- |Match a SrcSpan, using a ForestLine as the marker
type ForestSpan = (ForestPos,ForestPos)

instance HasLoc ForestSpan where
  getLoc fs    = fst (forestSpanToSimpPos fs)
  getLocEnd fs = snd (forestSpanToSimpPos fs)

-- ---------------------------------------------------------------------

data TreeId = TId !Int deriving (Eq,Ord,Show)

-- |Identifies the tree carrying the main tokens, not any work in
-- progress or deleted ones
mainTid :: TreeId
mainTid = TId 0

data TokenCache a = TK
  { tkCache :: !(Map.Map TreeId (Tree (Entry a)))
  , tkLastTreeId :: !TreeId
  }

-- ---------------------------------------------------------------------

class Allocatable b a where
  allocTokens :: b -> [a] -> LayoutTree a


-- |The IsToken class captures the different token type in use. For
-- GHC it represents the type returned by `GHC.getRichTokenStream`,
-- namely [(GHC.Located GHC.Token, String)]
-- For haskell-src-exts this is the reult of `lexTokenStream`, namely `[HSE.Loc HSE.Token]`
class (Show a) => IsToken a where
  -- TODO: get rid of these and put a HasLoc requirement
  getSpan     :: a -> Span
  putSpan     :: a -> Span -> a

  -- |tokenLen returns the length of the string representation of the
  -- token, not just the difference in the location, as the string may
  -- have changed without the position being updated, e.g. in a
  -- renaming
  tokenLen    :: a -> Int

  isComment   :: a -> Bool

  -- |Zero-length tokens, as appear in GHC as markers
  isEmpty      :: a -> Bool
  mkZeroToken  :: a

  isDo         :: a -> Bool
  isElse       :: a -> Bool
  isIn         :: a -> Bool
  isLet        :: a -> Bool
  isOf         :: a -> Bool
  isThen       :: a -> Bool
  isWhere      :: a -> Bool

  tokenToString :: a -> String
  -- TODO: may be able to get rid of next due to former
  showTokenStream :: [a] -> String

  -- |Mark a token so that it can be use to trigger layout checking
  -- later when the toks are retrieved
  markToken :: a -> a
  isMarked  :: a -> Bool



-- derived functions
isWhiteSpace :: (IsToken a) => a -> Bool
isWhiteSpace tok = isComment tok || isEmpty tok

notWhiteSpace :: (IsToken a) => a -> Bool
notWhiteSpace tok = not (isWhiteSpace tok)

isWhiteSpaceOrIgnored :: (IsToken a) => a -> Bool
isWhiteSpaceOrIgnored tok = isWhiteSpace tok || isIgnored tok

-- Tokens that are ignored when allocating tokens to a SrcSpan
isIgnored :: (IsToken a) => a -> Bool
isIgnored tok = isThen tok || isElse tok || isIn tok || isDo tok

-- | Tokens that are ignored when determining the first non-comment
-- token in a span
isIgnoredNonComment :: (IsToken a) => a -> Bool
isIgnoredNonComment tok = isThen tok || isElse tok || isWhiteSpace tok

isWhereOrLet :: (IsToken a) => a -> Bool
isWhereOrLet t = isWhere t || isLet t

showFriendlyToks :: IsToken a => [a] -> String
showFriendlyToks toks = reverse $ dropWhile (=='\n')
                      $ reverse $ dropWhile (=='\n') $ showTokenStream toks


class HasLoc a where
  getLoc      :: a -> SimpPos
  getLocEnd   :: a -> SimpPos

data Located e = L Span e
               deriving Show

data Span = Span (Row,Col) (Row,Col)
          deriving (Show,Eq,Ord)

nullSpan :: Span
nullSpan = Span (0,0) (0,0)

data TokenLayout a = TL (Tree (Entry a))

type LayoutTree a = Tree (Entry a)

instance (IsToken t) => Ord (LayoutTree t) where
  compare (Node a _) (Node b _) = compare (forestSpanFromEntry a) (forestSpanFromEntry b)

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
  where (Span (r,_) _) = getSpan tok

tokenCol :: (IsToken a) => a -> Int
tokenCol tok = c
  where (Span (_,c) _) = getSpan tok

tokenColEnd :: (IsToken a) => a -> Int
tokenColEnd tok = c
  where (Span _ (_,c)) = getSpan tok

tokenPos :: IsToken a => a -> SimpPos
tokenPos tok = startPos
  where (Span startPos _) = getSpan tok

tokenPosEnd :: IsToken a => a -> SimpPos
tokenPosEnd tok = endPos
  where (Span _ endPos) = getSpan tok

-- ---------------------------------------------------------------------

-- tokenLen :: PosToken -> Int
-- tokenLen (_,s)     = length s   --check this again! need to handle the tab key.

-- ---------------------------------------------------------------------

srcPosToSimpPos :: (Int,Int) -> (Int,Int)
srcPosToSimpPos (sr,c) = (l,c)
  where
    (ForestLine _ _ _ l) = ghcLineToForestLine sr

-- ---------------------------------------------------------------------

-- A data type for the line entries in a SrcSpan. This has the
-- following properties
--
-- 1. It can be converted to and from the underlying Int in the
--    original SrcSpan
-- 2. It allows the insertion of an arbitrary line as the start of a
--    new SrcSpan
-- 3. It has an ordering relation, which honours the inserts which
--    were made.
-- 4. It can keep track of tokens that have been removed from the main
--    AST, which can be edited outside of it and then inserted again
--
-- This is achieved by adding two fields to the SrcSpan, one to
-- indicate which AST fragment it is in, and the other to indicate its
-- insert relationship, encoded as 0 for the original, 1 for the
-- first, 2 for the second and so on.
--
-- This field is converted to and from the original line by being
-- multiplied by a very large number and added to the original.
--
-- The guaranteed max value in Haskell for an Int is 2^29 - 1.
-- This evaluates to 536,870,911,or 536.8 million.
--
-- However, as pointed out on #haskell, the GHC compiler (which this
-- implemtation explicitly targets) provides the full 32 bits (at
-- least, can be 64), so we have
--   maxBound :: Int = 2,147,483,647
--
-- Schema:max pos value is 0x7fffffff (31 bits)
-- 1 bit for LenChanged
-- 5 bits for tree    : 32 values
-- 5 bits for version : 32 values
-- 20 bits for line number: 1048576 values

forestLineMask,forestVersionMask,forestTreeMask,forestLenChangedMask :: Int
forestLineMask =          0xfffff -- bottom 20 bits
forestVersionMask    =  0x1f00000 -- next 5 bits
forestTreeMask       = 0x3e000000 -- next 5 bits
forestLenChangedMask = 0x40000000 -- top (non-sign) bit

forestVersionShift :: Int
forestVersionShift = 20

forestTreeShift :: Int
forestTreeShift    = 25


-- | Extract an encoded ForestLine from a GHC line
ghcLineToForestLine :: Int -> ForestLine
ghcLineToForestLine l = ForestLine ch tr v l'
  where
    l' =  l .&. forestLineMask
    v  = shiftR (l .&. forestVersionMask) forestVersionShift
    tr = shiftR (l .&. forestTreeMask)    forestTreeShift
    ch = (l .&. forestLenChangedMask) /= 0

-- TODO: check that the components are in range
forestLineToGhcLine :: ForestLine -> Int
forestLineToGhcLine fl =  (if (flSpanLengthChanged fl) then forestLenChangedMask else 0)
                        + (shiftL (flTreeSelector  fl) forestTreeShift)
                        + (shiftL (flInsertVersion fl) forestVersionShift)
                        + (flLine fl)


-- ---------------------------------------------------------------------

-- |Add a constant line and column offset to a span of tokens
addOffsetToToks :: (IsToken a) => SimpPos -> [a] -> [a]
addOffsetToToks (r,c) toks = map (\t -> increaseSrcSpan (r,c) t) toks

-- ---------------------------------------------------------------------

-- |Shift the whole token by the given offset
increaseSrcSpan :: (IsToken a) => SimpPos -> a -> a
increaseSrcSpan (lineAmount,colAmount) posToken
    = putSpan posToken newL
    where
        newL = Span (startLine + lineAmount, startCol + colAmount)
                    (endLine + lineAmount, endCol + colAmount)

        (Span (startLine, startCol) (endLine,endCol)) = getSpan posToken



-- ---------------------------------------------------------------------

{-
getLocatedStart :: Located t -> (Int, Int)
getLocatedStart (L l _) = getGhcLoc l

getLocatedEnd :: Located  t -> (Int, Int)
getLocatedEnd (L l _) = getGhcLocEnd l

getGhcLoc    (Span fm _to) = fm
getGhcLocEnd (Span _fm to) = to
-}

