{-# Language MultiParamTypeClasses #-}
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
  , SimpSpan
  , Layout(..)
  , EndOffset(..)
  , Outputable(..)
  , TokenLayout
  , LayoutTree
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

-- import Control.Exception
import Data.Bits
import Data.Tree

import qualified Text.PrettyPrint as P

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

type RowOffset = Int
type ColOffset = Int
type Row       = Int
type Col       = Int

type SimpPos = (Int,Int) -- Line, column
type SimpSpan = (SimpPos,SimpPos)

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
  -- |Construct a `LayoutTree` from a Haskell AST and a stream of tokens.
  allocTokens :: (IsToken a) => b -> [a] -> LayoutTree a


-- |The IsToken class captures the different token type in use. For
-- GHC it represents the type returned by `GHC.getRichTokenStream`,
-- namely [(GHC.Located GHC.Token, String)]
-- For haskell-src-exts this is the reult of `lexTokenStream`, namely `[HSE.Loc HSE.Token]`
class (Show a,HasLoc a) => IsToken a where

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

  -- |Create a stream of tokens from source
  lexStringToTokens :: SimpSpan -> String -> [a]

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

  getSpan     :: a -> SimpSpan
  getSpan a = (getLoc a,getLocEnd a)

  putSpan     :: a -> SimpSpan -> a

{-
data Located e = L Span e
               deriving Show

data Span = Span (Row,Col) (Row,Col)
          deriving (Show,Eq,Ord)

nullSpan :: Span
nullSpan = Span (0,0) (0,0)
-}

data TokenLayout a = TL (Tree (Entry a))

type LayoutTree a = Tree (Entry a)

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

class Outputable a where
  ppr :: a -> P.Doc

-- ---------------------------------------------------------------------

