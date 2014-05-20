module Language.Haskell.TokenUtils.Types
  (
    Entry(..)
  , TokenCache(..)
  , TreeId(..)
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
  , PosToken
  , Located(..)
  , Span(..)
  , showTokenStream
  , TokenLayout
  , LayoutTree
  , forestSpanToSimpPos
  , forestSpanVersionSet
  , treeStartEnd
  , groupTokensByLine
  , tokenRow
  , tokenCol
  , tokenColEnd
  , tokenLen
  , increaseSrcSpan
  , srcPosToSimpPos
  , addOffsetToToks
  , isComment
  , ghcLineToForestLine
  , forestLineToGhcLine

  , HasLoc(..)
  , IsToken(..)
  ) where

import Control.Exception
import Data.Bits
import Data.List
import Data.Tree

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | An entry in the data structure for a particular srcspan.
data Entry = Entry !ForestSpan -- The source span contained in this
                                  -- Node
                   !Layout     -- How the sub-elements nest
                   ![PosToken] -- ^The tokens for the SrcSpan if
                               --  subtree is empty
           | Deleted !ForestSpan -- The source span has been deleted
                     RowOffset   -- prior gap in lines
                     SimpPos     -- ^The gap between this span end and
                                 --  the start of the next in the
                                 --  fringe of the tree.
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

data TokenCache = TK
  { tkCache :: !(Map.Map TreeId (Tree Entry))
  , tkLastTreeId :: !TreeId
  }

-- ---------------------------------------------------------------------

class IsToken a where
  allocTokens :: t -> [a] -> LayoutTree

-- allocTokens :: GHC.ParsedSource -> [PosToken] -> LayoutTree

class HasLoc a where
  getStartPos :: a -> SimpPos
  getEndPos :: a -> SimpPos


type Token = String



type PosToken = (Located Token, String)

data Located e = L Span e
               deriving Show

data Span = Span (Row,Col) (Row,Col)
          deriving (Show,Eq)

showTokenStream :: [PosToken] -> String
showTokenStream toks = assert False undefined

data TokenLayout = TL (Tree Entry)

type LayoutTree = Tree Entry


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
treeStartEnd :: Tree Entry -> ForestSpan
treeStartEnd (Node (Entry sspan _ _) _) = sspan
treeStartEnd (Node (Deleted sspan _ _) _) = sspan

-- ---------------------------------------------------------------------

groupTokensByLine :: [PosToken] -> [[PosToken]]
groupTokensByLine xs = groupBy toksOnSameLine xs

toksOnSameLine :: PosToken -> PosToken -> Bool
toksOnSameLine t1 t2 = tokenRow t1 == tokenRow t2


-- ---------------------------------------------------------------------

tokenRow :: PosToken -> Int
tokenRow (L l _,_) = r
  where (Span (r,_) _) = l

tokenCol :: PosToken -> Int
tokenCol (L l _,_) = c
  where (Span (_,c) _) = l

tokenColEnd :: PosToken -> Int
tokenColEnd (L l _,_) = c
  where (Span _ (_,c)) = l

-- ---------------------------------------------------------------------

-- tokenLen :: PosToken -> Int
tokenLen (_,s)     = length s   --check this again! need to handle the tab key.

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
addOffsetToToks :: SimpPos -> [PosToken] -> [PosToken]
addOffsetToToks (r,c) toks = map (\t -> increaseSrcSpan (r,c) t) toks

-- ---------------------------------------------------------------------

increaseSrcSpan :: SimpPos -> PosToken -> PosToken
increaseSrcSpan (lineAmount,colAmount) posToken@(lt@(L _l t), s)
    = (L newL t, s)
    where
        newL = Span (startLine, startCol) (endLine, endCol)

        (startLine, startCol) = add1 $ getLocatedStart lt
        (endLine, endCol)     = add1 $ getLocatedEnd   lt

        add1 :: (Int, Int) -> (Int, Int)
        add1 (r,c) = (r+lineAmount,c+colAmount)

-- ---------------------------------------------------------------------

getLocatedStart :: Located t -> (Int, Int)
getLocatedStart (L l _) = getGhcLoc l

getLocatedEnd :: Located  t -> (Int, Int)
getLocatedEnd (L l _) = getGhcLocEnd l

getGhcLoc    (Span fm _to) = fm
getGhcLocEnd (Span _fm to) = to

isComment :: PosToken -> Bool
isComment = assert False undefined


