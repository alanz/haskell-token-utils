module Language.Haskell.TokenUtils.Types
  (
    Entry(..)
  , ForestSpan
  , ForestPos
  , ForestLine(..)
  , RowOffset
  , ColOffset
  , Row
  , Col
  , SimpPos
  , Layout(..)
  , EndOffset
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
  ) where

import Control.Exception
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

tokenLen :: PosToken -> Int
tokenLen (_,s)     = length s   --check this again! need to handle the tab key.

-- ---------------------------------------------------------------------

srcPosToSimpPos :: (Int,Int) -> (Int,Int)
srcPosToSimpPos (sr,c) = (l,c)
  where
    (ForestLine _ _ _ l) = ghcLineToForestLine sr

ghcLineToForestLine = assert False undefined

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

