module Language.Haskell.TokenUtils.Layout
  (
    -- allocTokens
    retrieveTokens
  ) where


import Control.Exception
import Data.Tree

import Language.Haskell.TokenUtils.Types
-- import Language.Haskell.TokenUtils.Utils

-- ---------------------------------------------------------------------

-- allocTokens = assert False undefined

-- ---------------------------------------------------------------------

retrieveTokens :: (IsToken a) => LayoutTree a -> [a]
retrieveTokens layout = go [] layout
  where
    -- go acc (Group _ _ xs)  = acc ++ (concat $ map (go []) xs)
    -- go acc (Leaf _ _ toks) = acc ++ toks
    go acc (Node (Entry _ _ []  ) xs) = acc ++ (concat $ map (go []) xs)
    go acc (Node (Entry _ _ toks)  _) = acc ++ toks
    go acc (Node (Deleted _ _ _)   _) = acc

-- ---------------------------------------------------------------------

