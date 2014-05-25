module SrcExtsLayout (
    hseAllocTokens
  ) where

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Utils

import Language.Haskell.Exts.Annotated

import SrcExtsUtils

-- ---------------------------------------------------------------------

hseAllocTokens :: Module SrcSpanInfo -> [Loc TuToken] -> LayoutTree (Loc TuToken)
hseAllocTokens modu toks = r
  where
    r = undefined
