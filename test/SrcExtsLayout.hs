module SrcExtsLayout (
    hseAllocTokens
  ) where

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.TokenUtils
import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Utils

import Language.Haskell.Exts.Annotated
import Language.KURE

import SrcExtsUtils
import SrcExtsKure

-- ---------------------------------------------------------------------

hseAllocTokens :: Module SrcSpanInfo -> [Loc TuToken] -> LayoutTree (Loc TuToken)
hseAllocTokens modu toks = r
  where
    r = undefined





-- ---------------------------------------------------------------------

type RewriteE a     = Rewrite Ctx KureM a
type TransformE a b = Transform Ctx KureM a b

-----------------------------------------------------------------

applyE :: TransformE a b -> a -> Either String b
applyE t = runKureM Right Left . applyT t initialContext

-----------------------------------------------------------------

initialContext = ""

-----------------------------------------------------------------
