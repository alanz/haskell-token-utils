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

-- based on the fib example

applyFib :: TransformA b -> Arith -> Either String b
applyFib t = runKureM Right Left . applyT t mempty


-- runKureM :: (a -> b) -> (String -> b) -> KureM a -> b
--   Eliminator for KureM.

-- applyT :: Transform c m a b -> c -> a -> m b
--   Apply a transformation to a value and its context.

-- | For this simple example, the context is just an 'AbsolutePath',
-- and transformations always operates on 'Arith'.
type TransformA b = Transform (AbsolutePath Crumb) KureM Arith b
type RewriteA = TransformA Arith

-- ---------------------------------------------------------------------

-- based on Exp example

type RewriteE a     = Rewrite Ctx KureM a
type TransformE a b = Transform Ctx KureM a b

-----------------------------------------------------------------

applyE :: TransformE a b -> a -> Either String b
applyE t = runKureM Right Left . applyT t initialContext

-----------------------------------------------------------------

initialContext = ""

-----------------------------------------------------------------
