{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.TokenUtils.API
  (
  -- * Creating a `LayoutTree`
    Allocatable(..)
  -- * Eliminating a `LayoutTree`
  , renderLayoutTree

  -- * Properties required for a `LayoutTree`
  , IsToken(..)
  , HasLoc(..)
  ) where

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.Types






