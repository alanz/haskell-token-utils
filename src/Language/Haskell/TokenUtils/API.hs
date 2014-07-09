{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.TokenUtils.API
  (
  -- * Creating a `LayoutTree`
  -- |This should only be required by impementors of token utils backends
    Allocatable(..)
  -- * Eliminating a `LayoutTree`
  -- |This should only be required by impementors of token utils backends
  , renderLayoutTree

  -- * Properties required for a `LayoutTree`
  -- |This should only be required by impementors of token utils backends
  , IsToken(..)
  , HasLoc(..)
  ) where

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Layout
import Language.Haskell.TokenUtils.Types






