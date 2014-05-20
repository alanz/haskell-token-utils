{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.TokenUtils.API
  (
    IsToken(..)
  , HasLoc(..)
  ) where

import Language.Haskell.TokenUtils.Types

class RoundTrip a where
  -- defined as a located token and a string to give a PosToken
  data PosToken :: * -> * -> *







