{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module SrcExtsKure
  (
  ) where

import Control.Monad

import Language.Haskell.Exts.Annotated
import Language.KURE

-- |Attempt token allocation using KURE library


-- ---------------------------------------------------------------------
-- First task is to defined the universe

-- We want to work with SrcSpans in general, as well as nodes
-- introducing layout.

data U = UModule         (Module SrcSpanInfo)
       | UModuleHead     (ModuleHead SrcSpanInfo)
       | UModuleName     (ModuleName SrcSpanInfo)
       | UExportSpecList (ExportSpecList SrcSpanInfo)
       | UDecl           (Decl SrcSpanInfo)
       | US String
-- ---------------------------------------------------------------------

-- Injection is a mapping from the outside world to/from the universe

instance Injection (Module SrcSpanInfo) U where
  inject = UModule

  project (UModule m) = Just m
  project _      = Nothing

-- -------------------------------------

instance Injection (ModuleHead SrcSpanInfo) U where
  inject = UModuleHead

  project (UModuleHead mh) = Just mh
  project _                = Nothing


-- -------------------------------------

instance Injection (ModuleName SrcSpanInfo) U where
  inject = UModuleName

  project (UModuleName mn) = Just mn
  project _                = Nothing


-- -------------------------------------

instance Injection (ExportSpecList SrcSpanInfo) U where
  inject = UExportSpecList

  project (UExportSpecList es) = Just es
  project _                    = Nothing


-- -------------------------------------

instance Injection (Decl SrcSpanInfo) U where
  inject = UDecl

  project (UDecl d) = Just d
  project _         = Nothing


-- -------------------------------------

instance Injection String U where
  inject = US

  project (US s) = Just s
  project _      = Nothing

-- ---------------------------------------------------------------------

-- Engine that drives it all is the Walker instance, defining allR

instance Walker c U where
  allR :: MonadCatch m => Rewrite c m U -> Rewrite c m U
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \ c -> \case
              UModule modu -> inject <$> applyR allRModule c modu
              _ -> undefined
    where
      allRModule :: (Module SrcSpanInfo) -> m (Module SrcSpanInfo)
      allRModule = readerT $ \case
                     _ -> idR
{-
   allR r = prefixFailMsg "allR failed: " $
            rewrite $ \ c -> \case
              GExpr e  -> inject <$> applyR allRexpr c e
              GCmd cm  -> inject <$> applyR allRcmd c cm
     where
       allRexpr = readerT $ \case
                    Add _ _  -> addAllR (extractR r) (extractR r)
                    ESeq _ _ -> eseqAllR (extractR r) (extractR r)
                    _        -> idR

       allRcmd  = readerT $ \case
                    Seq _ _    -> seqAllR (extractR r) (extractR r)
                    Assign _ _ -> assignR (extractR r)

-}

---------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

