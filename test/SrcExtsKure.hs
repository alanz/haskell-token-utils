{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module SrcExtsKure
  (
  Ctx(..)
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
       | UMModuleHead    (Maybe (ModuleHead SrcSpanInfo))
       | UModulePragmas  [ModulePragma SrcSpanInfo]
       | UImportDecls    [ImportDecl SrcSpanInfo]
       | UExportSpecList (ExportSpecList SrcSpanInfo)
       | UDecls          [Decl SrcSpanInfo]
       | UDecl           (Decl SrcSpanInfo)
       | US String
       | USS SrcSpanInfo
       deriving (Show)

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

instance Injection (Maybe (ModuleHead SrcSpanInfo)) U where
  inject = UMModuleHead

  project (UMModuleHead mh) = Just mh
  project _                 = Nothing


-- -------------------------------------

instance Injection (ModuleName SrcSpanInfo) U where
  inject = UModuleName

  project (UModuleName mn) = Just mn
  project _                = Nothing


-- -------------------------------------

instance Injection [ModulePragma SrcSpanInfo] U where
  inject = UModulePragmas

  project (UModulePragmas mn) = Just mn
  project _                   = Nothing


-- -------------------------------------

instance Injection [ImportDecl SrcSpanInfo] U where
  inject = UImportDecls

  project (UImportDecls mn) = Just mn
  project _                 = Nothing


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

instance Injection [Decl SrcSpanInfo] U where
  inject = UDecls

  project (UDecls d) = Just d
  project _          = Nothing

-- -------------------------------------

instance Injection SrcSpanInfo U where
  inject = USS

  project (USS s) = Just s
  project _       = Nothing

-- -------------------------------------

instance Injection String U where
  inject = US

  project (US s) = Just s
  project _      = Nothing

-- ---------------------------------------------------------------------

type Ctx = String -- For now

-- ---------------------------------------------------------------------

-- Engine that drives it all is the Walker instance, defining allR

instance Walker Ctx U where
  allR :: MonadCatch m => Rewrite Ctx m U -> Rewrite Ctx m U
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \ c -> \case
              -- UModule modu -> return (UModule <$> allRModule c modu)
              UModule modu -> inject <$> applyR allRModule c modu
              unk -> fail $ "allR U:no case for " ++ show unk
    where
      -- allRModule :: (MonadCatch m) => Rewrite Ctx m (Module SrcSpanInfo)
      allRModule = readerT $ \case
        (Module _ _ _ _ _) -> moduleAllR (extractR r) (extractR r) (extractR r) (extractR r) (extractR r)
        _                  -> idR
{-
   allR r = prefixFailMsg "allR failed: " $
            rewrite $ \ c -> \case
              GExpr e  -> inject <$> applyR allRexpr c e
              GCmd cm  -> inject <$> applyR allRcmd c cm
     where
       allRexpr = readerT $ \case
                    Add _ _  -> addAllR (extractR r) (extractR r)c
                    ESeq _ _ -> eseqAllR (extractR r) (extractR r)
                    _        -> idR

       allRcmd  = readerT $ \case
                    Seq _ _    -> seqAllR (extractR r) (extractR r)
                    Assign _ _ -> assignR (extractR r)

-}
{-
rewrite :: (c -> a -> m a) -> Rewrite c m a
  The primitive way of building a rewrite.

applyR :: Rewrite c m a -> c -> a -> m a
  Apply a rewrite to a value and its context.

applyT :: Transform c m a b -> c -> a -> m b
  Apply a transformation to a value and its context.

readerT :: (a -> Transform c m a b) -> Transform c m a b
  Look at the argument to the transformation before choosing which Transform to use. 

-}
---------------------------------------------------------------------------

moduleT :: (MonadCatch m)
        => Transform Ctx m c0 a1
        -> Transform Ctx m (Maybe (ModuleHead c0)) a2
        -> Transform Ctx m [ModulePragma c0] a3
        -> Transform Ctx m [ImportDecl c0] a4
        -> Transform Ctx m [Decl c0] a5
        -> (a1 -> a2 -> a3 -> a4 -> a5 -> b)
        -> Transform Ctx m (Module c0) b
moduleT t1 t2 t3 t4 t5 f = transform $ \c -> \case
  Module l mmh mps imps ds -> f <$> applyT t1 c l
                                <*> applyT t2 c mmh
                                <*> applyT t3 c mps
                                <*> applyT t4 c imps
                                <*> applyT t5 c ds
  _                        -> fail "not a Module"

-- moduleAllR :: (Monad m)
moduleAllR :: MonadCatch m
           => Rewrite Ctx m SrcSpanInfo
           -> Rewrite Ctx m (Maybe (ModuleHead SrcSpanInfo))
           -> Rewrite Ctx m [ModulePragma SrcSpanInfo]
           -> Rewrite Ctx m [ImportDecl SrcSpanInfo]
           -> Rewrite Ctx m [Decl SrcSpanInfo]
           -> Rewrite Ctx m (Module SrcSpanInfo)
moduleAllR r1 r2 r3 r4 r5 = moduleT r1 r2 r3 r4 r5 Module

{-
addT :: (ExtendPath c Int, Monad m) => Transform c m Expr a1 -> Transform c m Expr a2 -> (a1 -> a2 -> b) -> Transform c m Expr b
addT t1 t2 f = transform $ \ c -> \case
                                     Add e1 e2 -> f <$> applyT t1 (c @@ 0) e1 <*> applyT t2 (c @@ 1) e2
                                     _         -> fail "not an Add"

addAllR :: (ExtendPath c Int, Monad m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addAllR r1 r2 = addT r1 r2 Add

addAnyR :: (ExtendPath c Int, MonadCatch m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addAnyR r1 r2 = unwrapAnyR $ addAllR (wrapAnyR r1) (wrapAnyR r2)

addOneR :: (ExtendPath c Int, MonadCatch m) => Rewrite c m Expr -> Rewrite c m Expr -> Rewrite c m Expr
addOneR r1 r2 = unwrapOneR $ addAllR (wrapOneR r1) (wrapOneR r2)
-}


---------------------------------------------------------------------------

-- I find it annoying that Applicative is not a superclass of Monad.
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM
{-# INLINE (<$>) #-}

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
{-# INLINE (<*>) #-}

