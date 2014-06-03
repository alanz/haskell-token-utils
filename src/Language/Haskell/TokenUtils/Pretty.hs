{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.TokenUtils.Pretty
  (
  Outputable(..)
  , showPpr
  ) where

import Data.Semigroup hiding ( (<>) )

import Language.Haskell.TokenUtils.DualTree
import Language.Haskell.TokenUtils.Types

import Text.PrettyPrint

import qualified Data.List.NonEmpty as NE
import qualified Data.Tree.DUAL.Internal as I


-- ---------------------------------------------------------------------

showPpr :: Outputable a => a -> String
showPpr a = render $ ppr a

-- ---------------------------------------------------------------------

class Outputable a where
  ppr :: a -> Doc

-- ---------------------------------------------------------------------

instance (IsToken a) => Outputable (SourceTree a) where
  ppr (I.DUALTree ot)
      = case getOption ot of
             Nothing -> text "Nothing"
             Just t  -> ppr t

instance (IsToken a) =>
         Outputable (I.DUALTreeU Transformation (Up a) Annot (Prim a)) where
  ppr (I.DUALTreeU (u,t)) = parens $ ppr u <> comma $$ ppr t

instance (IsToken a) =>
         Outputable (I.DUALTreeNE Transformation (Up a) Annot (Prim a)) where
  ppr (I.Leaf u l)   = parens $ hang (text "Leaf")   1 (ppr u $$ ppr l)
  ppr (I.LeafU u)    = parens $ hang (text "LeafU")  1 (ppr u)
  ppr (I.Concat dts) = parens $ hang (text "Concat") 1 (ppr dts)
  ppr (I.Act d t)    = parens $ hang (text "Act")    1 (ppr d $$ ppr t)
  ppr (I.Annot a t)  = parens $ hang (text "Annot")  1 (ppr a $$ ppr t)

instance (IsToken a) => Outputable (Prim a) where
  ppr (PToks toks) = parens $ text "PToks" <+> text (show toks)
  ppr (PDeleted ss pg p) = parens $ text "PDeleted" <+> ppr ss
                               <+> ppr pg <+> ppr p

instance Outputable Transformation where
  ppr (TAbove co bo p1 p2 eo)  = parens $ text "TAbove" <+> ppr co
                              <+> ppr bo
                              <+> ppr p1  <+> ppr p2
                              <+> ppr eo

instance Outputable EndOffset where
  ppr None = text "None"
  ppr (SameLine co)     = parens $ text "SameLine" <+> ppr co
  ppr (FromAlignCol rc) = parens $ text "FromAlignCol" <+> ppr rc

instance Outputable Annot where
  ppr (Ann str) = parens $ text "Ann" <+> text str
  ppr (ADeleted ss pg p) = parens $ text "ADeleted" <+> ppr ss
                           <+> ppr pg <+> ppr p
  ppr (ASubtree ss)      = parens $ text "ASubtree" <+> ppr ss

instance (IsToken a) => Outputable (Up a) where
  ppr (Up ss a ls ds) = parens $ hang (text "Up") 1
                                 ((ppr ss <+> ppr a) $$ ppr ls $$ ppr ds)
  ppr (UDeleted d)  = parens $ text "UDeleted" <+> ppr d

instance Outputable Alignment where
  ppr ANone     = text "ANone"
  ppr AVertical = text "AVertical"

instance Outputable DeletedSpan where
  ppr (DeletedSpan ss ro p) = parens $ (text "DeletedSpan")
                               <+> ppr ss <+> ppr ro
                               <+> ppr p


instance Outputable Span where
  ppr (Span sp ep) = parens $ text "Span" <+> ppr sp <+> ppr ep

instance (Outputable a) => Outputable (NE.NonEmpty a) where
  -- ppr (x NE.:| xs) = parens $ hang (text "NonEmpty") 1 (ppr (x:xs))
  ppr (x NE.:| xs) = (ppr (x:xs))

instance (IsToken a) => Outputable (Line a) where
  ppr (Line r c o s f str) = parens $ text "Line" <+> ppr r
                         <+> ppr c <+> ppr o
                         <+> ppr s <+> ppr f
                         <+> text ("\"" ++ (showTokenStream str) ++ "\"")
                         -- <+> text (show str) -- ++AZ++ debug

instance Outputable Source where
  ppr SOriginal = text "SOriginal"
  ppr SAdded    = text "SAdded"
  ppr SWasAdded = text "SWasAdded"

instance Outputable LineOpt where
  ppr ONone  = text "ONone"
  ppr OGroup = text "OGroup"

instance Outputable ForestLine where
  ppr (ForestLine lc sel v l) = parens $ text "ForestLine"
                                       <+> ppr lc <+> int sel
                                       <+> int v <+> int l

instance Outputable Bool where
  ppr True  = text "True"
  ppr False = text "False"

instance Outputable Row where
  ppr r = int r

instance Outputable a => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a,Outputable b) => Outputable (a,b) where
    ppr (x,y) = parens (ppr x <> comma <> ppr y)


