{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
module SrcExtsUtils
  (
    loadFile
  , TuToken(..)
  ) where

import Control.Exception
import Data.Generics
import Data.List
import Data.Tree

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Lexer

import Language.Haskell.TokenUtils.Types

import SrcExtsKure

-- ---------------------------------------------------------------------

-- parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])

-- Parse a source file from a string using a custom parse mode and retaining comments. 

data TuToken = T Token | C Comment
             deriving (Show,Eq)

loadFile :: FilePath -> IO (ParseResult (Module SrcSpanInfo, [Loc TuToken]))
loadFile fileName = do
  src <- readFile fileName
  let mtoks = lexTokenStream src
  let res = case parseFileContentsWithComments defaultParseMode src of
              ParseOk (modu,comments) -> case mtoks of
                ParseOk toks -> ParseOk (modu,comments,toks)
                ParseFailed l s -> ParseFailed l s
              ParseFailed l s -> ParseFailed l s
  case res of
    ParseOk (m, comments,toks) -> return $ ParseOk (m, (mergeToksAndComments toks comments))
    ParseFailed l s -> return (ParseFailed l s)

mergeToksAndComments :: [Loc Token] -> [Comment] -> [Loc TuToken]
mergeToksAndComments toks comments = go toks comments
  where
    tokToTu (Loc l t) = Loc l (T t)
    commentToTu c@(Comment _ l _) = Loc l (C c)

    go :: [Loc Token] -> [Comment] -> [Loc TuToken]
    go ts [] = map tokToTu ts
    go [] cs = map commentToTu cs
    go (ht:ts) (hc@(Comment _ l _):cs)
      = if loc ht < l
          then tokToTu ht : go ts (hc:cs)
          else commentToTu hc : go (ht:ts) cs

t = loadFile "../test/testdata/Layout/LetExpr.hs"

-- ---------------------------------------------------------------------

instance IsToken (Loc TuToken) where
  getSpan a = ss2s $ loc a
  putSpan (Loc _ss v) s = Loc (s2ss s) v

  tokenLen (Loc l (T t)) = hseTokenLen (Loc l t)
  tokenLen (Loc l (C (Comment _ _ s))) = length s -- beware of newlines

  isComment (Loc _ (C _)) = True
  isComment _ = False

  -- No empty tokens in HSE
  isEmpty _ = False

  isDo (Loc _ (T KW_Do))  = True
  isDo (Loc _ (T KW_MDo)) = True
  isDo _                  = False

  isElse (Loc _ (T KW_Else))  = True
  isElse _                    = False

  isIn (Loc _ (T KW_In))  = True
  isIn _                  = False

  isLet (Loc _ (T KW_Let))  = True
  isLet _                   = False

  isOf (Loc _ (T KW_Of))  = True
  isOf _                  = False

  isThen (Loc _ (T KW_Then)) = True
  isThen _                   = False

  isWhere (Loc _ (T KW_Where)) = True
  isWhere _                    = False

  tokenToString   = hseTokenToString
  showTokenStream = hseShowTokenStream

ss2s :: SrcSpan -> Span
ss2s (SrcSpan _fn sr sc er ec) = Span (sr,sc) (er,ec)

s2ss :: Span -> SrcSpan
s2ss (Span (sr,sc) (er,ec)) = (SrcSpan "<unknown>" sr sc er ec)


-- |Return the length of the token, using the string representation
-- where possible as it may have changed during a refactoing.
hseTokenLen :: Loc Token -> Int
hseTokenLen (Loc _ t) = length (showToken' t)

hseTokenToString :: Loc TuToken -> String
hseTokenToString (Loc _ (C (Comment True _ s))) = "{-" ++ s ++ "-}"
hseTokenToString (Loc _ (C (Comment False _ s))) = "--" ++ s
hseTokenToString (Loc _ (T tok)) = showToken' tok


{-
data Comment

  A Haskell comment. The Bool is True if the comment is multi-line, i.e. {- -}.

  Constructors
    Comment Bool SrcSpan String
-}

hseShowTokenStream :: [Loc TuToken] -> String
hseShowTokenStream ts2 = (go startLoc ts2 "") ++ "\n"
    where startLoc =  (1,1)
          go _ [] = id
          go (locLine,locCol) (tok@(Loc ss _):ts1)
              = case ss of
                SrcSpan _ tokLine tokCol er ec
                 | locLine == tokLine -> ((replicate (tokCol - locCol) ' ') ++)
                                       . (str ++)
                                       . go tokEnd ts1
                 | otherwise -> ((replicate (tokLine - locLine) '\n') ++)
                              . ((replicate (tokCol - 1) ' ') ++)
                              . (str ++)
                              . go tokEnd ts1
                  where tokEnd = (er,ec)
                        str = tokenToString tok


{-
showRichTokenStream ts = go startLoc ts ""
    where sourceFile = getFile $ map (getLoc . fst) ts
          getFile [] = panic "showRichTokenStream: No source file found"
          getFile (UnhelpfulSpan _ : xs) = getFile xs
          getFile (RealSrcSpan s : _) = srcSpanFile s
          startLoc = mkRealSrcLoc sourceFile 1 1
          go _ [] = id
          go loc ((L span _, str):ts)
              = case span of
                UnhelpfulSpan _ -> go loc ts
                RealSrcSpan s
                 | locLine == tokLine -> ((replicate (tokCol - locCol) ' ') ++)
                                       . (str ++)
                                       . go tokEnd ts
                 | otherwise -> ((replicate (tokLine - locLine) '\n') ++)
                              . ((replicate (tokCol - 1) ' ') ++) -- AZ: updated line
                              . (str ++)
                              . go tokEnd ts
                  where (locLine, locCol) = (srcLocLine loc, srcLocCol loc)
                        (tokLine, tokCol) = (srcSpanStartLine s, srcSpanStartCol s)
                        tokEnd = realSrcSpanEnd s

-}

-- ---------------------------------------------------------------------

-- This function is copied here from haskell-src-exts
-- module Language.Haskell.Exts.InternalLexer until issue
-- https://github.com/haskell-suite/haskell-src-exts/issues/119 is
-- resolved

------------------------------------------------------------------
-- "Pretty" printing for tokens

showToken' :: Token -> String
showToken' t = case t of
  VarId s           -> s
  QVarId (q,s)      -> q ++ '.':s
  IDupVarId s       -> '?':s
  ILinVarId s       -> '%':s
  ConId s           -> s
  QConId (q,s)      -> q ++ '.':s
  DVarId ss         -> concat $ intersperse "-" ss
  VarSym s          -> s
  ConSym s          -> s
  QVarSym (q,s)     -> q ++ '.':s
  QConSym (q,s)     -> q ++ '.':s
  IntTok (_, s)         -> s
  FloatTok (_, s)       -> s
  Character (_, s)      -> '\'':s ++ "'"
  StringTok (_, s)      -> '"':s ++ "\""
  IntTokHash (_, s)     -> s ++ "#"
  WordTokHash (_, s)    -> s ++ "##"
  FloatTokHash (_, s)   -> s ++ "#"
  DoubleTokHash (_, s)  -> s ++ "##"
  CharacterHash (_, s)  -> '\'':s ++ "'#"
  StringHash (_, s)     -> '"':s ++ "\"#"
  LeftParen         -> "("
  RightParen        -> ")"
  LeftHashParen     -> "(#"
  RightHashParen    -> "#)"
  SemiColon         -> ";"
  LeftCurly         -> "{"
  RightCurly        -> "}"
  VRightCurly       -> "virtual }"
  LeftSquare        -> "["
  RightSquare       -> "]"
  Comma             -> ","
  Underscore        -> "_"
  BackQuote         -> "`"
  Dot               -> "."
  DotDot            -> ".."
  Colon             -> ":"
  DoubleColon       -> "::"
  Equals            -> "="
  Backslash         -> "\\"
  Bar               -> "|"
  LeftArrow         -> "<-"
  RightArrow        -> "->"
  At                -> "@"
  Tilde             -> "~"
  DoubleArrow       -> "=>"
  Minus             -> "-"
  Exclamation       -> "!"
  Star              -> "*"
  LeftArrowTail     -> ">-"
  RightArrowTail    -> "-<"
  LeftDblArrowTail  -> ">>-"
  RightDblArrowTail -> "-<<"
  THExpQuote        -> "[|"
  THPatQuote        -> "[p|"
  THDecQuote        -> "[d|"
  THTypQuote        -> "[t|"
  THCloseQuote      -> "|]"
  THIdEscape s      -> '$':s
  THParenEscape     -> "$("
  THVarQuote        -> "'"
  THTyQuote         -> "''"
  THQuasiQuote (n,q) -> "[$" ++ n ++ "|" ++ q ++ "]"
  RPGuardOpen       -> "(|"
  RPGuardClose      -> "|)"
  RPCAt             -> "@:"
  XCodeTagOpen      -> "<%"
  XCodeTagClose     -> "%>"
  XStdTagOpen       -> "<"
  XStdTagClose      -> ">"
  XCloseTagOpen     -> "</"
  XEmptyTagClose    -> "/>"
  XPCDATA s         -> "PCDATA " ++ s
  XRPatOpen         -> "<["
  XRPatClose        -> "]>"
  PragmaEnd         -> "#-}"
  RULES             -> "{-# RULES"
  INLINE b          -> "{-# " ++ if b then "INLINE" else "NOINLINE"
  INLINE_CONLIKE    -> "{-# " ++ "INLINE_CONLIKE"
  SPECIALISE        -> "{-# SPECIALISE"
  SPECIALISE_INLINE b -> "{-# SPECIALISE " ++ if b then "INLINE" else "NOINLINE"
  SOURCE            -> "{-# SOURCE"
  DEPRECATED        -> "{-# DEPRECATED"
  WARNING           -> "{-# WARNING"
  SCC               -> "{-# SCC"
  GENERATED         -> "{-# GENERATED"
  CORE              -> "{-# CORE"
  UNPACK            -> "{-# UNPACK"
  OPTIONS (mt,s)    -> "{-# OPTIONS" ++ maybe "" (':':) mt ++ " ..."
--  CFILES  s         -> "{-# CFILES ..."
--  INCLUDE s         -> "{-# INCLUDE ..."
  LANGUAGE          -> "{-# LANGUAGE"
  ANN               -> "{-# ANN"
  KW_As         -> "as"
  KW_By         -> "by"
  KW_Case       -> "case"
  KW_Class      -> "class"
  KW_Data       -> "data"
  KW_Default    -> "default"
  KW_Deriving   -> "deriving"
  KW_Do         -> "do"
  KW_MDo        -> "mdo"
  KW_Else       -> "else"
  KW_Family     -> "family"
  KW_Forall     -> "forall"
  KW_Group      -> "group"
  KW_Hiding     -> "hiding"
  KW_If         -> "if"
  KW_Import     -> "import"
  KW_In         -> "in"
  KW_Infix      -> "infix"
  KW_InfixL     -> "infixl"
  KW_InfixR     -> "infixr"
  KW_Instance   -> "instance"
  KW_Let        -> "let"
  KW_Module     -> "module"
  KW_NewType    -> "newtype"
  KW_Of         -> "of"
  KW_Proc       -> "proc"
  KW_Rec        -> "rec"
  KW_Then       -> "then"
  KW_Type       -> "type"
  KW_Using      -> "using"
  KW_Where      -> "where"
  KW_Qualified  -> "qualified"
  KW_Foreign    -> "foreign"
  KW_Export     -> "export"
  KW_Safe       -> "safe"
  KW_Unsafe     -> "unsafe"
  KW_Threadsafe -> "threadsafe"
  KW_Interruptible -> "interruptible"
  KW_StdCall    -> "stdcall"
  KW_CCall      -> "ccall"
  XChildTagOpen -> "<%>"
  KW_CPlusPlus  -> "cplusplus"
  KW_DotNet     -> "dotnet"
  KW_Jvm        -> "jvm"
  KW_Js         -> "js"
  KW_CApi       -> "capi"

  EOF           -> "EOF"


-- ---------------------------------------------------------------------

instance Allocatable (Module SrcSpanInfo) (Loc TuToken) where
  allocTokens = hseAllocTokens


hseAllocTokens :: Module SrcSpanInfo -> [Loc TuToken] -> LayoutTree (Loc TuToken)
hseAllocTokens modu toks = r
  where
    -- ss = foo modu
    ss = bar modu
    r = error $ "foo=" ++ show ss


bar :: Module SrcSpanInfo -> Tree SrcSpan
bar modu = r
  where
    nullTree = Node (SrcSpan "" 0 0 0 0) []

    start :: Tree SrcSpan -> Tree SrcSpan
    start old = old

    r = synthesize nullTree redf (start `mkQ` bb) modu

    redf :: Tree SrcSpan -> Tree SrcSpan -> Tree SrcSpan
    redf (Node s sub)  old = (Node s (sub ++ [old]))

    bb :: SrcSpanInfo -> Tree SrcSpan -> Tree SrcSpan
    bb (SrcSpanInfo ss sss) (Node s sub) = Node ss (map (\s -> Node s []) sss)
    -- bb (SrcSpanInfo ss sss) = Node ss (map (\s -> Node s []) sss)




foo :: Module SrcSpanInfo -> [SrcSpan]
foo modu = r
  where
   r = everything (++) ([] `mkQ` blah) modu

   blah :: SrcSpanInfo -> [SrcSpan]
   blah (SrcSpanInfo ss sss) = (ss:sss)

-- synthesize :: s -> (t -> s -> s) -> GenericQ (s -> t) -> GenericQ t
-- synthesize z o f

-- Bottom-up synthesis of a data structure;
--  1st argument z is the initial element for the synthesis;
--  2nd argument o is for reduction of results from subterms;
--  3rd argument f updates the synthesised data according to the given term
