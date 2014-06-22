{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.TokenUtils.HSE.Layout
  (
    loadFile
  , loadFileWithMode
  , templateHaskellMode
  , TuToken(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Generics hiding (GT)
import Data.List
import Data.Monoid
import Data.Tree

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Lexer

import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.Utils

import qualified Data.Set as Set

-- import SrcExtsKure

import Debug.Trace

-- ---------------------------------------------------------------------

-- parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])

-- Parse a source file from a string using a custom parse mode and retaining comments. 

data TuToken = T Token | C Comment
             deriving (Show,Eq)

loadFile :: FilePath -> IO (ParseResult (Module SrcSpanInfo, [Loc TuToken]))
loadFile fileName = loadFileWithMode defaultParseMode fileName

templateHaskellMode :: ParseMode
templateHaskellMode
  = defaultParseMode { extensions = (EnableExtension TemplateHaskell):(extensions defaultParseMode)}

loadFileWithMode :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Loc TuToken]))
loadFileWithMode parseMode fileName = do
  src <- readFile fileName
  let mtoks = lexTokenStream src
  let res = case parseFileContentsWithComments parseMode src of
              ParseOk (modu,comments) -> case mtoks of
                ParseOk toks -> ParseOk (modu,comments,toks)
                ParseFailed l s -> ParseFailed l s
              ParseFailed l s -> ParseFailed l s
  case res of
    ParseOk (m, comments,toks) -> return $ ParseOk (m, (mergeToksAndComments toks comments))
    ParseFailed l s -> return (ParseFailed l s)


-- ---------------------------------------------------------------------

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
    ss = allocTokens' modu
    ss1 = (ghead "hseAllocTokens" ss)
    ss2 = addEndOffsets ss1 toks
    ss3 = decorate ss2 toks
    -- r = error $ "foo=" ++ show ss2
    -- r = error $ "foo=" ++ drawTreeCompact (Node nullSpan ss)
    -- r = error $ "foo=" ++ drawForestEntry ss
    -- r = error $ "foo toks=" ++ show toks
    -- r = error $ "foo modu=" ++ show modu
    r = ss3


-- ---------------------------------------------------------------------

-- Allocate all the tokens to the given tree
decorate :: LayoutTree (Loc TuToken) -> [Loc TuToken] -> LayoutTree (Loc TuToken)
decorate tree toks = go toks tree
  where
    go :: [Loc TuToken] -> LayoutTree (Loc TuToken) -> LayoutTree (Loc TuToken)
    go ts (Node e subs) = (Node e subs'')
      where
        -- b = map (\t -> let f = treeStartEnd t in (getLoc f, getLocEnd f)) subs

        doOne :: ([Loc TuToken],[LayoutTree (Loc TuToken)]) -> LayoutTree (Loc TuToken) -> ([Loc TuToken],[LayoutTree (Loc TuToken)])
        doOne (ts1,acc) tree = (ts1',acc')
          where
            ss = treeStartEnd tree
            (before,middle,after) = splitToksIncComments (getLoc ss,getLocEnd ss) ts1
            ts1' = after
            tree' = case tree of
              Node (Entry ss1 lo []) [] -> [Node (Entry ss1 lo middle) []]
              _ -> [go middle tree]
            acc' = acc ++ makeLeafFromToks before ++ tree'

        (end,subs') = foldl' doOne (ts,[]) subs
        -- (end,subs') = doOne (ts,[]) (head subs)
        subs'' = subs' ++ makeLeafFromToks end

    go ts tr = error $ "decorate:not processing :" ++ show (ts,tr)


-- ---------------------------------------------------------------------
{-
Notes re HSE let binds

data Exp l
    ....
    | Let l (Binds l) (Exp l)               -- ^ local declarations with @let@ ... @in@ ...

instance ExactP Exp where
  exactP exp = case exp of
    .....
    Let l bs e      ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "let"
            exactPC bs
            printStringAt (pos b) "in"
            exactPC e
         _ -> errorEP "ExactP: Exp: Let is given wrong number of srcInfoPoints"
    ....
    Case l e alts   ->
        case srcInfoPoints l of
         a:b:pts -> do
            printString "case"
            exactPC e
            printStringAt (pos b) "of"
            layoutList pts alts
         _ -> errorEP "ExactP: Exp: Case is given too few srcInfoPoints"
    Do l stmts      ->
        case srcInfoPoints l of
         a:pts -> do
            printString "do"
            layoutList pts stmts
         _ -> errorEP "ExactP: Exp: Do is given too few srcInfoPoints"
    MDo l stmts     ->
        case srcInfoPoints l of
         a:pts -> do
            printString "mdo"
            layoutList pts stmts
         _ -> errorEP "ExactP: Exp: Mdo is given wrong number of srcInfoPoints"

Need let/in
     where
     do
     case
-}

-- ---------------------------------------------------------------------

-- allocTokens' :: Module SrcSpanInfo -> [LayoutTree (Loc TuToken)]
allocTokens' :: Data a => a -> [LayoutTree (Loc TuToken)]
allocTokens' modu = r
  where
    start :: [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    start old = old

    -- r = synthesize [] redf (start `mkQ` bb
    -- NOTE: the token re-alignement needs a left-biased tree, not a right-biased one, hence synthesizel
    r = synthesizel [] redf (start `mkQ` bb
                           `extQ` letExp
                           `extQ` expr
                           `extQ` match
                           `extQ` binds
                           `extQ` decl
                           `extQ` stmt
                           ) modu


    -- ends up as GenericQ (SrcSpanInfo -> LayoutTree TuToken)
    bb :: SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    bb (SrcSpanInfo ss _sss) vv = [Node (Entry (sf $ ss2s ss) NoChange []) vv]

    letExp :: Exp SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    letExp (Let l@(SrcSpanInfo ss _) bs e) vv =
        case srcInfoPoints l of
          [letPos,inPos] ->
            let
              (Span letStart letEnd) = ss2s letPos
              (Span inStart inEnd)   = ss2s inPos
              io = FromAlignCol letStart
              (Span start end) = ss2s ss
              eo = FromAlignCol inEnd
            in
               trace ("let:" ++ show (ss,map treeStartEnd (subTreeOnly vv)))
               -- trace ("let:" ++ show (ss,map treeStartEnd (subTreeOnly vv),drawTreeCompact (head vv)))
               [Node (Entry (sf $ ss2s ss) (Above io start end eo) []) [(makeGroup vv)]]
          _ -> vv
    letExp _ vv = vv

    -- ---------------------------------

    expr :: Exp SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    expr (Do l@(SrcSpanInfo ss _) stmts) vv =
      case srcInfoPoints l of
        pts@(doPos:fstPos:_) ->
          let
            (Span doStart _doEnd) = ss2s doPos
            (Span _ ssEnd) = ss2s ss
            io = FromAlignCol doStart
            (Span start _) = ss2s fstPos
            (Span _ end) = ss2s (last pts)
            eo = None -- will be calculated later
            subs = concatMap allocTokens' stmts
          in
             trace ("do:" ++ show (ss,map treeStartEnd (subTreeOnly vv),srcInfoPoints l))
             -- trace ("do:" ++ show (ss,map treeStartEnd (subTreeOnly vv),drawTreeCompact (head vv)))
             [makeGroup [Node (Entry (sf $ ss2s doPos) NoChange []) [],
                         Node (Entry (sf $ (Span doStart ssEnd)) (Above io start end eo) []) subs]]
        (doPos:_) -> error $ "allocTokens'.expr.Do:missing statements:" ++ show (l,stmts)
        _ -> vv

    expr _ vv = vv

    -- ---------------------------------

    match :: Match SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    match (Match l _ _ _ Nothing) vv = vv
    match (Match l@(SrcSpanInfo ss _) name pats rhs (Just (BDecls (SrcSpanInfo bs _) _))) vv =
      case srcInfoPoints l of
        (wherePos:_) ->
          let
            (Span whereStart whereEnd) = ss2s wherePos
            io = FromAlignCol whereStart
            -- (Span start end) = ss2s ss
            (Span start end) = ss2s bs
            eo = None -- will be calculated later FromAlignCol (0,0)
          in
             trace ("match:" ++ show (ss,map treeStartEnd (subTreeOnly vv)))
             -- trace ("match:" ++ show (ss,map treeStartEnd (subTreeOnly vv),drawTreeCompact (head vv)))
             [Node (Entry (sf $ ss2s ss) (Above io start end eo) []) (subTreeOnly vv)]
        _ -> vv

    -- -----------------------

    binds :: Binds SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    binds (BDecls  l@(SrcSpanInfo ss _) bs) vv =
      let
        -- TODO: properly calculate `so` and `eo`
        ro = 0
        co = 0
        so = makeOffset ro co
        (Span start end) = ss2s ss
        -- eo = FromAlignCol (1,-39)
        eo = None -- will be added later
        vv' = case vv of
         [Node (Entry fss _ _) sub] ->
           if fss == (sf $ ss2s ss) then sub else vv
         _ -> vv
      in
        trace ("binds:BDecls" ++ show (ss, map treeStartEnd vv))
        -- [Node (Entry (sf $ ss2s ss) (Above so start end eo) []) (subTreeOnly vv')]
        [Node (Entry (sf $ ss2s ss) NoChange []) (subTreeOnly vv')]
    binds (IPBinds l@(SrcSpanInfo ss _) bs) vv = vv

    -- --------------

    decl :: Decl SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    decl (FunBind l@(SrcSpanInfo ss _) matches) vv =
      trace ("decl:FunBind" ++ show (ss, map treeStartEnd vv))
      [Node (Entry (sf $ ss2s ss) NoChange []) vv]

    decl (PatBind   (SrcSpanInfo _  _) _    _     _    Nothing) vv = vv
    decl (PatBind l@(SrcSpanInfo ss _) _pat _mtyp _rhs (Just (BDecls (SrcSpanInfo bs _) _))) vv =
      case srcInfoPoints l of
        (wherePos:_) ->
          let
            (Span whereStart _whereEnd) = ss2s wherePos
            io = FromAlignCol whereStart
            (Span start end) = ss2s bs
            eo = None -- will be calculated later FromAlignCol (0,0)
          in
             trace ("decl:patBind:" ++ show (ss,map treeStartEnd (subTreeOnly vv)))
             -- trace ("decl:patBind:" ++ show (ss,map treeStartEnd (subTreeOnly vv),drawTreeCompact (head vv)))
             [Node (Entry (sf $ ss2s ss) (Above io start end eo) []) (subTreeOnly vv)]
        _ -> vv

    decl _ vv = vv

    -- --------------

    stmt :: Stmt SrcSpanInfo -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    stmt (LetStmt l@(SrcSpanInfo ss _) _binds) vv =
      case srcInfoPoints l of
        pts@(letPos:_) ->
          let
            (Span letStart _letEnd) = ss2s letPos
            (Span _ ssEnd) = ss2s ss
            io = FromAlignCol letStart
            (Span start end) = fs $ treeStartEnd (ghead "stmt.letStmt" (subTreeOnly vv)) -- ss2s fstPos
            eo = None -- will be calculated later
          in
             trace ("stmt:LetStmt:" ++ show (ss,map treeStartEnd (subTreeOnly vv),srcInfoPoints l))
             -- trace ("dstmt:LetStmt:" ++ show (ss,map treeStartEnd (subTreeOnly vv),drawTreeCompact (head vv)))
             [makeGroup [Node (Entry (sf $ ss2s letPos) NoChange []) [],
                         Node (Entry (sf $ (Span letStart ssEnd)) (Above io start end eo) []) (subTreeOnly vv)]]
        _ -> error $ "allocTokens'.stmt:LetStmt:missing statements:" ++ show (l,_binds)
        -- _ -> vv

    stmt _ vv = vv

    -- --------------

    mergeSubs as bs = as ++ bs

    redf :: [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)] -> [LayoutTree (Loc TuToken)]
    redf [] b = b
    redf a [] = a

    redf [a@(Node e1@(Entry s1 l1 []) sub1)]  [b@(Node _e2@(Entry s2 l2 []) sub2)]
      =
        let
          (as,ae) = spanStartEnd $ fs $ treeStartEnd a
          (bs,be) = spanStartEnd $ fs $ treeStartEnd b
          ss = combineSpans s1 s2
          ret =
           case (compare as bs,compare ae be) of
             (EQ,EQ) -> [Node (Entry s1 (l1 <> l2) []) (sub1 ++ sub2)]

             (LT,EQ) -> [Node (Entry ss (l1 <> l2) []) (mergeSubs sub1 [b])]    -- b is sub of a
             (GT,EQ) -> [Node (Entry ss (l1 <> l2) []) (mergeSubs sub2 [a])]    -- a is sub of b

             (EQ,GT) -> [Node (Entry ss (l1 <> l2) []) (mergeSubs [b] sub1)]    -- b is sub of a
             (EQ,LT) -> [Node (Entry ss (l1 <> l2) []) (mergeSubs [a] sub2)]    -- a is sub of b

             (_,_) -> if ae <= bs
                        then [Node e [a,b]]
                        else if be <= as
                          then [Node e [b,a]]
                          else -- fully nested case
                            [Node e1 (sub1++[b])] -- should merge subs
                        where
                          e = Entry ss NoChange []

        in
         trace (show ((compare as bs,compare ae be),(fs $ treeStartEnd a,l1,length sub1)
                                                   ,(fs $ treeStartEnd b,l2,length sub2)
                                                   ,(fs $ treeStartEnd (head ret))))
         ret

    redf new  old = error $ "bar2.redf:" ++ show (new,old)


-- synthesize :: s -> (t -> s -> s) -> GenericQ (s -> t) -> GenericQ t
-- synthesize z o f

-- Bottom-up synthesis of a data structure;
--  1st argument z is the initial element for the synthesis;
--  2nd argument o is for reduction of results from subterms;
--  3rd argument f updates the synthesised data according to the given term

-- synthesize :: s  -> (t -> s -> s) -> GenericQ (s -> t) -> GenericQ t
-- synthesize z o f x = f x (foldr o z (gmapQ (synthesize z o f) x))

-- foldr :: (b -> a -> a) -> a -> [b] -> a
-- foldl :: (a -> b -> a) -> a -> [b] -> a


-- | Bottom-up synthesis of a data structure;
--   1st argument z is the initial element for the synthesis;
--   2nd argument o is for reduction of results from subterms;
--   3rd argument f updates the synthesised data according to the given term
--
synthesizel :: s  -> (s -> t -> s) -> GenericQ (s -> t) -> GenericQ t
synthesizel z o f x = f x (foldl o z (gmapQ (synthesizel z o f) x))

-- ---------------------------------------------------------------------

instance Monoid Layout where
  mempty = NoChange

  mappend NoChange NoChange = NoChange
  mappend NoChange x = x
  mappend x NoChange = x
  mappend (Above bo1 ps1 pe1 eo1) (Above bo2 ps2 pe2 eo2)
    = (Above bo ps pe eo)
      where
        (bo,ps) = if ps1 <= ps2 then (bo1,ps1)
                                else (bo2,ps2)

        (eo,pe) = if pe1 >= pe2 then (eo1,pe1)
                                else (eo2,pe2)



nullSpan :: SrcSpan
nullSpan = (SrcSpan "" 0 0 0 0)

-- ---------------------------------------------------------------------
