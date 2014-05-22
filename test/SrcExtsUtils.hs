module SrcExtsUtils
  (
  loadFile
  ) where

import Control.Exception

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Lexer

-- parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])

-- Parse a source file from a string using a custom parse mode and retaining comments. 


-- loadFile :: FilePath -> 
loadFile :: FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]), ParseResult [Loc Token])
loadFile fileName = do
  src <- readFile fileName
  let toks = lexTokenStream src
  let ast = parseFileContentsWithComments defaultParseMode src
  return (ast,toks)

-- parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
-- defaultParseMode :: ParseMode

t = loadFile "../test/testdata/Layout/LetExpr.hs"
