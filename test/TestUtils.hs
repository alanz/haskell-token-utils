{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module TestUtils
       ( compareFiles
       , compareStrings
       , parsedFileGhc
       , testCradle
       , catchException
       , mkHseSrcSpan
       , realSrcLocFromTok
       , hex
       , unspace
       , PosToken
       , pwd
       , cd
       , bypassGHCBug7351
       , mkTestGhcName
       ) where


import GHC.Paths ( libdir )
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified GHC.Paths     as GHC
import qualified Lexer         as GHC
import qualified Name          as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified Unique        as GHC

import Control.Monad
import Data.Algorithm.Diff
import Data.List
import Data.Tree
import Exception
import Language.Haskell.GhcMod

import Language.Haskell.TokenUtils.API
import Language.Haskell.TokenUtils.Types
import Language.Haskell.TokenUtils.GHC.Layout
import Language.Haskell.TokenUtils.Utils

import qualified Language.Haskell.Exts.Annotated as HSE

import Numeric
import System.Directory
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

type PosToken = (GHC.Located GHC.Token, String)

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------

hex :: Int -> String
hex v = "0x" ++ showHex v ""

-- ---------------------------------------------------------------------

compareFiles :: FilePath -> FilePath -> IO [Diff [String]]
compareFiles fileA fileB = do
  astr <- readFile fileA
  bstr <- readFile fileB
  -- return $ filter (\c -> not( isBoth c)) $ getGroupedDiff (lines astr) (lines bstr)
  return $ compareStrings astr bstr

compareStrings :: String -> String -> [Diff [String]]
compareStrings astr bstr = filter (\c -> not( isBoth c)) $ getGroupedDiff (lines astr) (lines bstr)
    where
      isBoth (Both _ _) = True
      isBoth _        = False

-- ---------------------------------------------------------------------

testCradle :: Cradle
testCradle = Cradle "./test/testdata/" "./test/testdata/" Nothing []

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Maybe String)
catchException f = do
  res <- handle handler (f >> return Nothing)
  return res
  where
    handler:: SomeException -> IO (Maybe String)
    handler e = return (Just (show e))

-- ---------------------------------------------------------------------

-- |Convert any sequence of more than one space to a single space
unspace :: String -> String
unspace str = go [] str
  where
    go acc []  = acc
    go acc [x] = acc ++ [x]
    go acc (' ':' ':xs) = go acc (' ':xs)
    go acc (x:xs) = go (acc++[x]) xs

-- ---------------------------------------------------------------------

-- http://hackage.haskell.org/trac/ghc/ticket/7351
-- bypassGHCBug7351 :: [PosToken] -> [PosToken]
bypassGHCBug7351 :: [(GHC.Located GHC.Token, String)] -> [(GHC.Located GHC.Token, String)]
bypassGHCBug7351 ts = map go ts
  where
   go :: (GHC.Located GHC.Token, String) -> (GHC.Located GHC.Token, String)
   go rt@(GHC.L (GHC.UnhelpfulSpan _) _t,_s) = rt
   go    (GHC.L (GHC.RealSrcSpan l) t,s) = (GHC.L (fixCol l) t,s)

   fixCol l = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.srcSpanFile l) (GHC.srcSpanStartLine l) ((GHC.srcSpanStartCol l) - 1))
                            (GHC.mkSrcLoc (GHC.srcSpanFile l) (GHC.srcSpanEndLine l) ((GHC.srcSpanEndCol l) - 1))


-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule
-- type PosToken = (GHC.Located GHC.Token, String)

parsedFileGhc :: String -> IO (ParseResult,[(GHC.Located GHC.Token, String)])
parsedFileGhc fileName = do
#if __GLASGOW_HASKELL__ > 704
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
    GHC.defaultErrorHandler GHC.defaultLogAction $ do
#endif
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = foldl GHC.xopt_set dflags
                           [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

            dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }

            dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                                   GHC.ghcLink =  GHC.LinkInMemory }

        void $ GHC.setSessionDynFlags dflags'''
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        -- GHC.liftIO $ putStrLn $ "targets set"
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        g <- GHC.getModuleGraph
        let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))
        -- modSum <- GHC.getModSummary $ GHC.mkModuleName "BCpp"
        let modSum = head g
        p <- GHC.parseModule modSum
        t <- GHC.typecheckModule p
        -- GHC.liftIO $ putStrLn $ "parsed"
        toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        return (t,toks)

-- ---------------------------------------------------------------------

-- | gets the (row,col) of the start of the @GHC.SrcSpan@, or (-1,-1)
-- if there is an @GHC.UnhelpfulSpan@
getGhcLoc :: GHC.SrcSpan -> (Int, Int)
getGhcLoc (GHC.RealSrcSpan ss)  = (GHC.srcSpanStartLine ss, GHC.srcSpanStartCol ss)
getGhcLoc (GHC.UnhelpfulSpan _) = (-1,-1)

-- | gets the (row,col) of the end of the @GHC.SrcSpan@, or (-1,-1)
-- if there is an @GHC.UnhelpfulSpan@
getGhcLocEnd :: GHC.SrcSpan -> (Int, Int)
getGhcLocEnd (GHC.RealSrcSpan ss)  = (GHC.srcSpanEndLine ss, GHC.srcSpanEndCol ss)
getGhcLocEnd (GHC.UnhelpfulSpan _) = (-1,-1)

getLocatedStart :: GHC.GenLocated GHC.SrcSpan t -> (Int, Int)
getLocatedStart (GHC.L l _) = getGhcLoc l

getLocatedEnd :: GHC.GenLocated GHC.SrcSpan t -> (Int, Int)
getLocatedEnd (GHC.L l _) = getGhcLocEnd l

-- ---------------------------------------------------------------------

mkHseSrcSpan :: SimpPos -> SimpPos -> HSE.SrcSpan
mkHseSrcSpan (sr,sc) (er,ec) = (HSE.mkSrcSpan (HSE.SrcLoc "filename" sr sc) (HSE.SrcLoc "filename" er ec))

-- ---------------------------------------------------------------------

lexStringToRichTokens :: GHC.RealSrcLoc -> String -> IO [PosToken]
lexStringToRichTokens startLoc str = do
  -- error $ "lexStringToRichTokens: (startLoc,str)=" ++ (showGhc (startLoc,str)) -- ++AZ
#if __GLASGOW_HASKELL__ > 704
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
  GHC.defaultErrorHandler GHC.defaultLogAction $ do
#endif
    GHC.runGhc (Just GHC.libdir) $ do
      dflags <- GHC.getSessionDynFlags
      let dflags' = foldl GHC.xopt_set dflags
                    [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]
      _ <- GHC.setSessionDynFlags dflags'

      -- lexTokenStream :: StringBuffer -> RealSrcLoc -> DynFlags -> ParseResult [Located Token]
      let res = GHC.lexTokenStream (GHC.stringToStringBuffer str) startLoc dflags'
      case res of
        GHC.POk _ toks -> return $ GHC.addSourceToTokens startLoc (GHC.stringToStringBuffer str) toks 
        GHC.PFailed _srcSpan _msg -> error $ "lexStringToRichTokens:" -- ++ (show $ GHC.ppr msg)

        -- addSourceToTokens :: RealSrcLoc -> StringBuffer -> [Located Token] -> [(Located Token, String)]

-- ---------------------------------------------------------------------

realSrcLocFromTok :: PosToken -> GHC.RealSrcLoc
realSrcLocFromTok (GHC.L (GHC.RealSrcSpan srcspan) _,_) = GHC.realSrcSpanStart srcspan
realSrcLocFromTok (GHC.L _ _,_) = GHC.mkRealSrcLoc (GHC.mkFastString "") 1 1


-- ---------------------------------------------------------------------

mkTestGhcName :: Int -> Maybe GHC.Module -> String -> GHC.Name
mkTestGhcName u maybeMod name = n
  where
      un = GHC.mkUnique 'H' (u+1) -- H for HaRe :)
      n = case maybeMod of
               Nothing -> GHC.localiseName $ GHC.mkSystemName un (GHC.mkVarOcc name)
               Just modu -> GHC.mkExternalName un modu (GHC.mkVarOcc name) nullSrcSpan

-- EOF

