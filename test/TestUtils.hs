{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module TestUtils
       ( compareFiles
       , compareStrings
       , parsedFileGhc
       -- , parseSourceFileTest
       -- , getTestDynFlags
       -- , runLogTestGhc
       -- , runTestGhc
       -- , runRefactGhcState
       -- , runRefactGhcStateLog
       -- , initialState
       -- , initialLogOnState
       -- , toksFromState
       -- , sourceTreeFromState
       -- , linesFromState
       -- , layoutFromState
       -- , entriesFromState
       -- , defaultTestSettings
       -- , logTestSettings
       -- , testSettingsMainfile
       -- , logTestSettingsMainfile
       , testCradle
       , catchException
       -- , mkTokenCache
       , hex
       , unspace
       -- , mkTestGhcName
       -- , setLogger

       , pwd
       , cd
       , bypassGHCBug7351
       ) where


-- import qualified Data.Generics.Schemes as SYB
-- import qualified Data.Generics.Aliases as SYB
-- import qualified GHC.SYB.Utils         as SYB



import GHC.Paths ( libdir )
-- import Var
-- import qualified CoreFVs               as GHC
-- import qualified CoreSyn               as GHC
import qualified DynFlags              as GHC
-- import qualified ErrUtils              as GHC
-- import qualified Exception             as GHC
-- import qualified FastString            as GHC
import qualified GHC                   as GHC
-- import qualified HscTypes              as GHC
-- import qualified Lexer                 as GHC
import qualified MonadUtils            as GHC
-- import qualified Name                  as GHC
-- import qualified Outputable            as GHC
-- import qualified SrcLoc                as GHC
-- import qualified StringBuffer          as GHC
-- import qualified Unique                as GHC

import Control.Monad
import Data.Algorithm.Diff
import Data.List
import Data.Tree
import Exception
import Language.Haskell.GhcMod

import Language.Haskell.TokenUtils.API
import Language.Haskell.TokenUtils.Types

import GhcLayout
import Numeric
import System.Directory
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- Define the type family for use with GHC

-- instance RoundTrip
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
{-
parseSourceFileTest :: FilePath -> RefactGhc (ParseResult,[PosToken])
parseSourceFileTest fileName = do
  parseSourceFileGhc fileName -- Load the file first
  p <- getTypecheckedModule
  toks <- fetchOrigToks
  return (p,toks)
-}
-- ---------------------------------------------------------------------
{-
initialState :: RefactState
initialState = RefSt
  { rsSettings = defaultTestSettings
  , rsUniqState = 1
  , rsFlags = RefFlags False
  , rsStorage = StorageNone
  , rsGraph = []
  , rsModuleGraph = []
  , rsCurrentTarget = Nothing
  , rsModule = Nothing
  }
-}
-- ---------------------------------------------------------------------
{-
initialLogOnState :: RefactState
initialLogOnState = RefSt
  { rsSettings = logTestSettings
  , rsUniqState = 1
  , rsFlags = RefFlags False
  , rsStorage = StorageNone
  , rsGraph = []
  , rsModuleGraph = []
  , rsCurrentTarget = Nothing
  , rsModule = Nothing
  }
-}
-- ---------------------------------------------------------------------
{-
toksFromState :: RefactState -> [PosToken]
toksFromState st =
  case (rsModule st) of
    Just tm -> retrieveTokensFinal $ (tkCache $ rsTokenCache tm) Map.! mainTid
    Nothing -> []
-}
-- ---------------------------------------------------------------------
{-
sourceTreeFromState :: RefactState -> Maybe SourceTree
sourceTreeFromState st =
  case (rsModule st) of
    Just tm -> Just $ layoutTreeToSourceTree $ (tkCache $ rsTokenCache tm) Map.! mainTid
    Nothing -> Nothing
-}
-- ---------------------------------------------------------------------

{-
linesFromState :: RefactState -> [Line]
linesFromState st =
  case (rsModule st) of
    Just tm -> retrieveLinesFromLayoutTree $ (tkCache $ rsTokenCache tm) Map.! mainTid
    Nothing -> []
-}
-- ---------------------------------------------------------------------
{-
layoutFromState :: RefactState -> Maybe (Tree Entry)
layoutFromState st =
  case (rsModule st) of
    Just tm -> Just ((tkCache $ rsTokenCache tm) Map.! mainTid)
    Nothing -> Nothing
-}
-- ---------------------------------------------------------------------
{-
entriesFromState :: RefactState -> [Entry]
entriesFromState st =
  case (rsModule st) of
    -- Just tm -> retrieveTokens $ (tkCache $ rsTokenCache tm) Map.! mainTid
    Just tm -> retrieveTokens' $ (tkCache $ rsTokenCache tm) Map.! mainTid
    Nothing -> []
-}
-- ---------------------------------------------------------------------

-- mkTokenCache :: Tree Entry -> TokenCache
-- mkTokenCache forest = TK (Map.fromList [((TId 0),forest)]) (TId 0)

-- ---------------------------------------------------------------------
{-
getTestDynFlags :: IO GHC.DynFlags
getTestDynFlags = do
  (df,_) <- runTestGhc $ GHC.getSessionDynFlags
  return df
-}
-- ---------------------------------------------------------------------
{-
runLogTestGhc :: RefactGhc a -> IO (a, RefactState)
runLogTestGhc comp = do
   res <- runRefactGhc comp $ initialLogOnState
   return res
-}
-- ---------------------------------------------------------------------
{-
runTestGhc :: RefactGhc a -> IO (a, RefactState)
runTestGhc comp = do
   res <- runRefactGhc comp $ initialState
   return res
-}
-- ---------------------------------------------------------------------
{-
runRefactGhcState :: RefactGhc t -> IO (t, RefactState)
runRefactGhcState paramcomp = runRefactGhcStateLog paramcomp Normal
-}
-- ---------------------------------------------------------------------
{-
runRefactGhcStateLog :: RefactGhc t -> VerboseLevel -> IO (t, RefactState)
runRefactGhcStateLog paramcomp logOn  = do
  let
     initState = RefSt
        { rsSettings = defaultTestSettings { rsetVerboseLevel = logOn }
        , rsUniqState = 1
        , rsFlags = RefFlags False
        , rsStorage = StorageNone
        , rsGraph = []
        , rsModuleGraph = []
        , rsCurrentTarget = Nothing
        , rsModule = Nothing
        }
  (r,s) <- runRefactGhc (initGhcSession testCradle (rsetImportPaths defaultTestSettings) >> 
                                                paramcomp) initState
  return (r,s)
-}
-- ---------------------------------------------------------------------

testCradle :: Cradle
testCradle = Cradle "./test/testdata/" "./test/testdata/" Nothing []

-- ---------------------------------------------------------------------
{-
defaultTestSettings :: RefactSettings
defaultTestSettings = defaultSettings { rsetImportPaths = ["./test/testdata/"]
                                      , rsetCheckTokenUtilsInvariant = True
                                      , rsetVerboseLevel = Normal }

logTestSettings :: RefactSettings
logTestSettings = defaultSettings { rsetImportPaths = ["./test/testdata/"]
                                  , rsetCheckTokenUtilsInvariant = True
                                  , rsetVerboseLevel = Debug
                                  }

testSettingsMainfile :: FilePath -> RefactSettings
testSettingsMainfile mainFile = defaultTestSettings { rsetMainFile = Just [mainFile] }

logTestSettingsMainfile :: FilePath -> RefactSettings
logTestSettingsMainfile mainFile = logTestSettings { rsetMainFile = Just [mainFile] }
-}
-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Maybe String)
catchException f = do
  res <- handle handler (f >> return Nothing)
  return res
  where
    handler:: SomeException -> IO (Maybe String)
    handler e = return (Just (show e))

-- ---------------------------------------------------------------------
{-
setLogger :: IO ()
setLogger = do
  {-
  h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
                 setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "MyApp.BuggyComponent" (addHandler h)
  -}

  -- s <- streamHandler stdout DEBUG
  h <- fileHandler "debug.log" DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [h])
-}
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
{-
mkTestGhcName :: Int -> Maybe GHC.Module -> String -> GHC.Name
mkTestGhcName u maybeMod name = n
  where
      un = GHC.mkUnique 'H' (u+1) -- H for HaRe :)
      n = case maybeMod of
               Nothing -> GHC.localiseName $ GHC.mkSystemName un (GHC.mkVarOcc name)
               Just modu -> GHC.mkExternalName un modu (GHC.mkVarOcc name) nullSrcSpan
-}
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


-- EOF

