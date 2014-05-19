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
       , mkTokenCache
       , hex
       , unspace
       -- , mkTestGhcName
       -- , setLogger

       , pwd
       , cd
       , bypassGHCBug7351
       ) where


import qualified GHC           as GHC
import qualified Name          as GHC
import qualified Unique        as GHC

import Data.Algorithm.Diff
import Exception
import Language.Haskell.GhcMod
import Language.Haskell.TokenUtils.Types
-- import Language.Haskell.Refact.Utils.DualTree
-- import Language.Haskell.Refact.Utils.LocUtils
-- import Language.Haskell.Refact.Utils.Monad
-- import Language.Haskell.Refact.Utils.MonadFunctions
-- import Language.Haskell.Refact.Utils.TokenUtils
-- import Language.Haskell.Refact.Utils.TokenUtilsTypes
-- import Language.Haskell.Refact.Utils.TypeSyn
import Numeric

import Data.Tree
import System.Directory
-- import System.Log.Handler.Simple
-- import System.Log.Logger
import qualified Data.Map as Map


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
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule
-- type PosToken = (GHC.Located GHC.Token, String)

parsedFileGhc :: String -> IO (ParseResult,[PosToken])
parsedFileGhc fileName = do
  assert False undefined
{-
  let
    comp = do
       res <- parseSourceFileTest fileName
       return res
  (parseResult,_s) <- runRefactGhcState comp
  return parseResult
-}
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

mkTokenCache :: Tree Entry -> TokenCache
mkTokenCache forest = TK (Map.fromList [((TId 0),forest)]) (TId 0)

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
bypassGHCBug7351 ts = map go ts
  where
   go :: (GHC.Located GHC.Token, String) -> (GHC.Located GHC.Token, String)
   go rt@(GHC.L (GHC.UnhelpfulSpan _) _t,_s) = rt
   go    (GHC.L (GHC.RealSrcSpan l) t,s) = (GHC.L (fixCol l) t,s)

   fixCol l = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.srcSpanFile l) (GHC.srcSpanStartLine l) ((GHC.srcSpanStartCol l) - 1))
                            (GHC.mkSrcLoc (GHC.srcSpanFile l) (GHC.srcSpanEndLine l) ((GHC.srcSpanEndCol l) - 1))


-- EOF

