module Main where

import Paths_de3lang (version)
import Data.Version(Version(..), showVersion)
import System.IO(hPutStrLn, stdin, stdout, stderr)
import System.Exit(exitSuccess)
import System.Environment(getArgs)
import System.Console.GetOpt
import EAFIT.De3Lang

data Options =  Options { optShowVersion :: Bool
                        , optShowHelp    :: Bool
                        , optGrammar     :: FilePath
                        , optDerive      :: FilePath
                        , optTree        :: FilePath
                        , optResumen     :: Bool
                        } deriving Show

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optShowHelp    = False
                         , optGrammar     = ""
                         , optDerive      = ""
                         , optTree        = ""
                         , optResumen     = False
                         }

toGrammar :: String -> Options -> Options
toGrammar s opts = opts { optGrammar = s }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["version"]
      (NoArg (\opts -> opts { optShowVersion  = True }))
      "shows a version number and quits "
    , Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts { optShowHelp = True }))
      "shows help menu and quits"
    , Option ['g'] ["grammar"]
      (ReqArg (\s opts -> opts { optGrammar = s }) "FILE")
      "parser a grammar"
    , Option ['d'] ["derivation"]
      (ReqArg (\s opts -> opts { optDerive = s }) "FILE")
      "parser a derivation"
    , Option ['t'] ["tree"]
      (ReqArg (\s opts -> opts { optTree = s }) "FILE")
      "parser a tree"
    , Option ['r'] ["resumen"]
      (NoArg (\opts -> opts { optResumen = True }))
      "resumen options"
    ]

compilerOpts :: [String] -> IO Options
compilerOpts argv =
  case getOpt Permute options argv of
    (o,_,[]) -> return (foldl (flip id) defaultOptions o)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: de3lang [OPTION...]"

processStaticOptions :: Options -> IO ()
processStaticOptions opts 
   | optShowVersion opts    = do hPutStrLn stdout $ "de3lang version: " ++ (showVersion version)
                                 exitSuccess
   | optShowHelp opts       = showHelp
   | optResumen  opts       = do putStrLn $ "grammar=" ++ optGrammar opts
                                 putStrLn $ "derivation=" ++ optDerive opts
                                 putStrLn $ "tree=" ++ optTree opts 
   | otherwise              = return ()

showHelp :: IO ()
showHelp = do
  mapM_ (hPutStrLn stderr) (lines $ usageInfo header options)
  exitSuccess
  where header = "Usage: de3lang [OPTION...]"
                 
processGrammar :: Options -> IO ()
processGrammar opts = case optGrammar opts of
                        "" -> return ()
                        f  -> do ei <- pCFGFile f
                                 case ei of
                                   Left er   -> hPutStrLn stderr (show er)
                                   Right cfg -> hPutStrLn stdout (show cfg)

processDerivation :: Options -> IO ()
processDerivation opts = case optDerive opts of
                           "" -> return () 
                           f  -> do ei <- pDrvFile f
                                    case ei of
                                      Left er  -> hPutStrLn stderr (show er)
                                      Right dr -> hPutStrLn stdout (show dr)

processTree :: Options -> IO ()
processTree opts = case optTree opts of
                     "" -> return ()
                     f  -> do ei <- pTreeFile f
                              case ei of
                                Left er  -> hPutStrLn stderr (show er)
                                Right tr -> hPutStrLn stdout (show tr)
                                            
main :: IO ()
main = do
  hPutStrLn stdout "Staring De3Lang"
  opts <- (getArgs >>= compilerOpts)
  processStaticOptions opts
  processGrammar opts
  processDerivation opts
  processTree opts
