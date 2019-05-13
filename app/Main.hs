module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import AbsOstaczGr
import Control.Monad (when)
import Interpreter
import LexOstaczGr
import ParOstaczGr
import PrintOstaczGr
import TypeChecker

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ([Token] -> Err Program) -> String -> IO b
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ([Token] -> Err Program) -> String -> IO b
run v p s =
  let ts = myLLexer s
   in case p ts of
        Bad err -> do
          putStrLn "\nParse              Failed...\n"
          putStrV v "Tokens:"
          putStrLn err
          exitFailure
        Ok tree -> do
          case checkProgram tree of
            Left e -> do
              putStrLn ("TypeChecker error. Following error first occured: " ++ e)
              exitFailure
            Right _ -> return ()
          res <- afterEval tree
          case res of
            Left e -> do
              putStrLn ("RUNTIME ERROR! " ++ e)
              exitFailure
            Right val -> putStrLn ("\n" ++ "Program returned: " ++ show val)
          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:"
      , "  --help          Display this help message."
      , "  (no arguments)  Parse stdin verbosely."
      , "  (files)         Parse content of files verbosely."
      , "  -s (files)      Silent mode. Parse content of files silently."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs