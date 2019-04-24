module TypeChecker where

import Simplified
import AbsOstaczGr
import TypeCheckExpr

import qualified Data.Map as M
import ErrM
import Control.Monad.Except
import Data.Maybe(fromMaybe, isNothing, fromJust)
import Control.Monad(when)
import Control.Monad.Reader
import Control.Monad.State

-- TODO: change into a function of another type, f.e returning bool,
-- catch exception
checkProgram :: Program -> S()
checkProgram (Prog tops) = checkTopDefinitions tops

checkTopDefinitions :: [TopDef] -> S()
checkTopDefinitions t = do
  pre <- preDeclFunctions t M.empty
  local (M.union pre) (mapM_ checkTopDef t)

-- TODO: simplify using a folding function
preDeclFunctions :: [TopDef] -> TypeEnv -> S TypeEnv
preDeclFunctions [] env = return env
preDeclFunctions (FnDef t (Ident f) args block:funcs) env = do
  let mt = M.lookup f env
  if isNothing mt
    then preDeclFunctions funcs (M.insert f (typeFromArgs args (transType t)) env)
    else throwError defaultErr

typeFromArgs :: [Arg] -> Types -> Types
typeFromArgs [] t = t
typeFromArgs (ArgByVal a _:args) t = transType a :->: typeFromArgs args t
typeFromArgs (ArgByVar a _:args) t = transType a :->: typeFromArgs args t

checkTopDef :: TopDef -> S ()
checkTopDef (FnDef t (Ident f) args block) = do
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then local (M.insert "" (transType t)) funcWithDecls
    else throwError defaultErr
  where
    funcWithDecls = checkBlock (concatBlocks (declFromArgs args) block)

declFromArgs :: [Arg] -> Block
declFromArgs (ArgByVal a x:args) = BlockStmt [PreDecl a x]
declFromArgs (ArgByVar a x:args) = BlockStmt [PreDecl a x]

checkBlock :: Block -> S ()
checkBlock (BlockStmt (PreDecl t (Ident v):bs)) = do
  redecl <- isInEnv (Ident v)
  if redecl
    then throwError defaultErr
    else local (M.insert v (transType t)) (checkBlock (BlockStmt bs))

checkBlock (BlockStmt (Empty:bs)) = checkBlock (BlockStmt bs)

checkBlock (BlockStmt (BStmt b:bs)) = do
  checkBlock b
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (Ass (Ident x) e:bs)) = do
  t1 <- checkExpr e
  t2 <- checkExpr (EVar (Ident x))
  unifyTypes t1 t2

checkBlock (BlockStmt (Incr (Ident x):bs)) = do
  t <- checkExpr (EVar (Ident x))
  unifyTypes t TI

checkBlock (BlockStmt (Decr (Ident x):bs)) = do
  t <- checkExpr (EVar (Ident x))
  unifyTypes t TI

checkBlock (BlockStmt (Ret e:bs)) = do
  t <- checkExpr e
  env <- ask
  let mt = M.lookup "" env
  when (isNothing mt) $ throwError defaultErr
  let rt = fromJust mt
  unifyTypes rt t
  checkBlock (BlockStmt bs)
  
checkBlock (BlockStmt (VRet:bs)) = do
  env <- ask
  let mt = M.lookup "" env
  when (isNothing mt) $ throwError defaultErr
  let rt = fromJust mt
  unifyTypes rt TV
  checkBlock (BlockStmt bs)