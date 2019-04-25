module TypeChecker where

import Simplified
import AbsOstaczGr
import TypeCheckExpr
import Debug.Trace

import qualified Data.Map as M
import ErrM
import Control.Monad.Except
import Data.Maybe(fromMaybe, isNothing, fromJust)
import Control.Monad(when)
import Control.Monad.Reader
import Control.Monad.State

-- TODO: change into a function of another type, f.e returning bool, catch exception
-- TODO: check for no return statement in the function
--handler :: S()
--handler =

checkProgram :: Program -> String
checkProgram (Prog tops) =
  trace "run check Program" $
  case runExcept (runReaderT (checkTopDefinitions tops) M.empty) of
    Left e -> e
    Right _ -> "went ok"

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
    else trace ("error in preDeclFunctions") $ throwError defaultErr

typeFromArgs :: [Arg] -> Types -> Types
typeFromArgs [] t = t
typeFromArgs (ArgByVal a _:args) t = transType a :->: typeFromArgs args t
typeFromArgs (ArgByVar a _:args) t = transType a :->: typeFromArgs args t

checkTopDef :: TopDef -> S ()
checkTopDef (FnDef t (Ident f) args block) = do
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then trace ("error in checkTopDef") $ throwError defaultErr
    else local (M.insert "" (transType t)) funcWithDecls
  where
    funcWithDecls = checkBlock (concatBlocks (declFromArgs args) block)

declFromArgs :: [Arg] -> Block
declFromArgs [] = BlockStmt [Empty]
declFromArgs (ArgByVal a x:args) = concatBlocks (BlockStmt [PreDecl a x]) (declFromArgs args)
declFromArgs (ArgByVar a x:args) = concatBlocks (BlockStmt [PreDecl a x]) (declFromArgs args)

-- TODO: think of deleting sections that do nothing. Or maybe don't, because easier to maintain?

checkBlock :: Block -> S ()
checkBlock (BlockStmt []) = return ()
checkBlock (BlockStmt (PreDecl t (Ident v):bs)) = do
  redecl <- isInEnv (Ident v)
  if redecl
    then trace ("error in checkBlock") $ throwError defaultErr
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
  when (isNothing mt) $ trace ("error in block: " ++ show bs) $ throwError defaultErr
  let rt = fromJust mt
  unifyTypes rt t
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (VRet:bs)) = do
  env <- ask
  let mt = M.lookup "" env
  when (isNothing mt) $ trace ("error in block: " ++ show bs) $ throwError defaultErr
  let rt = fromJust mt
  unifyTypes rt TV
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (Cond e stmt:bs)) = do
  t <- checkExpr e
  unifyTypes t TB
  checkBlock (BlockStmt [stmt])
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (CondElse e stmt1 stmt2:bs)) = do
  t <- checkExpr e
  unifyTypes t TB
  checkBlock (BlockStmt [stmt1])
  checkBlock (BlockStmt [stmt2])
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (Subsection sect b:bs)) = checkBlock (BlockStmt bs)

checkBlock (BlockStmt (SubsectionPaid sect expr b:bs)) = do
  t <- checkExpr expr
  unifyTypes t TI
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (Repeat:bs)) = checkBlock (BlockStmt bs)

checkBlock (BlockStmt (Finish:bs)) = checkBlock (BlockStmt bs)

checkBlock (BlockStmt (Earn expr:bs)) = do
  t <- checkExpr expr
  unifyTypes t TI
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (RepeatXTimes expr:bs)) = do
  t <- checkExpr expr
  unifyTypes t TI
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt (SExp expr:bs)) = do
  t <- checkExpr expr
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt bs) = trace ("couldn't work with block: " ++ show bs) $ throwError fatalErr