module TypeChecker where

import Simplified
import TypeCheckExpr
import Data.Maybe(fromMaybe, isNothing)

isInEnv :: Ident -> S Bool
isInEnv (Ident x) = do
  env <- ask
  let mt = M.lookup x env
  if isNothing mt
    then return False
    else return True

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

checkTopDef :: TopDef -> S ()
checkTopDef (FnDef t (Ident f) args block) = do
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then local (M.insert f (typeFromArgs args (transType t))) funcWithDecls
    else throwError defaultErr
  where
    funcWithDecls = checkBlock (concatBlocks (declFromArgs args) block)

typeFromArgs :: [Arg] -> Types -> Types
typeFromArgs [] t = t
typeFromArgs (ArgByVal a _:args) t = transType a :->: typeFromArgs args t
typeFromArgs (ArgByVar a _:args) t = transType a :->: typeFromArgs args t

declFromArgs :: [Arg] -> Block
declFromArgs (ArgByVal a x:args) = BlockStmt [PreDecl a x]
declFromArgs (ArgByVar a x:args) = BlockStmt [PreDecl a x]

checkBlock :: Block -> S ()
checkBlock (BlockStmt (PreDecl t (Ident v):bs)) = do
  redecl <- isInEnv (Ident v)
  if redecl
    then throwError defaultErr
    else local (M.insert v (transType t)) (checkBlock (BlockStmt bs))
checkBlock (BlockStmt (Ass (Ident x) e:bs)) = do
  t1 <- checkExpr e
  t2 <- checkExpr (EVar (Ident x))
  if t1 /= t2
    then throwError defaultErr
    else checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Incr (Ident x):bs)) = do
  t <- checkExpr (EVar (Ident x))
  if t /= TI
    then throwError defaultErr
    else checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Decr (Ident x):bs)) = do
  t <- checkExpr (EVar (Ident x))
  if t /= TI
    then throwError defaultErr
    else checkBlock (BlockStmt bs)