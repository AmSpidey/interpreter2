module TypeChecker where

import AbsOstaczGr
import ErrM
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe(fromMaybe)
import Control.Monad.Except

data Types = TI | TB | TS | Types :->: Types deriving (Eq, Show)

type TypeError = String
defaultErr = "error in typeChecker"

type TypeEnv = M.Map String Types
type S a = ReaderT TypeEnv (Except TypeError) a

--checkTopDef :: TopDef -> S Bool
--checkTopDef (FnDef (Type t) (Ident f) args block) = do


checkBlock :: Block -> S ()
checkBlock (BlockStmt (PreDecl t (Ident v):bs)) = local (M.insert v (transType t)) (checkBlock (BlockStmt bs))
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

{--insertDecl :: Stmt -> TypeEnv -> TypeEnv
insertDecl (Decl t ([])) env = env
insertDecl (Decl t (NoInit (Ident v):vars)) env = do
  M.insert v (transType t) env
insertDecl (Decl t (Init (Ident v):vars)) env = do
  M.insert v (transType t) env-}

unifyTypes :: Types -> Types -> Types -> S ()
unifyTypes t1 t2 t = Control.Monad.when (t1 /= t2 || t1 == t) $ throwError defaultErr

transType :: Type -> Types
transType TInt = TI
transType TBool = TB
transType TStr = TS

checkExpr :: Expr -> S Types
checkExpr (EVar (Ident x)) = do
  env <- ask
  let mt = M.lookup x env
  maybe (throwError defaultErr) return mt
checkExpr (ELitInt _) = return TI
checkExpr (ELitBool _) = return TB
checkExpr (EString _) = return TS
checkExpr (Not b) = do
  t <- checkExpr b
  case t of
    TB -> return TB
    _ -> throwError defaultErr
checkExpr (Neg x) = do
  t <- checkExpr x
  case t of
    TI -> return TI
    _ -> throwError defaultErr
checkExpr (EMul expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  if t1 /= TI || t2 /= TI
    then throwError defaultErr
    else return TI
checkExpr (EAdd expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  if t1 /= TI || t2 /= TI
    then throwError defaultErr
    else return TI
checkExpr (ERel expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  if t1 /= TI || t2 /= TI
    then throwError defaultErr
    else return TB
checkExpr (EAnd expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  if t1 /= TB || t2 /= TB
    then throwError defaultErr
    else return TB
checkExpr (EOr expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  if t1 /= TB || t2 /= TB
    then throwError defaultErr
    else return TB