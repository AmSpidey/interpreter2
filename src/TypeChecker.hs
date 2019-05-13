module TypeChecker where

import AbsOstaczGr
import TypeCheckExpr

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, empty, insert, member)

concatBlocks :: Block -> Block -> Block
concatBlocks (BlockStmt stmts1) (BlockStmt stmts2) = BlockStmt (stmts1 ++ stmts2)

checkProgram :: Program -> Either String ()
checkProgram (Prog tops) = runExcept (runReaderT (preDecl (checkForMain tops) tops) M.empty)

checkForMain :: [Decl] -> S ()
checkForMain [] = throwError noMain
checkForMain (FnDef t (Ident f) args _:decls) = unless (f == "main" && args == [] && t == TInt) $ checkForMain decls
checkForMain (_:decls) = checkForMain decls

preDecl :: S a -> [Decl] -> S a
preDecl g [] = g
preDecl g (d@(FnDef t (Ident f) args _):decls) = do
  findRepeating args Data.Set.empty
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then local
           (M.insert f (typeFromArgs args (transType t)))
           (do checkTopDef d
               preDecl g decls)
    else throwError repNames
preDecl g (VarDecl t vars:decls) = declVars t vars (preDecl g decls)

declVar :: Type -> Item -> S a -> S a
declVar t (Init (Ident v) expr) g = do
  tE <- checkExpr expr
  unifyTypes tE (transType t)
  local (M.insert v (transType t)) g

declVars :: Type -> [Item] -> S a -> S a
declVars t vars f = foldr (declVar t) f vars

typeFromArgs :: [Arg] -> Types -> Types
typeFromArgs [] t = t
typeFromArgs (ArgByVal a _:args) t = (ByVal, transType a) :->: typeFromArgs args t
typeFromArgs (ArgByVar a _:args) t = (ByVar, transType a) :->: typeFromArgs args t

findRepeating :: [Arg] -> Set Ident -> S ()
findRepeating [] _ = return ()
findRepeating (ArgByVal _ i:args) s = do
  when (Data.Set.member i s) $ throwError repArgs
  let newSet = Data.Set.insert i s
  findRepeating args newSet
findRepeating (ArgByVar _ i:args) s = do
  when (Data.Set.member i s) $ throwError repArgs
  let newSet = Data.Set.insert i s
  findRepeating args newSet

-- TODO: check for repeating arguments in functions (SET?)
checkTopDef :: Decl -> S ()
checkTopDef (FnDef t (Ident _) args block) = local (M.insert "" (transType t)) funcWithDecls
  where
    funcWithDecls = checkBlock (concatBlocks (declFromArgs args) block)
checkTopDef _ = error "variable declaration passed to checkTopDef"

declFromArgs :: [Arg] -> Block
declFromArgs [] = BlockStmt [Empty]
declFromArgs (ArgByVal t v:args) =
  concatBlocks (BlockStmt [DeclStmt (VarDecl t [Init v (defVal t)])]) (declFromArgs args)
declFromArgs (ArgByVar t v:args) =
  concatBlocks (BlockStmt [DeclStmt (VarDecl t [Init v (defVal t)])]) (declFromArgs args)

defVal :: Type -> Expr
defVal TStr = EString ""
defVal TInt = ELitInt 0
defVal TBool = ELitBool (BVAL "false")
defVal TVoid = error "void passed to defVal"

checkBlock :: Block -> S ()
checkBlock (BlockStmt []) = return ()
checkBlock (BlockStmt (DeclStmt decl:bs)) = preDecl (checkBlock (BlockStmt bs)) [decl]
checkBlock (BlockStmt (Empty:bs)) = checkBlock (BlockStmt bs)
checkBlock (BlockStmt (BStmt b:bs)) = do
  checkBlock b
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Ass (Ident x) e:bs)) = do
  t1 <- checkExpr e
  t2 <- checkExpr (EVar (Ident x))
  unifyTypes t1 t2
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Incr (Ident x):bs)) = do
  t <- checkExpr (EVar (Ident x))
  unifyTypes t TI
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Decr (Ident x):bs)) = do
  t <- checkExpr (EVar (Ident x))
  unifyTypes t TI
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Ret e:bs)) = do
  t <- checkExpr e
  env <- ask
  let mt = M.lookup "" env
  when (isNothing mt) $ error "no return environment inside a block"
  let rt = fromJust mt
  unifyTypes rt t
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt (VRet:bs)) = do
  env <- ask
  let mt = M.lookup "" env
  when (isNothing mt) $ error "no return environment inside a block"
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
checkBlock (BlockStmt (Subsection _ _:bs)) = checkBlock (BlockStmt bs)
checkBlock (BlockStmt (SubsectionPaid _ expr _:bs)) = do
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
  _ <- checkExpr expr
  checkBlock (BlockStmt bs)
checkBlock (BlockStmt (Show expr:bs)) = do
  t <- checkExpr expr
  when (t /= TI && t /= TS && t /= TB) $ throwError wrongShow
  checkBlock (BlockStmt bs)