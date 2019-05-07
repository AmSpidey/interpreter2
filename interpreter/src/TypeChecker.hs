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

checkProgram :: Program -> String
checkProgram (Prog tops) =
  trace "run check Program" $
  case runExcept (runReaderT (checkDecls ((return ())) tops) M.empty) of
    Left e -> e
    Right _ -> "went ok"

checkDecls :: S a -> [Decl] -> S a
checkDecls f (decls) = trace "starting checkDecls" $ preDecl (do
  mapM_ checkTopDef decls
  f) decls
  --mapM_ checkTopDef (d:decls)
  --local (M.union pre) (mapM_ checkTopDef t)

-- TODO: create nicer errors than just defaultErr.
-- TODO: simplify using a folding function
-- TODO: possibly reverse args. Idk.
-- TODO: change to functions having to be declared BEFORE functions that uses them. Less complicated.
preDecl :: S a -> [Decl] -> S a
preDecl g [] = g
preDecl g ((FnDef t (Ident f) args block):decls) = do
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then local ((M.insert f (typeFromArgs args (transType t)))) (preDecl g decls)
    else trace ("repeating function names") $ throwError defaultErr
preDecl g ((VarDecl t (vars)):decls) = declVars t (vars) (preDecl g decls)
--declVar t v (preDecl g ((VarDecl t (vars)):decls))

declVar :: Type -> Item -> S a -> S a
declVar t (Init (Ident v) expr) g = do
  tE <- checkExpr expr
  unifyTypes tE (transType t)
  redecl <- isInEnv (Ident v)
  if redecl
    then trace ("redeclaring var") $ throwError defaultErr
    else local (M.insert (v) (transType t)) (g)

declVars :: Type -> [Item] -> S a -> S a
declVars t ([]) f = f
declVars t (v:vars) f = declVar t v (declVars t vars f)

-- TODO: check for void arguments. Is it a bug or a feature?
typeFromArgs :: [Arg] -> Types -> Types
typeFromArgs [] t = t
typeFromArgs (ArgByVal a _:args) t = transType a :->: typeFromArgs args t
typeFromArgs (ArgByVar a _:args) t = transType a :->: typeFromArgs args t

-- TODO: check for repeating arguments in functions (SET?)
-- TODO: CHECK FOR MAIN.
checkTopDef :: Decl -> S ()
checkTopDef (FnDef t (Ident f) args block) = do
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then trace ("error in checkTopDef") $ throwError defaultErr
    else local (M.insert "" (transType t)) funcWithDecls
  where
    funcWithDecls = checkBlock (concatBlocks (declFromArgs args) block)
checkTopDef elseDef = return ()

declFromArgs :: [Arg] -> Block
declFromArgs [] = BlockStmt [Empty]
declFromArgs (ArgByVal t v:args) = concatBlocks (BlockStmt [DeclStmt(VarDecl t [Init v (defVal t)])]) (declFromArgs args)
declFromArgs (ArgByVar t v:args) = concatBlocks (BlockStmt [DeclStmt(VarDecl t [Init v (defVal t)])]) (declFromArgs args)

defVal :: Type -> Expr
defVal TStr = EString ""
defVal TInt = ELitInt 0
defVal TBool = ELitBool (BVAL "false")

checkBlock :: Block -> S ()
checkBlock (BlockStmt []) = return ()

checkBlock (BlockStmt ((DeclStmt decl):bs)) = checkDecls (checkBlock (BlockStmt bs)) [decl]

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

checkBlock (BlockStmt (Show expr:bs)) = do
  t <- checkExpr expr
  checkBlock (BlockStmt bs)

checkBlock (BlockStmt bs) = trace ("couldn't work with block: " ++ show bs) $ throwError fatalErr