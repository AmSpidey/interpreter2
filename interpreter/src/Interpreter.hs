module Interpreter where

-- Haskell module generated by the BNF converter
-- Parts dedicated to store, memory heavily inspired by code
-- for Tiny that was published by our teachers

import AbsOstaczGr
import ErrM
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe, fromJust)
import Control.Monad.Cont
import Control.Monad.Except

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

-- TODO: pass by variable
data Value = ValInt Integer | ValS String | ValB Bool | ValV | Func [ValueArg] Env Block deriving (Ord, Eq, Show)

data ValueArg = ByVar Ident | ByVal Ident deriving (Ord, Eq, Show)

data Passed = Val Value | Var Loc

getBool :: Value -> Bool
getBool (ValB v) = v

type Loc = Int

-- environment
type Env = M.Map String Loc

-- state and next free location
type Mem = M.Map Loc Value
type Store = (Mem, Loc)

-- TODO: change into something with exceptions. + how to store functions?
type SS a = StateT Store (ReaderT Env (Except String)) a

-- reserving new location
newloc :: SS Loc
newloc = do
  (st,l) <- get
  put (st,l+1)
  return l

modifyMem :: (Mem -> Mem) -> SS ()
modifyMem f =
  modify (\(st,l) -> (f st,l))

inLocal :: Env -> SS a -> SS a
inLocal locEnv f = local (const locEnv) f

decVar :: Ident -> SS a -> SS a
decVar (Ident v) g = do
  l <- newloc
  local (M.insert v l) g

setVar :: Ident -> Value -> SS a -> SS a
setVar (Ident v) val g = do
  env <- ask
  let l = fromJust (M.lookup v env)
  (st, ml) <- get
  put (M.insert l val st, ml)
  g

instance Num Value where
  abs (ValInt a) = ValInt (abs a)
  signum (ValInt a) = ValInt (signum a)
  fromInteger = ValInt
  ValInt a + ValInt b = ValInt $ a + b
  ValInt a - ValInt b = ValInt $ a - b
  ValInt a * ValInt b = ValInt $ a * b
instance Real Value where
  toRational (ValInt a) = toRational a
instance Enum Value where
  toEnum a = ValInt (toEnum a)
  fromEnum (ValInt a) = fromEnum a
instance Integral Value where
  toInteger (ValInt a) = a
  rem (ValInt a) (ValInt b)= ValInt $ rem a b
  mod (ValInt a) (ValInt b) = ValInt $ mod a b
  quot (ValInt a) (ValInt b) = ValInt $ quot a b

type Interpretation a = Maybe a

-- TODO: if you reach end of a Block with no return statement, return Exception

{--evalStmt :: Stmt -> SS (Maybe Value)
evalStmt Empty = return Nothing

evalStmt (BStmt b) = local (id) (evalBlock b)

evalStmt (PreDecl t v) =-}

evalBlock :: Block -> SS (Maybe Value)
evalBlock b = return $ Just (ValInt 2)

evalBlock (BlockStmt (Empty:stmts)) = evalBlock (BlockStmt stmts)

evalBlock (BlockStmt (BStmt b:stms)) = local (id) (evalBlock b)

evalBlock (BlockStmt (((PreDecl _ v)):stmts)) = decVar v (evalBlock (BlockStmt stmts))

evalBlock (BlockStmt (((Ass v e)):stmts)) = do
  val <- interpretExpr e
  setVar v val (evalBlock (BlockStmt stmts))

evalBlock (BlockStmt (((Ass v e)):stmts)) = do
  val <- interpretExpr e
  setVar v val (evalBlock (BlockStmt stmts))

-- TODO: in typechecker check if the expression is variable!
-- TODO: remember to call in a local env around the function
evalFunc :: Value -> [Passed] -> SS (Value)
evalFunc (Func [] env block) [] = do
  ret <- evalBlock block
  case ret of
    Nothing -> throwError ("no return statement in the end of function")
    Just r -> return r

evalFunc (Func ((ByVal v):args) env block ) ((Val p):expr) = decVar v $ setVar v p $ evalFunc (Func args env block) expr

evalFunc (Func ((ByVar (Ident v)):args) env block ) ((Var l):expr) = local (M.insert v l) (evalFunc (Func args env block) expr)

eval _ _ = error ("non-matching argument to function. Possible fault with TypeChecker")

-- TODO: make it work with Either
-- evalExpr :: Expr -> Value
--evalExpr expr = runExcept (runReaderT (evalStateT (interpretExpr expr) (M.empty, 0)) M.empty)

preArgs :: [Expr] -> [ValueArg] -> SS [Passed]
preArgs [] [] = return []

preArgs ((EVar (Ident v)):e) ((ByVar x):args) = do
  env <- ask
  let l = fromJust (M.lookup v env)
  res <- preArgs e args
  return( ([Var l])  ++ res)

preArgs (e:expr) ((ByVar x):args) = do
  val <- interpretExpr e
  res <- preArgs expr args
  return( ([Val val])  ++ res)

interpretExpr :: Expr -> SS Value
interpretExpr (EVar (Ident var)) = do
  env <- ask
  let l = fromMaybe (error "undefined variable") (M.lookup var env)
  (st,_)  <- get
  return $ fromMaybe (error "undefined location") (M.lookup l st)

interpretExpr (ELitInt integer) = return $ ValInt integer

interpretExpr (ELitBool (BVAL "true")) = return $ ValB True

interpretExpr (EString string) = return $ ValS string

interpretExpr (EAdd expr1 op expr2)  = do
  v1 <- interpretExpr expr1
  v2 <- interpretExpr expr2
  case op of
    Plus -> return $ v1 + v2
    Minus -> return $ v1 - v2

interpretExpr (EMul expr1 op expr2) = do
 v1 <- interpretExpr expr1
 v2 <- interpretExpr expr2
 case op of
   Times -> return $ v1 * v2
   Div -> return $ quot v1 v2
   Mod -> return $ mod v1 v2

interpretExpr (Neg expr) = do
  v <- interpretExpr expr
  return ((-1) * v)

interpretExpr (Not expr) = do
  v <- getBool <$> interpretExpr expr
  return $ ValB (not v)

interpretExpr (ERel expr1 relop expr2) = do
  v1 <- interpretExpr expr1
  v2 <- interpretExpr expr2
  case relop of
    LTH -> return $ ValB $ v1 < v2
    LE -> return $ ValB $ v1 <= v2
    GTH -> return $ ValB $ v1 > v2
    GE -> return $ ValB $ v1 > v2
    EQU -> return $ ValB $ v1 == v2
    NE -> return $ ValB $ v1 /= v2

interpretExpr (EAnd expr1 expr2) = do
  v1 <- getBool <$> interpretExpr expr1
  v2 <- getBool <$> interpretExpr expr2
  return $ ValB $ v1 && v2

interpretExpr (EOr expr1 expr2) = do
  v1 <- getBool <$> interpretExpr expr1
  v2 <- getBool <$> interpretExpr expr2
  return $ ValB $ v1 || v2

interpretExpr (EApp (Ident func) pass) = do
  env <- ask
  let l = fromJust (M.lookup func env)
  (st, _) <- get
  let f@(Func args env block) = fromJust (M.lookup l st)
  toPass <- preArgs pass args
  local (const env) (evalFunc f toPass)

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transBVAL :: BVAL -> Result
transBVAL x = case x of
  BVAL string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog topdefs -> failure x
transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef type_ ident args block -> failure x
transArg :: Arg -> Result
transArg x = case x of
  ArgByVal type_ ident -> failure x
  ArgByVar type_ ident -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  BlockStmt stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ items -> failure x
  Ass ident expr -> failure x
  Incr ident -> failure x
  Decr ident -> failure x
  Ret expr -> failure x
  VRet -> failure x
  Cond expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  Subsection ident block -> failure x
  SubsectionPaid ident expr block -> failure x
  Repeat -> failure x
  Finish -> failure x
  Earn expr -> failure x
  RepeatXTimes expr -> failure x
  SExp expr -> failure x
transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x
  TStr -> failure x
  TBool -> failure x
  TVoid -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar ident -> failure x
  ELitInt integer -> failure x
  ELitBool bval -> failure x
  EApp ident exprs -> failure x
  EString string -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x