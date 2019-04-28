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

--TODO: pass by variable

data ValueT = ValInt Integer | ValS String | ValB Bool | Func [ValueArg] ValueT Block deriving (Ord, Eq, Show)
data ValueArg = ByVar ValueT | ByVal ValueT

getBool :: ValueT -> Bool
getBool (ValB v) = v

type Loc = Int

-- environment
type Env = M.Map String Loc

-- state and next free location
type Mem = M.Map Loc ValueT
type Store = (Mem, Loc)

--TODO: change into something with exceptions. + how to store functions?
--type SS a = StateT Store (Reader Env) a
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

instance Num ValueT where
  abs (ValInt a) = ValInt (abs a)
  signum (ValInt a) = ValInt (signum a)
  fromInteger = ValInt
  ValInt a + ValInt b = ValInt $ a + b
  ValInt a - ValInt b = ValInt $ a - b
  ValInt a * ValInt b = ValInt $ a * b
instance Real ValueT where
  toRational (ValInt a) = toRational a
instance Enum ValueT where
  toEnum a = ValInt (toEnum a)
  fromEnum (ValInt a) = fromEnum a
instance Integral ValueT where
  toInteger (ValInt a) = a
  rem (ValInt a) (ValInt b)= ValInt $ rem a b
  mod (ValInt a) (ValInt b) = ValInt $ mod a b
  quot (ValInt a) (ValInt b) = ValInt $ quot a b

type Interpretation a = Maybe a

-- TODO: if you reach end of a Block with no return statement, return Exception

evalBlock :: Block -> SS Store
evalBlock b = return (M.empty, 0)

decVar :: Ident -> SS a -> SS a
decVar v g = local (M.insert newloc v) (g)

setVar :: Ident -> Expr -> SS a -> SS a
setVar v e g = do
  val <- interpretExpr e
  env <- ask
  let l = fromJust (M.lookup v env)
  (st,_) <- get
  local (M.insert )

-- TODO: in typechecker check if the expression is variable!
--evalFunc :: ValueT -> [Expr] -> SS Store
--evalFunc (Func ((ByVal v):args) _) (e:expr) = decVar v evalFunc args expr

evalExpr :: Expr -> ValueT
evalExpr expr = runExcept (runReaderT (evalStateT (interpretExpr expr) (M.empty, 0)) M.empty)

interpretExpr :: Expr -> SS ValueT

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

{-interpretExpr (EApp func p:pass) = do
  env <- ask
  let l = fromJust (M.lookup func env)
  (st, _) <- get
  let f = fromJust (M.lookup l st)-}




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