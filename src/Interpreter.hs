module Interpreter where

-- Haskell module generated by the BNF converter
-- Parts dedicated to store, memory heavily inspired by code
-- for Tiny that was published by our teachers
import AbsOstaczGr
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import ErrM

type Result = Err String

-- TODO: starting env has to have main! type int.
failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

data Value
  = ValInt Integer
  | ValS String
  | ValB Bool
  | ValV
  | LF LoopFlag
  | Func [ValueArg]
         Env
         Block
  deriving (Ord, Eq)

instance Show Value where
  show (ValInt i) = show i
  show (ValS s) = s
  show (ValB b) = show b
  show ValV = show "void val"
  show _ = error errInVal

data ValueArg
  = ByVar Ident
  | ByVal Ident
  deriving (Ord, Eq, Show)

data Passed
  = Val Value
  | Var Loc

getBool :: Value -> Bool
getBool (ValB v) = v
getBool _ = error "error in typeChecker for bool"

getInt :: Value -> Integer
getInt (ValInt v) = v
getInt _ = error "error in typeChecker for integer"

type Loc = Int

-- environment
type Env = M.Map String Loc

-- state and next free location
type Mem = M.Map Loc Value

type Coins = Integer

data LoopFlag
  = FIN
  | REP Integer
  deriving (Ord, Eq, Show)

type Store = (Mem, Loc, Coins)

type SS a = StateT Store (ReaderT Env (ExceptT String IO)) a

-- reserving new location
newloc :: SS Loc
newloc = do
  (st, l, c) <- get
  put (st, l + 1, c)
  return l

modifyMem :: (Mem -> Mem) -> SS ()
modifyMem f = modify (\(st, l, c) -> (f st, l, c))

-- TODO: delete or use.
inLocal :: Env -> SS a -> SS a
inLocal locEnv = local (const locEnv)

earn :: Integer -> SS ()
earn x
  | x < 0 = throwError ("trying to earn less than 0 amount of money " ++ (show x))
  | otherwise = do
    (mem, loc, coins) <- get
    put (mem, loc, coins + x)

spend :: Integer -> SS ()
spend x = do
  (mem, loc, coins) <- get
  put (mem, loc, coins - x)

decVar :: Ident -> SS a -> SS a
decVar (Ident v) g = do
  l <- newloc
  local (M.insert v l) g

setVar :: Ident -> Value -> SS a -> SS a
setVar (Ident v) val g = do
  env <- ask
  let l = fromJust (M.lookup v env)
  (st, ml, c) <- get
  put (M.insert l val st, ml, c)
  g

instance Num Value where
  abs (ValInt a) = ValInt (abs a)
  abs _ = error errInVal
  signum (ValInt a) = ValInt (signum a)
  signum _ = error errInVal
  fromInteger = ValInt
  ValInt a + ValInt b = ValInt $ a + b
  _ + _ = error errInVal
  ValInt a - ValInt b = ValInt $ a - b
  _ - _ = error errInVal
  ValInt a * ValInt b = ValInt $ a * b
  _ * _ = error errInVal

instance Real Value where
  toRational (ValInt a) = toRational a
  toRational _ = error errInVal

instance Enum Value where
  toEnum a = ValInt (toEnum a)
  fromEnum (ValInt a) = fromEnum a
  fromEnum _ = error errInVal

instance Integral Value where
  toInteger (ValInt a) = a
  toInteger _ = error errInVal
  rem (ValInt a) (ValInt b) = ValInt $ rem a b
  rem _ _ = error errInVal
  mod (ValInt a) (ValInt b) = ValInt $ mod a b
  mod _ _ = error errInVal
  quot (ValInt a) (ValInt b) = ValInt $ quot a b
  quot _ _ = error errInVal
  quotRem a b = (quot a b, rem a b)

errInVal :: String
errInVal = "error in value type"

-- TODO: delete?
type Interpretation a = Maybe a

afterEval :: Program -> IO (Either String Value)
afterEval prog = runExceptT (runReaderT (evalStateT (evalProgram prog) (M.empty, 0, 0)) M.empty)

evalProgram :: Program -> SS Value
evalProgram (Prog []) = error "empty Program. Something went wrong."
evalProgram (Prog [d]) = evalDecl d evalMain
evalProgram (Prog (d:decls)) = evalDecl d (evalProgram (Prog decls))

evalMain :: SS Value
evalMain = interpretExpr (EApp (Ident "main") [])

processArgs :: [Arg] -> [ValueArg]
processArgs [] = []
processArgs (ArgByVal _ v:args) = ByVal v : processArgs args
processArgs (ArgByVar _ v:args) = ByVar v : processArgs args

evalDecl :: Decl -> SS a -> SS a
evalDecl (FnDef _ (Ident f) args block) g = do
  env <- ask
  l <- newloc
  let newEnv = M.insert f l env
  local (const newEnv) (setVar (Ident f) (Func (processArgs args) newEnv block) g)
evalDecl (VarDecl _ []) g = g
evalDecl (VarDecl t (Init v e:items)) g = do
  val <- interpretExpr e
  decVar v (setVar v val (evalDecl (VarDecl t items) g))

evalLoop :: Block -> Integer -> SS (Maybe Value)
evalLoop (BlockStmt (Subsection _ _:stmts)) 0 = evalBlock (BlockStmt stmts)
evalLoop (BlockStmt (Subsection i b:stmts)) x = do
  res <- evalBlock b
  case res of
    Nothing -> evalLoop (BlockStmt (Subsection i b : stmts)) (x - 1)
    Just (LF FIN) -> evalBlock (BlockStmt stmts)
    Just (LF (REP y)) -> evalLoop (BlockStmt (Subsection i b : stmts)) (x + y)
    _ -> return res
evalLoop _ _ = error "put loop inside evalLoop. Something went wrong."

catchRet :: Maybe Value -> SS (Maybe Value) -> SS (Maybe Value)
catchRet r g =
  if isNothing r
    then g
    else return r

evalBlock :: Block -> SS (Maybe Value)
evalBlock (BlockStmt []) = return Nothing
evalBlock (BlockStmt (DeclStmt decls:stmts)) = evalDecl decls (evalBlock (BlockStmt stmts))
evalBlock (BlockStmt (Empty:stmts)) = evalBlock (BlockStmt stmts)
evalBlock (BlockStmt (BStmt b:stms)) = do
  res <- evalBlock b
  catchRet res (evalBlock (BlockStmt stms))
evalBlock (BlockStmt (Ass v e:stmts)) = do
  val <- interpretExpr e
  setVar v val (evalBlock (BlockStmt stmts))
evalBlock (BlockStmt (Ret e:_)) = do
  val <- interpretExpr e
  return (Just val)
evalBlock (BlockStmt (VRet:_)) = return (Just ValV)
evalBlock (BlockStmt (Cond e s:stmts)) = do
  val <- interpretExpr e
  let cond = getBool val
  if cond
    then do
      res <- evalBlock (BlockStmt [s])
      catchRet res (evalBlock (BlockStmt stmts))
    else evalBlock (BlockStmt stmts)
evalBlock (BlockStmt (CondElse e s1 s2:_)) = do
  val <- interpretExpr e
  let cond = getBool val
  if cond
    then evalBlock (BlockStmt [s1])
    else evalBlock (BlockStmt [s2])
evalBlock (BlockStmt (SExp e:stmts)) = do
  _ <- interpretExpr e
  evalBlock (BlockStmt stmts)
evalBlock (BlockStmt (Subsection i b:stmts)) = evalLoop (BlockStmt (Subsection i b : stmts)) 1
evalBlock (BlockStmt (SubsectionPaid i expr b:stmts)) = do
  val <- interpretExpr expr
  let toPay = getInt val
  (_, _, toSpend) <- get
  when (toPay < 0) $ throwError "section worth less than 0"
  if toSpend >= toPay
    then do
      spend toPay
      evalLoop (BlockStmt (Subsection i b : stmts)) 1
    else evalBlock (BlockStmt stmts)
evalBlock (BlockStmt (RepeatXTimes expr:_)) = do
  val <- interpretExpr expr
  let int = getInt val
  return (Just (LF (REP int)))
evalBlock (BlockStmt (Repeat:stmts)) = evalBlock (BlockStmt (RepeatXTimes (ELitInt 1) : stmts))
evalBlock (BlockStmt (Finish:_)) = return (Just (LF FIN))
evalBlock (BlockStmt (Show expr:stmts)) = do
  val <- interpretExpr expr
  liftIO $ putStr (show val)
  evalBlock (BlockStmt stmts)
evalBlock (BlockStmt (Earn expr:stmts)) = do
  val <- interpretExpr expr
  earn (getInt val)
  evalBlock (BlockStmt stmts)
evalBlock (BlockStmt (Incr v:stmts)) = evalBlock (BlockStmt (Ass v (EAdd (EVar v) Plus (ELitInt 1)) : stmts))
evalBlock (BlockStmt (Decr v:stmts)) = evalBlock (BlockStmt (Ass v (EAdd (EVar v) Minus (ELitInt 1)) : stmts))

evalFunc :: Value -> [Passed] -> SS Value
evalFunc (Func [] _ block) [] = do
  ret <- evalBlock block
  case ret of
    Nothing -> throwError "no return statement in the end of function"
    Just (LF _) -> throwError "repeat or finish statement not in the section"
    Just r -> return r
evalFunc (Func (ByVal v:args) env block) (Val p:expr) = decVar v $ setVar v p $ evalFunc (Func args env block) expr
evalFunc (Func (ByVar (Ident v):args) env block) (Var l:expr) =
  local (M.insert v l) (evalFunc (Func args env block) expr)
evalFunc _ _ = error "non-matching argument to function. Possible fault with TypeChecker"

preArgs :: [Expr] -> [ValueArg] -> SS [Passed]
preArgs [] [] = return []
preArgs (EVar (Ident v):e) (ByVar _:args) = do
  env <- ask
  let l = fromJust (M.lookup v env)
  res <- preArgs e args
  return (Var l : res)
preArgs (e:expr) (ByVal _:args) = do
  val <- interpretExpr e
  res <- preArgs expr args
  return (Val val : res)
preArgs (_:_) (ByVar _:_) = throwError "passing expression as argument by variable"
preArgs _ _ = error ""

interpretExpr :: Expr -> SS Value
interpretExpr (EVar (Ident var)) = do
  env <- ask
  let l = fromMaybe (error "undefined variable") (M.lookup var env)
  (st, _, _) <- get
  return $ fromMaybe (error "undefined location") (M.lookup l st)
interpretExpr (ELitInt integer) = return $ ValInt integer
interpretExpr (ELitBool (BVAL "true")) = return $ ValB True
interpretExpr (EString string) = return $ ValS string
interpretExpr (EAdd expr1 op expr2) = do
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
  (st, _, _) <- get
  let f@(Func args envF _) = fromJust (M.lookup l st)
  toPass <- preArgs pass args
  local (const envF) (evalFunc f toPass)
interpretExpr _ = error "not handled expression type"

transIdent :: Ident -> Result
transIdent x =
  case x of
    Ident _ -> failure x

transBVAL :: BVAL -> Result
transBVAL x =
  case x of
    BVAL _ -> failure x

transProgram :: Program -> Result
transProgram x =
  case x of
    Prog _ -> failure x

transDecl :: Decl -> Result
transDecl x =
  case x of
    FnDef {} -> failure x
    VarDecl _ _ -> failure x

transArg :: Arg -> Result
transArg x =
  case x of
    ArgByVal _ _ -> failure x
    ArgByVar _ _ -> failure x

transBlock :: Block -> Result
transBlock x =
  case x of
    BlockStmt _ -> failure x

transStmt :: Stmt -> Result
transStmt x =
  case x of
    Empty -> failure x
    BStmt _ -> failure x
    DeclStmt _ -> failure x
    Ass _ _ -> failure x
    Incr _ -> failure x
    Decr _ -> failure x
    Ret _ -> failure x
    VRet -> failure x
    Cond _ _ -> failure x
    CondElse {} -> failure x
    Subsection _ _ -> failure x
    SubsectionPaid {} -> failure x
    Repeat -> failure x
    Finish -> failure x
    Earn _ -> failure x
    RepeatXTimes _ -> failure x
    SExp _ -> failure x
    Show _ -> failure x

transItem :: Item -> Result
transItem x =
  case x of
    Init _ _ -> failure x

transType :: Type -> Result
transType x =
  case x of
    TInt -> failure x
    TStr -> failure x
    TBool -> failure x
    TVoid -> failure x

transExpr :: Expr -> Result
transExpr x =
  case x of
    EVar _ -> failure x
    ELitInt _ -> failure x
    ELitBool _ -> failure x
    EApp _ _ -> failure x
    EString _ -> failure x
    Neg _ -> failure x
    Not _ -> failure x
    EMul {} -> failure x
    EAdd {} -> failure x
    ERel {} -> failure x
    EAnd _ _ -> failure x
    EOr _ _ -> failure x

transAddOp :: AddOp -> Result
transAddOp x =
  case x of
    Plus -> failure x
    Minus -> failure x

transMulOp :: MulOp -> Result
transMulOp x =
  case x of
    Times -> failure x
    Div -> failure x
    Mod -> failure x

transRelOp :: RelOp -> Result
transRelOp x =
  case x of
    LTH -> failure x
    LE -> failure x
    GTH -> failure x
    GE -> failure x
    EQU -> failure x
    NE -> failure x