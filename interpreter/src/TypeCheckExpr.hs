module TypeCheckExpr where
import Control.Monad(when)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe, isNothing, fromJust)
import AbsOstaczGr
import Debug.Trace
import qualified Data.Map as M
import ErrM
import Control.Monad.Except

data Types = TI | TB | TS | TV | Types :->: Types deriving (Eq, Show)

unifyTypes :: Types -> Types -> S ()
unifyTypes t1 t2 = when (t1 /= t2) $ throwError (unifyFail ++ show t1 ++ " and " ++ show t2)

transType :: Type -> Types
transType TInt = TI
transType TBool = TB
transType TStr = TS
transType TVoid = TV

isInEnv :: Ident -> S Bool
isInEnv (Ident x) = do
  env <- ask
  let mt = M.lookup x env
  if isNothing mt
    then return False
    else return True

type TypeError = String
defaultErr = "error in typeChecker. Go check your code! Or not. Maybe you have better stuff to do. I'm not your mom."
fatalErr = "fatal error in typeChecker. This shouldn't have happened. Blamey."
noMain = "couldn't find proper int main() function. I personally don't like it."
repNames = "repeating function names."
repVars = "repeating variable names in one environment."
notFound = "variable or function not found in the environment: "
wrongType = "expression is of the wrong type: "
unifyFail = "expressions are supposed to be of the same type, but they are not: "

type TypeEnv = M.Map String Types
type S a = ReaderT TypeEnv (Except TypeError) a

passToTypes :: [Expr] -> Types -> S Types
passToTypes [] ret = return ret
passToTypes (e:expr) ret = do
  t1 <- (checkExpr e)
  t2 <- passToTypes expr ret
  return (t1 :->: t2)

retType :: Types -> S Types
retType (t :->: types) = retType types
retType (t) = return t

checkExpr :: Expr -> S Types
checkExpr (EVar (Ident x)) = do
  env <- ask
  let mt = M.lookup x env
  maybe (throwError (notFound ++ x)) return mt
checkExpr (ELitInt _) = return TI
checkExpr (ELitBool _) = return TB
checkExpr (EString _) = return TS
checkExpr (Not b) = do
  t <- checkExpr b
  case t of
    TB -> return TB
    _ -> throwError (wrongType ++ show b)
checkExpr (Neg x) = do
  t <- checkExpr x
  case t of
    TI -> return TI
    _ -> throwError (wrongType ++ show x)
checkExpr (EMul expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2
  unifyTypes t2 TI
  return TI
checkExpr (EAdd expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2
  unifyTypes t2 TI
  return TI
checkExpr (ERel expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2
  unifyTypes t2 TI
  return TB
checkExpr (EAnd expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2
  unifyTypes t2 TB
  return TB
checkExpr (EOr expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2
  unifyTypes t2 TB
  return TB
checkExpr (EApp (Ident f) pass) = do
  env <- ask
  let t2 = M.lookup f env
  when (isNothing t2) $ throwError (notFound ++ f)
  let t = fromJust t2
  ret <- retType t
  t1 <- passToTypes pass ret
  unifyTypes t1 t
  return ret

