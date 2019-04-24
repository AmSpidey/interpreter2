module TypeCheckExpr where
import Control.Monad(when)
import Control.Monad.Reader
import Control.Monad.State
import AbsOstaczGr
import qualified Data.Map as M
import ErrM
import Control.Monad.Except

data Types = TI | TB | TS | Types :->: Types deriving (Eq, Show)

unifyTypes :: Types -> Types -> Types -> S ()
unifyTypes t1 t2 t = Control.Monad.when (t1 /= t2 || t1 == t) $ throwError defaultErr

transType :: Type -> Types
transType TInt = TI
transType TBool = TB
transType TStr = TS

type TypeError = String
defaultErr = "error in typeChecker. Go check your code!"
fatalErr = "fatal error in typeChecker. This shouldn't have happened. Blamey."

type TypeEnv = M.Map String Types
type S a = ReaderT TypeEnv (Except TypeError) a

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
  unifyTypes t1 t2 TI
  return TI
checkExpr (EAdd expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2 TI
  return TI
checkExpr (ERel expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2 TI
  return TB
checkExpr (EAnd expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2 TB
  return TB
checkExpr (EOr expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  unifyTypes t1 t2 TB
  return TB

