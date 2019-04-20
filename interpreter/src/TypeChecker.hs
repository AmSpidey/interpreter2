module TypeChecker where

import AbsOstaczGr
import ErrM
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe)
import Control.Monad.Except

type TypeError = String

type Types = M.Map String Type
type S a = ReaderT Types (Except TypeError) a

checkExpr :: Expr -> S Type
checkExpr (EVar (Ident x)) = do
  env <- ask
  let mt = M.lookup x env
  when (isNothing mt)
checkExpr (ELitInt _) = return TInt
checkExpr (ELitBool _) = return TBool
checkExpr (EString _) = return TStr
checkExpr (Not b) = do
  t <- checkExpr b
  case t of
    TBool -> return TBool
    _ -> throwError "error in typeChecker"
checkExpr (Neg x) = do
  t <- checkExpr x
  case t of
    TInt -> return TInt
    _ -> throwError "error in typeChecker"
checkExpr (Neg x) = do
  t <- checkExpr x
  case t of
    TInt -> return TInt
    _ -> throwError "error in typeChecker"