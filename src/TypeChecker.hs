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
checkExpr (ELitInt _) = do
  return Int
checkExpr (ELitBool _) = do
  return Bool
checkExpr (EString _) = do
  return Str
checkExpr
{--checkExpr :: Expr -> Type -> S

check-}
