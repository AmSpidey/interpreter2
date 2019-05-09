{-# LANGUAGE FlexibleContexts #-}
module Simplified where

import AbsOstaczGr
import TypeCheckExpr
import Data.Map as M
import Control.Monad.Error.Class

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[a]] = [a]
flatten (x: xs) = x++ flatten xs

concatBlocks :: Block -> Block -> Block
concatBlocks (BlockStmt stmts1) (BlockStmt stmts2) = BlockStmt (stmts1++stmts2)

{--preDeclFunctions :: MonadError String m => [TopDef] -> TypeEnv -> m TypeEnv
preDeclFunctions [] env = return env
preDeclFunctions (FnDef t (Ident f) args block:funcs) env = do
  let mt = M.lookup f env
  if isNothing mt
    then preDeclFunctions funcs (M.insert f (typeFromArgs args (transType t)) env)
    else trace ("error in preDeclFunctions") $ throwError defaultErr

typeFromArgs :: [Arg] -> Types -> Types
typeFromArgs [] t = t
typeFromArgs (ArgByVal a _:args) t = transType a :->: typeFromArgs args t
typeFromArgs (ArgByVar a _:args) t = transType a :->: typeFromArgs args t

checkTopDef :: TopDef -> S ()
checkTopDef (FnDef t (Ident f) args block) = do
  env <- ask
  let mt = M.lookup f env
  if isNothing mt
    then trace ("error in checkTopDef") $ throwError defaultErr
    else local (M.insert "" (transType t)) funcWithDecls
  where
    funcWithDecls = checkBlock (concatBlocks (declFromArgs args) block)-}