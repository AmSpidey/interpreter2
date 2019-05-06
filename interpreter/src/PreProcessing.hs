module PreProcessing where

import AbsOstaczGr
import Simplified
import Debug.Trace

preProcess :: Program -> Program
preProcess (Prog t) = Prog (fmap preDecls t)

preDecls :: Decl -> Decl
preDecls (FnDef t x args b) = FnDef t x args (preBlock b)
preDecls r = r

preBlock :: Block -> Block
preBlock (BlockStmt stms) = BlockStmt (fmap preStmt stms)

preStmt :: Stmt -> Stmt
preStmt (Incr v) = (Ass v ((EAdd (EVar v) Plus (ELitInt 1))))

preStmt (Decr v) = (Ass v ((EAdd (EVar v) Minus (ELitInt 1))))

preStmt (BStmt b) = BStmt (preBlock b)

preStmt stmt = stmt