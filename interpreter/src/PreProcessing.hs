module PreProcessing where

import AbsOstaczGr
import Simplified
import Debug.Trace

preProcess :: Program -> Program
preProcess (Prog t) = (Prog (fmap preTopDef t))

preTopDef :: TopDef -> TopDef
preTopDef (FnDef t x args b) = FnDef t x args (preBlock b)

preBlock :: Block -> Block
preBlock (BlockStmt stms) = BlockStmt (flatten (fmap preStmt stms))

preStmt :: Stmt -> [Stmt]
preStmt (Decl t []) = [Empty]
preStmt (Decl t (NoInit x:items)) = PreDecl t x : preStmt (Decl t items)
preStmt (Decl t (Init x expr:items)) = PreDecl t x : Ass x expr : preStmt (Decl t items)
preStmt stmt = [stmt]