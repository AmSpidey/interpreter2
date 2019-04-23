module Simplified where

import AbsOstaczGr

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[a]] = [a]
flatten (x: xs) = x++ flatten xs

concatBlocks :: Block -> Block -> Block
concatBlocks (BlockStmt stmts1) (BlockStmt stmts2) = BlockStmt (stmts1++stmts2)