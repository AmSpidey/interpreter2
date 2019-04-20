-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParOstaczGr where
import AbsOstaczGr
import LexOstaczGr
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&' { PT _ (TS _ 4) }
  '&&' { PT _ (TS _ 5) }
  '(' { PT _ (TS _ 6) }
  ')' { PT _ (TS _ 7) }
  '*' { PT _ (TS _ 8) }
  '+' { PT _ (TS _ 9) }
  '++' { PT _ (TS _ 10) }
  ',' { PT _ (TS _ 11) }
  '-' { PT _ (TS _ 12) }
  '--' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ';' { PT _ (TS _ 15) }
  '<' { PT _ (TS _ 16) }
  '<=' { PT _ (TS _ 17) }
  '=' { PT _ (TS _ 18) }
  '==' { PT _ (TS _ 19) }
  '>' { PT _ (TS _ 20) }
  '>=' { PT _ (TS _ 21) }
  '@_' { PT _ (TS _ 22) }
  'boolean' { PT _ (TS _ 23) }
  'earn' { PT _ (TS _ 24) }
  'else' { PT _ (TS _ 25) }
  'finish' { PT _ (TS _ 26) }
  'if' { PT _ (TS _ 27) }
  'int' { PT _ (TS _ 28) }
  'repeat' { PT _ (TS _ 29) }
  'string' { PT _ (TS _ 30) }
  'times' { PT _ (TS _ 31) }
  'void' { PT _ (TS _ 32) }
  '{' { PT _ (TS _ 33) }
  '||' { PT _ (TS _ 34) }
  '}' { PT _ (TS _ 35) }
  '~' { PT _ (TS _ 36) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }
L_BVAL { PT _ (T_BVAL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }
BVAL    :: { BVAL} : L_BVAL { BVAL ($1)}

Program :: { Program }
Program : ListTopDef { AbsOstaczGr.Prog $1 }
TopDef :: { TopDef }
TopDef : Type Ident '(' ListArg ')' Block { AbsOstaczGr.FnDef $1 $2 $4 $6 }
ListTopDef :: { [TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }
Arg :: { Arg }
Arg : Type Ident { AbsOstaczGr.ArgByVal $1 $2 }
Arg2 :: { Arg }
Arg2 : '&' Type Ident { AbsOstaczGr.ArgByVar $2 $3 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
Block :: { Block }
Block : '{' ListStmt '}' { AbsOstaczGr.BlockStmt (reverse $2) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { AbsOstaczGr.Empty }
     | Block { AbsOstaczGr.BStmt $1 }
     | Type ListItem ';' { AbsOstaczGr.Decl $1 $2 }
     | Ident '=' Expr ';' { AbsOstaczGr.Ass $1 $3 }
     | Ident '++' ';' { AbsOstaczGr.Incr $1 }
     | Ident '--' ';' { AbsOstaczGr.Decr $1 }
     | '~' Expr ';' { AbsOstaczGr.Ret $2 }
     | '~' ';' { AbsOstaczGr.VRet }
     | 'if' '(' Expr ')' Stmt { AbsOstaczGr.Cond $3 $5 }
     | 'if' '(' Expr ')' Stmt 'else' Stmt { AbsOstaczGr.CondElse $3 $5 $7 }
     | '@_' Ident Block { AbsOstaczGr.Subsection $2 $3 }
     | '@_' Ident Expr Block { AbsOstaczGr.SubsectionPaid $2 $3 $4 }
     | 'repeat' { AbsOstaczGr.Repeat }
     | 'finish' { AbsOstaczGr.Finish }
     | 'earn' Expr { AbsOstaczGr.Earn $2 }
     | 'repeat' Expr 'times' { AbsOstaczGr.RepeatXTimes $2 }
     | Expr ';' { AbsOstaczGr.SExp $1 }
Item :: { Item }
Item : Ident { AbsOstaczGr.NoInit $1 }
     | Ident '=' Expr { AbsOstaczGr.Init $1 $3 }
ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }
Type :: { Type }
Type : 'int' { AbsOstaczGr.TInt }
     | 'string' { AbsOstaczGr.TStr }
     | 'boolean' { AbsOstaczGr.TBool }
     | 'void' { AbsOstaczGr.TVoid }
Expr6 :: { Expr }
Expr6 : Ident { AbsOstaczGr.EVar $1 }
      | Integer { AbsOstaczGr.ELitInt $1 }
      | BVAL { AbsOstaczGr.ELitBool $1 }
      | Ident '(' ListExpr ')' { AbsOstaczGr.EApp $1 $3 }
      | String { AbsOstaczGr.EString $1 }
      | '(' Expr ')' { $2 }
Expr5 :: { Expr }
Expr5 : '-' Expr6 { AbsOstaczGr.Neg $2 }
      | '!' Expr6 { AbsOstaczGr.Not $2 }
      | Expr6 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { AbsOstaczGr.EMul $1 $2 $3 }
      | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { AbsOstaczGr.EAdd $1 $2 $3 }
      | Expr4 { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { AbsOstaczGr.ERel $1 $2 $3 }
      | Expr3 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { AbsOstaczGr.EAnd $1 $3 } | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 '||' Expr { AbsOstaczGr.EOr $1 $3 } | Expr1 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AddOp :: { AddOp }
AddOp : '+' { AbsOstaczGr.Plus } | '-' { AbsOstaczGr.Minus }
MulOp :: { MulOp }
MulOp : '*' { AbsOstaczGr.Times }
      | '/' { AbsOstaczGr.Div }
      | '%' { AbsOstaczGr.Mod }
RelOp :: { RelOp }
RelOp : '<' { AbsOstaczGr.LTH }
      | '<=' { AbsOstaczGr.LE }
      | '>' { AbsOstaczGr.GTH }
      | '>=' { AbsOstaczGr.GE }
      | '==' { AbsOstaczGr.EQU }
      | '!=' { AbsOstaczGr.NE }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
