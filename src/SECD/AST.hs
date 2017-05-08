module SECD.AST where

data Expr =
    Const Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Lam String Expr
    | App Expr Expr
    | Fix Expr
    | If Expr Expr Expr
    deriving (Show)

data SECD =
    LDC Int
    | LD Int
    | LDF Code 
    | LDRF Code
    | ADD
    | SUB
    | MUL
    | DIV
    | AP
    | RTN
    | SEL Code Code
    | JOIN
    | HALT
    deriving (Show)

data Value = 
    Int Int 
    | Addr Addr 
    deriving (Show)

type Var = String
type Addr = Int
type Sym = [(Var, Addr)]

type Stack = [Value]
type Env = [Value]
type Code = [SECD]
type Dump = [(Stack, Env, Code)]

type Store = [(Addr, Closure)]
type Closure = (Code, Env)

type State = (Stack, Env, Code, Dump, Store)