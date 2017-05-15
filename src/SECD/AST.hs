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
type Sym = [Var]
type Stack = [Value]
type Env = [Value]
type Code = [SECD]
type Dump = [(Stack, Env, Code)]

type Store = [(Addr, Closure)]
type Closure = (Code, Env)

type State = (Stack, Env, Code, Dump, Store)

-- infix helper functions

infixl 8 <**>, <//>
infixl 7 <++>, <-->

(<++>) :: Expr -> Expr -> Expr
e1 <++> e2 = Add e1 e2

(<-->) :: Expr -> Expr -> Expr
e1 <--> e2 = Sub e1 e2

(<**>) :: Expr -> Expr -> Expr
e1 <**> e2 = Mul e1 e2

(<//>) :: Expr -> Expr -> Expr
e1 <//> e2 = Div e1 e2