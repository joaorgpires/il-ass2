data Expr = Var String
    | Const Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | IfZero Expr Expr Expr
    | Lam String Expr
    | App Expr Expr
    | Let String Expr Expr
    | Fix Expr
    deriving (Eq, Show)

data SECD = LD String -- Load Variable
    | LDC Int -- Load constant
    | LDF [SECD] -- Load Function
    | LDRF [SECD] -- Load Recursive Function
    | APP -- Apply
    | ENT -- Enter -> Adds to environment (x, compile e2)
    | RTN -- Return -> Removes from environment (x, compile e2)
    | SEL [SECD] [SECD] -- Select zero/non-zero
    | JOIN -- Join main control
    | ADD -- Add
    | SUB -- Subtract
    | MUL -- Multiply
    | HALT -- Halt execution
    deriving (Show)

data Value = ConstValue Int
    | Closure Expr Env
    deriving (Show)

type Env = [(String, Value)]

compile :: Expr -> Env -> [SECD] -- Sym is the symbol table
compile (Var x) sym = [LD x]
compile (Const k) sym = [LDC k]
compile (Add e1 e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [ADD]
compile (Sub e1 e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [SUB]
compile (Mul e1 e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [MUL]
--compile (Lam x e) = [Closure x (compile e)]
compile (App e1 e2) sym = (compile e2 sym) ++ (compile e1 sym) ++ [APP]
compile (IfZero e1 e2 e3) sym = (compile e1 sym) ++ [SEL (compile e2 sym) (compile e3 sym)]
compile (App (Lam x e1) e2) sym = (compile e2 sym) ++ [ENT] ++ (compile e1 sym) ++ [RTN]

--exec :: [SECD String]
