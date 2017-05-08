module SECD.Compile where

import SECD.AST
import SECD.Table

compile :: Expr -> Sym -> Code
compile (Const k) _ = [LDC k]
compile (Var x) sym = [LD i]
    where (Just i) = (lookup x sym)
compile (Add e1 e2) sym = (compile e2 sym) ++ (compile e1 sym) ++ [ADD]
compile (Sub e1 e2) sym = (compile e2 sym) ++ (compile e1 sym) ++ [SUB]
compile (Mul e1 e2) sym = (compile e2 sym) ++ (compile e1 sym) ++ [MUL]
compile (Div e1 e2) sym = (compile e2 sym) ++ (compile e1 sym) ++ [DIV]
compile (If e0 e1 e2) sym = (compile e0 sym) ++ [SEL c1 c2]
    where 
        c1 = compile e1 sym ++ [JOIN]
        c2 = compile e2 sym ++ [JOIN]
compile (Lam x e) sym = [LDF (compile e s1)] ++ [RTN]
    where s1 = extend x sym  -- extend serve para acrescentar o x ao sym, vai buscar a posicao next na memoria
compile (App e1 e2) sym = (compile e2 sym) ++ (compile e1 sym) ++ [AP]
compile (Fix (Lam f (Lam x e))) sym = [LDRF (compile e s1)] ++ [RTN]
    where s1 = extend x (extend f sym)