module SECD.Examples where

import SECD.AST
import SECD.Compile
import SECD.Execute

-- Factorial (Fix point)
fact = Fix (Lam "f" (Lam "n" (If (Var "n") (Const 1) ((App (Var "f") (Var "n" <--> Const 1)) <**> Var "n"))))
fact10 = App fact (Const 10)
-- Let example
ex1 = Let "x" (Const 10) (Var "x" <++> Const 20)
-- Detecting free variables
ex2 = Let "x" (Const 10) (Var "x" <++> Var "y")

-- Compilation
compileFact10 = compileProgram fact10
compileEx1 = compileProgram ex1
compileEx2 = compileProgram ex2

-- Running
runFact10 = run compileFact10
runEx1 = run compileEx1
runEx2 = run compileEx2
