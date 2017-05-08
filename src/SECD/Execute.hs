module SECD.Execute where

import SECD.AST
import SECD.Table

executeOnce :: State -> State
executeOnce (stack, e, (LDC n):c, d, store)
    = ((Int n):stack, e, c, d, store) -- load a constant
executeOnce (stack, e, (LD addr):c, d, store)
    = (v:stack, e, c, d, store) -- load an address to a variable
    where v = e !! addr
executeOnce (stack, e, (LDF c'):c, d, store)
    = ((Addr a):stack, e, c, d, store ++ [(a, (c', e))]) -- load a function with env stored in closure
    where a = nextAddr store
executeOnce (stack, e, (LDRF c'):c, d, store)
    = ((Addr a):stack, e, c, d, store ++ [(a, (c', (Addr a):e))]) -- load a recursive function with recursive closure
    where a = nextAddr store
executeOnce ((Int n1):(Int n2):stack, e, (ADD):c, d, store)
    = ((Int (n1+n2)):stack, e, c, d, store)
executeOnce ((Int n1):(Int n2):stack, e, (SUB):c, d, store)
    = ((Int (n1-n2)):stack, e, c, d, store)
executeOnce ((Int n1):(Int n2):stack, e, (MUL):c, d, store)
    = ((Int (n1*n2)):stack, e, c, d, store)
executeOnce ((Int n1):(Int n2):stack, e, (DIV):c, d, store)
    = ((Int (n1`div`n2)):stack, e, c, d, store)
executeOnce (argument:(Addr a):stack, e, (AP):c, d, store)
    = ([], argument:e', c', (stack,e,c):d, store) -- apply a function to an argument, so we replace state w/ its stk, env, code
    where Just (c', e') = lookup a store
executeOnce (result:stack, e, (RTN):c, (stk', e', c'):d, store)
    = (result:stk', e', c', d, store) -- exit from function application, so we restore original stk, env and code
executeOnce ((Int 0):stack, e, (SEL c1 _):c, d, store)
    = (stack, e, c1, ([], [], c):d, store) -- if zero, then choose option c1
executeOnce ((Int n):stack, e, (SEL _ c2):c, d, store)
    = (stack, e, c2, ([], [], c):d, store) -- else, choose option c2
executeOnce (stack, e, (JOIN):c, (_, _, c'):d, store)
    = (stack, e, c', d, store) -- going back to normal program flow after exiting if
executeOnce (stack, e, (HALT):c, d, store)
    = (stack, e, [], d, store) -- program is done

execute :: State -> Value
execute st@(stk,_,c,_,_) = case null c of 
    True -> stk !! 0 
    False -> execute $ executeOnce st 

run :: Code -> Value
run c = execute ([],[],c,[],[])