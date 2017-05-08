module SECD.Table where

import SECD.AST 

extend :: a -> [(a,Addr)] -> [(a,Addr)]
extend x l = l ++ [(x, nextAddr l)]

nextAddr :: [(a,b)] -> Addr
nextAddr l = length l + 1
