module Main where
import UntypedExpr
import Interpreter

main :: IO ()
main = print (runProgram [] (EDiv (EInt 4) (EBool True)))
