module Main where
import Parser

main :: IO ()
main =
    print (runParser "letrec fact : (int => int) n : int = if iszero n then 1 else n * fact (n - 1) in fact 5")
