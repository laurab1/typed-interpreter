{-# LANGUAGE InstanceSigs #-}
module Value where
import UntypedExpr

type Env a = [(String, a)]

data Value = VInt Int | VBool Bool | VFun String UntypedExpr (Env UntypedExpr) | VFunRec String String UntypedExpr (Env UntypedExpr)

instance Eq Value where
    (==) :: Value -> Value -> Bool
    VInt x == VInt y = x == y
    VBool x == VBool y = x == y
    _ == _ = False --ignore functions

instance Show Value where
    show :: Value -> String
    show (VInt x) = show x
    show (VBool x) = show x
    show _  = "<function>"