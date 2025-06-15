module Main where
import TypeChecker
import Value
import UntypedExpr

eval :: Env UntypedExpr -> UntypedExpr -> Either String Value
eval env uexpr = case uexpr of
    EInt n -> Right (VInt n)
    EBool b -> Right (VBool b)
    EVar x -> case lookup x env of
        Nothing -> Left ("Unbound variable " ++ x)
        Just e -> eval env e
    EAdd n m -> do
        a <- eval env n
        b <- eval env m
        case (a,b) of
            (VInt a1, VInt a2) -> return (VInt (a1 + a2))
    EMul n m -> do
        a <- eval env n
        b <- eval env m
        case (a,b) of
            (VInt a1, VInt a2) -> return (VInt (a1 * a2))
    EIsZero m -> do
        m1 <- eval env m
        if m1 == VInt 0 then Right (VBool True) else Right (VBool False)
    EDiv n m -> case eval env (EIsZero m) of
        Right (VBool True) -> Left "Division by zero"
        _ -> do
            a <- eval env n
            b <- eval env m
            case (a,b) of
                (VInt a1, VInt a2) -> return (VInt (a1 `div` a2))
    ELet ide e1 e2 -> eval ((ide, e1):env) e2
    _ -> Left "Stub"

runProgram :: Env UntypedExpr -> UntypedExpr -> Either String Value
runProgram env uexpr = typeCheck [] uexpr >> eval env uexpr

main :: IO ()
main = print (runProgram [] (EDiv (EInt 4) (EBool True)))
