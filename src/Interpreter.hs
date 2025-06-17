module Interpreter where
import TypeChecker
import Value
import UntypedExpr
import Parser

eval :: Env Value -> UntypedExpr -> Either String Value
eval env uexpr = case uexpr of
    EInt n -> Right (VInt n)
    EBool b -> Right (VBool b)
    EVar x -> case lookup x env of
        Nothing -> Left ("Unbound variable " ++ x)
        Just v -> Right v
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
    ESub n m -> do
        a <- eval env n
        b <- eval env m
        case (a,b) of
            (VInt a1, VInt a2) -> return (VInt (a1 - a2))
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
    EIf g e1 e2 -> do
        tg <- eval env g
        if tg == VBool True then eval env e1 else eval env e2
    ELet ide e1 e2 -> do
        val <- eval env e1
        eval ((ide, val):env) e2
    EFun par tpar body -> Right (VFun par body env)
    EApp f arg -> do
        funVal <- eval env f
        argVal <- eval env arg
        case funVal of
            VFun par body fEnv ->
                eval ((par, argVal) : fEnv) body
    ELetRec f _ par _ funBody letBody -> 
        let recClosure = VFun par funBody recEnv
            recEnv = (f, recClosure) : env
        in eval recEnv letBody


runProgram :: Env Value -> String -> Either String Value
runProgram env input = do
    program <- runParser input
    typeCheck [] program >> eval env program