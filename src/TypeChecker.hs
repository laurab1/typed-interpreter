module TypeChecker where
import Type
import UntypedExpr

type TypeEnv = [(String, Type)]

typeCheck :: TypeEnv -> UntypedExpr -> Either String Type
typeCheck env uexpr = case uexpr of
    EInt _ -> Right TInt
    EBool _ -> Right TBool
    EVar x -> let t = lookup x env
              in case t of
                    Nothing -> Left ("Unbound variable " ++ x)
                    Just ty -> Right ty
    EAdd n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch: add arguments must be integer"
    ESub n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch: sub arguments must be integer"
    EMul n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch: mul arguments must be integer"
    EDiv n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch: div arguments must be integer"
    EIsZero m -> do
        t <- typeCheck env m
        if t == TInt then Right TBool else Left "Type mismatch: iszero argument must be an integer"
    EIf g e1 e2 -> do
        tg <- typeCheck env g 
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case tg of
            TBool -> if t1 == t2 then Right t1 else Left "Type mismatch between if cases"
            _ -> Left "Type mismatch: if guard must be boolean"
    EFun par t body -> do
        t1 <- typeCheck ((par,t):env) body
        return (TFun t t1)
    ELet ide expr body -> do
        t1 <- typeCheck env expr
        typeCheck ((ide, t1) : env) body
    EApp f arg -> do
        tfun <- typeCheck env f
        targ <- typeCheck env arg
        case tfun of
            TFun t1 t2 -> if t1 == targ then Right t2 else Left "Type mismatch on function argument"
            _ -> Left "Type mismatch: not a function"
    ELetRec f tfun par tpar funbody letbody -> do
        t <- typeCheck ((f,tfun):(par,tpar):env) funbody
        case tfun of
            TFun targ tbody -> if t == tbody && targ == tpar then typeCheck ((f,tfun):(par,tpar):env) letbody else Left "Type mismatch on recursive function argument"
            _ -> Left "Type mismatch: not a function"
