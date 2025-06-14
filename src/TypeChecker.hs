{-# LANGUAGE GADTs #-}

module TypeChecker where
import Data.Typeable
import Type
import TypedExpr
import UntypedExpr

type TypeEnv = [(String, Type)]

data TypedExprBox where
    TypedExprBox :: Typeable a => TypedExpr a -> TypedExprBox


typeCheck :: TypeEnv -> UntypedExpr -> Either String Type
typeCheck env uexpr = case uexpr of
    EInt i -> Right TInt
    EBool b -> Right TBool
    EVar x -> let t = lookup x env
              in case t of
                    Nothing -> Left ("Unbound variable " ++ x)
                    Just ty -> Right ty

    EAdd n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch"
    EMul n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch"
    EDiv n m -> do
        t1 <- typeCheck env n
        t2 <- typeCheck env m
        case (t1, t2) of
            (TInt, TInt) -> return TInt
            _ -> Left "Type mismatch"
    EIsZero m -> do
        t <- typeCheck env m
        if t == TInt then Right TBool else Left "Type mismatch"
    EIf g e1 e2 -> do
        tg <- typeCheck env g
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case tg of
            TBool -> if t1 == t2 then Right t1 else Left "Type mismatch between if cases"
            _ -> Left "Type mismatch on boolean guard"
    EFun par body -> typeCheck env body
    _ -> Left "TBD"