{-# LANGUAGE GADTs #-}

module TypedExpr where
import Type

data TypedExpr a where
    TEInt    :: Int -> TypedExpr Int
    TEBool   :: Bool -> TypedExpr Bool
    TEAdd    :: TypedExpr Int -> TypedExpr Int -> TypedExpr Int
    TEMul    :: TypedExpr Int -> TypedExpr Int -> TypedExpr Int
    TEDiv    :: TypedExpr Int -> TypedExpr Int -> TypedExpr Int
    TEIf     :: TypedExpr Bool -> TypedExpr a -> TypedExpr a -> TypedExpr a
    TEIsZero :: TypedExpr Int -> TypedExpr Bool
    TEVar    :: String -> TypedExpr a
    TELet    :: String -> TypedExpr a -> TypedExpr b -> TypedExpr b
    TEFun    :: String -> Type -> TypedExpr b -> TypedExpr (a -> b)
    TEApp    :: TypedExpr (a -> b) -> TypedExpr a -> TypedExpr b
    TELetRec :: String -> String -> Type -> TypedExpr b -> TypedExpr c -> TypedExpr c