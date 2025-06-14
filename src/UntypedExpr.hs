module UntypedExpr where

data UntypedExpr
  = EInt Int
  | EBool Bool
  | EVar String
  | EAdd UntypedExpr UntypedExpr
  | EMul UntypedExpr UntypedExpr
  | EDiv UntypedExpr UntypedExpr
  | EIf UntypedExpr UntypedExpr UntypedExpr
  | EIsZero UntypedExpr
  | ELet String UntypedExpr UntypedExpr
  | EFun String UntypedExpr
  | EApp UntypedExpr UntypedExpr
  | ELetRec String String UntypedExpr UntypedExpr
  deriving (Eq, Show)