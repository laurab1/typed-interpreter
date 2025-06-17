module UntypedExpr where
import Type

data UntypedExpr
  = EInt Int
  | EBool Bool
  | EVar String
  | EAdd UntypedExpr UntypedExpr
  | EMul UntypedExpr UntypedExpr
  | ESub UntypedExpr UntypedExpr
  | EDiv UntypedExpr UntypedExpr
  | EIf UntypedExpr UntypedExpr UntypedExpr
  | EIsZero UntypedExpr
  | ELet String UntypedExpr UntypedExpr
  | EFun String Type UntypedExpr
  | EApp UntypedExpr UntypedExpr
  | ELetRec String Type String Type UntypedExpr UntypedExpr
  deriving (Eq, Show)