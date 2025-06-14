module Type where

data Type
  = TInt
  | TBool
  | TFun Type Type
  deriving (Eq, Show)
