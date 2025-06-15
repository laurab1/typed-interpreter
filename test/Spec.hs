import Test.HUnit
import UntypedExpr
import TypeChecker
import Interpreter
import Value
import Type


testTypeCheckOK :: Test
testTypeCheckOK = TestCase $
  assertEqual "typecheck simple addition"
    (Right TInt)
    (typeCheck [] (EAdd (EInt 2) (EInt 3)))

testTypeCheckFail :: Test
testTypeCheckFail = TestCase $
  assertEqual "typecheck wrong addition (int + bool)"
    (Left "Type mismatch")
    (typeCheck [] (EAdd (EInt 2) (EBool True)))

testTypeCheckFun :: Test
testTypeCheckFun = TestCase $
  assertEqual "typecheck function abstraction"
    (Right (TFun TInt TInt))
    (typeCheck [] (EFun "x" TInt (EAdd (EVar "x") (EInt 1))))

testEvalAdd :: Test
testEvalAdd = TestCase $
  assertEqual "evaluate addition"
    (Right (VInt 5))
    (runProgram [] (EAdd (EInt 2) (EInt 3)))

testEvalIf :: Test
testEvalIf = TestCase $
  assertEqual "evaluate if expression"
    (Right (VInt 42))
    (runProgram [] (EIf (EBool True) (EInt 42) (EInt 0)))

testEvalDivZero :: Test
testEvalDivZero = TestCase $
  assertEqual "division by zero"
    (Left "Division by zero")
    (runProgram [] (EDiv (EInt 5) (EInt 0)))

testEvalFun :: Test
testEvalFun = TestCase $
  assertEqual "evaluate function application"
    (Right (VInt 6))
    (runProgram [] (ELet "f" (EFun "x" TInt (EAdd (EVar "x") (EInt 1)))
                          (EApp (EVar "f") (EInt 5))))

testEvalRec :: Test
testEvalRec = TestCase $
  assertEqual "evaluate recursive factorial (of 3)"
    (Right (VInt 6))
    (runProgram []
      (ELetRec "fact" (TFun TInt TInt) "n" TInt
         (EIf (EIsZero (EVar "n"))
              (EInt 1)
              (EMul (EVar "n") (EApp (EVar "fact") (EAdd (EVar "n") (EInt (-1))))))
         (EApp (EVar "fact") (EInt 3))))

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [ testTypeCheckOK
    , testTypeCheckFail
    , testTypeCheckFun
    , testEvalAdd
    , testEvalIf
    , testEvalDivZero
    , testEvalFun
    , testEvalRec
    ]
  return ()