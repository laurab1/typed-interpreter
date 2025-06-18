import Test.HUnit
import Parser
import Interpreter
import UntypedExpr
import Value
import TypeChecker

testInt :: Test
testInt = TestCase $
    assertEqual "parse and eval int" 
        (Right (VInt 42)) 
        (runProgram [] "42")

testBool :: Test
testBool = TestCase $
    assertEqual "parse and eval bool" 
        (Right (VBool True)) 
        (runProgram [] "true")

testAdd :: Test
testAdd = TestCase $
    assertEqual "parse and eval addition" 
        (Right (VInt 7)) 
        (runProgram [] "3 + 4")

testMul :: Test
testMul = TestCase $
    assertEqual "parse and eval multiplication" 
        (Right (VInt 12)) 
        (runProgram [] "3 * 4")

testIf :: Test
testIf = TestCase $
    assertEqual "parse and eval if true" 
        (Right (VInt 1)) 
        (runProgram [] "if true then 1 else 2")

testLet :: Test
testLet = TestCase $
    assertEqual "parse and eval let binding" 
        (Right (VInt 5)) 
        (runProgram [] "let x = 5 in x + 0")

testFunApp :: Test
testFunApp = TestCase $
    assertEqual "parse and eval simple function application" 
        (Right (VInt 8)) 
        (runProgram [] "let f = fun x:int -> x + 3 in f 5")

testLetRec :: Test
testLetRec = TestCase $
    assertEqual "parse and eval factorial (let rec)" 
        (Right (VInt 120)) 
        (runProgram [] 
            "letrec fact : (int => int) n : int = if iszero n then 1 else n * fact (n - 1) in fact 5")

allTests :: Test
allTests = TestList [testInt, testBool, testAdd, testMul, testIf, testLet, testFunApp, testLetRec]

main :: IO Counts
main = runTestTT allTests
