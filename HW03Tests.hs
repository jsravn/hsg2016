module HW03Tests where

import HW03
import Test.HUnit

tests :: Test
tests = TestList
    [
      TestCase (assertEqual "empty" 0 (empty "B"))
    , TestCase (assertEqual "evalE" 5 (evalE empty (Val 5)))
    , TestCase (assertEqual "evalE" 0 (evalE empty (Op (Val 1) Eql (Val 2))))
    , TestCase (assertEqual "evalE" 3 (evalE (extend empty "B" 3) (Var "B")))
    ]

runTests :: IO Counts
runTests = runTestTT tests
