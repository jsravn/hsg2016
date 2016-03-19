module HW03Tests where

import           HW03
import           Test.HUnit

evalTests :: Test
evalTests = TestList
    [
      TestCase (assertEqual "" 5 (evalE empty (Val 5)))
    , TestCase (assertEqual "" 0 (evalE empty (Op (Val 1) Eql (Val 2))))
    , TestCase (assertEqual "" 3 (evalE (extend empty "B" 3) (Var "B")))
    ]

desugarTests :: Test
desugarTests = TestList
    [
      TestCase (assertEqual "assign"
        (DAssign "A" (Val 3))
        (desugar $ Assign "A" (Val 3))
      )
    , TestCase (assertEqual "if"
        (DIf (Val 0) DSkip DSkip)
        (desugar $ If (Val 0) Skip Skip)
      )
    , TestCase (assertEqual "while"
        (DWhile (Val 0) DSkip)
        (desugar $ While (Val 0) Skip)
      )
    , TestCase (assertEqual "sequence"
        (DSequence DSkip DSkip)
        (desugar $ Sequence Skip Skip)
      )
    , TestCase (assertEqual "skip"
        DSkip
        (desugar Skip)
      )
    , TestCase (assertEqual "incr"
        (DAssign "A" (Op (Var "A") Plus (Val 1)))
        (desugar $ Incr "A")
      )
    , TestCase (assertEqual "for"
        (DSequence
          (DAssign "A" (Val 0))
          (DWhile
            (Op (Var "A") Lt (Val 6))
            (DSequence
              (DAssign "B" (Op (Var "B") Plus (Val 2)))
              (DAssign "A" (Op (Var "A") Plus (Val 1)))
            )
          )
        )
        (desugar $
          For
            (Assign "A" (Val 0))
            (Op (Var "A") Lt (Val 6))
            (Incr "A")
            (Assign "B" (Op (Var "B") Plus (Val 2)))
        )
      )
    ]

evalSimpleTests :: Test
evalSimpleTests = TestList
    [
    ]

-- Add tests below to be run

tests :: Test
tests = TestList
    [
      TestLabel "empty state" (TestCase (assertEqual "empty" 0 (empty "B")))
    , TestLabel "evalE" evalTests
    , TestLabel "desugar" desugarTests
    ]

runTests :: IO Counts
runTests = runTestTT tests
