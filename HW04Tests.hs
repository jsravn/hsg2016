module HW04Tests where

import           HW04
import           Test.HUnit

xTests :: Test
xTests = TestList
    [ TestCase (assertEqual "int" (P [0, 1] :: Poly Int) (x :: Poly Int))
    , TestCase (assertEqual "double" (P [0.0, 1.0] :: Poly Double) (x :: Poly Double))
    ]

equalTests :: Test
equalTests = TestList
    [ TestCase (assertEqual "" (P [5, 1, 2] :: Poly Int) (P [5, 1, 2] :: Poly Int))
    , TestCase (assertEqual "" (P [2, 3, 0] :: Poly Int) (P [2, 3] :: Poly Int))
    , TestCase (assertBool "" $ (P [1, 2, 3] :: Poly Int) /= (P [1, 2] :: Poly Int))
    ]

showTests :: Test
showTests = TestList
    [ TestCase (assertEqual "whole shebang" "9x^4 + x^2 + x + -3" (show (P [-3, 1, 1, 0, 9] :: Poly Int)))
    , TestCase (assertEqual "zero" "0" (show (P [0, 0, 0, 0] :: Poly Int)))
    , TestCase (assertEqual "one" "x + 1" (show (P [1, 1] :: Poly Int)))
    , TestCase (assertEqual "minus one" "2x^2 + -x" (show (P [0, -1, 2] :: Poly Int)))
    , TestCase (assertEqual "minus one again" "x^2 + -1" (show (P [-1, 0, 1] :: Poly Int)))
    , TestCase (assertEqual "x" "x" (show (x :: Poly Int)))
    , TestCase (assertEqual "leading zeros dropped" "3x^3" (show (P [0, 0, 0, 3, 0, 0] :: Poly Int)))
    ]

plusTests :: Test
plusTests = TestList
    [ TestCase (assertEqual "" (P [6, 1, 3] :: Poly Int) (P [5, 0, 1] + P [1, 1, 2]))
    , TestCase (assertEqual "" (P [2, 1, 1] :: Poly Int) (P [1, 0, 1] + P [1, 1]))
    ]

timesTests :: Test
timesTests = TestList
    [ TestCase (assertEqual "" (P [2, 4, 4, 2] :: Poly Int) (P [1, 1, 1] * P [2, 2]))
    ]

instanceTests :: Test
instanceTests = TestList
    [ TestCase (assertEqual "" (P [-1, -2, -3] :: Poly Int) (negate $ P [1, 2, 3]))
    , TestCase (assertEqual "" (P [99] :: Poly Int) 99)
    ]

applyTests :: Test
applyTests = TestList
    [ TestCase (assertEqual "" (4 :: Int) (applyP (x^2 + 2*x + 1) 1))
    , TestCase (assertEqual "" (9 :: Int) (applyP (x^2 + 2*x + 1) 2))
    ]

derivTests :: Test
derivTests = TestList
    [ TestCase (assertEqual "" (2*x + 3) (deriv (x^2 +3*x + 5)))
    , TestCase (assertEqual "" (1008*x^6 - 18) (nderiv 3 (2*x^9 - 3*x^3 + 2)))
    ]
--

runTests :: IO Counts
runTests = runTestTT $ TestList
    [ TestLabel "test x" xTests
    , TestLabel "test Eq" equalTests
    , TestLabel "test show" showTests
    , TestLabel "test plus" plusTests
    , TestLabel "test times" timesTests
    , TestLabel "test instance" instanceTests
    , TestLabel "test applyP" applyTests
    , TestLabel "test deriv" derivTests
    ]
