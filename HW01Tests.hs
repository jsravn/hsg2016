-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import           HW01
import           Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testRevDigits :: (Integer, [Integer]) -> Bool
testRevDigits (l, r) = toRevDigits l == r

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testRevDigits
             [(9, [9]), (1234, [4, 3, 2, 1]), (0, []),
              (421320, [0, 2, 3, 1, 2, 4])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (l, r) = doubleEveryOther l == r

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([0], [0]), ([4, 9, 5, 5], [4, 18, 5, 10]),
              ([2, 3, 5], [2, 6, 5])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (l, r) = sumDigits l == r

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10, 5, 18, 4], 19), ([], 0), ([5], 5)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (l, r) = luhn l == r

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [(Peg, Peg)]) -> Bool
testHanoi (a, b, c, d, e) = hanoi a b c d == e

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [
               (2, "a", "b", "c", [ ("a", "b"), ("a", "c"), ("b", "c") ]),
               (3, "a", "b", "c", [ ("a","c"),("a","b"),("c","b"),("a","c")
               ,("b","a"),("b","c"),("a","c") ])
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
