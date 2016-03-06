-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module HW02Tests where

import           HW02
import           Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3) ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue],
               Move [Red, Orange, Orange, Blue] 1 2)
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "filterCodes test" filterCodes
             [ (Move [Red, Red, Blue, Green] 1 1,
               [[Red, Blue, Yellow, Purple], [Red, Blue, Red, Purple],
                [Blue, Green, Blue, Purple]],
               [[Red, Blue, Yellow, Purple], [Blue, Green, Blue, Purple]])
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF1 "allCodes test" allCodes
             [ (1, [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]])]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = [ testF1 "solve test" solve
             [ ([Blue, Green, Blue],
                [Move [Red,Red,Red] 0 0,
                 Move [Green,Green,Green] 1 0,
                 Move [Green,Blue,Blue] 1 2,
                 Move [Blue,Green,Blue] 3 0]) ,
               ([Blue, Blue, Blue],
                [Move [Red,Red,Red] 0 0,
                 Move [Green,Green,Green] 0 0,
                 Move [Blue,Blue,Blue] 3 0])]
           ]

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = [ testF1 "solve test" fiveGuess
             [ ([Red, Red, Green, Blue]
             , [Move [Red,Red,Green,Green] 3 0,Move [Red,Green,Green,Blue] 3 0
             , Move [Red,Red,Green,Blue] 4 0]),
               ([Green, Red, Green, Blue]
             , [Move [Red,Red,Green,Green] 2 1,Move [Red,Green,Green,Blue] 2 2
             , Move [Red,Green,Blue,Green] 0 4,Move [Green,Red,Green,Blue] 4 0])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , bonusTests
                  ]
