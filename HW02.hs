{-# OPTIONS_GHC -Wall #-}
module HW02 where

import           Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a  = length . filter (uncurry (==)) . zip a

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map countColor colors
  where countColor color = length . elemIndices color $ code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum . map (uncurry min) $ zip (countColors a) (countColors b)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
  where exact = exactMatches secret guess
        nonExact = matches secret guess - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move guess _ _) secret = m == newMove
  where newMove = getMove secret guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

addColor :: [Code] -> Peg -> [Code]
addColor [] peg = [[peg]]
addColor codes peg = map (peg:) codes

allCodes :: Int -> [Code]
allCodes n
  | n < 1     = []
  | otherwise = concatMap (addColor prevCodes) colors
  where prevCodes = allCodes (n - 1)
  
-- Exercise 7 -----------------------------------------

solved :: Move -> Bool
solved (Move g m _) = length g == m

allMoves :: Code -> [Move]
allMoves secret = map toMove $ allCodes (length secret)
  where toMove = getMove secret

isConsistentMove :: Move -> Move -> Bool
isConsistentMove a (Move g _ _) = isConsistent a g

consistentMoves :: [Move] -> [Move]
consistentMoves []       =  []
consistentMoves (x:xs)   = x : consistentMoves filteredXs
  where filteredXs = filter (isConsistentMove x) xs

allConsistentMoves :: Code -> [Move]
allConsistentMoves = consistentMoves . allMoves

takeUntil               :: (a -> Bool) -> [a] -> [a]
takeUntil _ []          =  []
takeUntil p (x:xs)
            | p x       =  [x]
            | otherwise =  x : takeUntil p xs

solve :: Code -> [Move]
solve code = takeUntil solved (allConsistentMoves code)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
