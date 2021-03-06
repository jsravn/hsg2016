{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 =  1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a rest) = a : streamToList rest

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a $ sRepeat a

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a . sIterate f $ f a

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a rest) next = Cons a $ sInterleave next rest

sTake :: Int -> Stream a -> [a]
sTake n (Cons a rest)
  | n <= 0    = []
  | otherwise = a : sTake (n - 1) rest

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+ 1) 0

ruler :: Stream Integer
ruler = sInterleave zeros twos
  where
    zeros = sRepeat 0
    twos = sInterleave (sRepeat 1) (sIterate (+1) 2)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate lcg
  where
    lcg prev = (1103515245 * prev + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 238 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

updateMinMax :: Int -> Maybe (Int, Int) -> Maybe (Int, Int)
updateMinMax val Nothing = Just (val, val)
updateMinMax val (Just (!minVal, !maxVal)) = Just (min val minVal, max val maxVal)

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = foldl' (flip updateMinMax) Nothing

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
