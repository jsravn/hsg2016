{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P as) (P bs) = trim as == trim bs
        where
            trim = reverse . dropWhile (== 0) . reverse

-- Exercise 3 -----------------------------------------

showTerm :: (Num a, Eq a, Show a) => Int -> a -> String
showTerm ex coeff
    | coeff == 0     = ""
    | abs coeff == 1 = show1 ex coeff
    | otherwise      = show coeff ++ showX ex
    where
        show1 e c
            | e == 0    = show c
            | c == -1   = "-" ++ showX e
            | otherwise = showX e

showX :: Int -> String
showX ex
    | ex == 0   = ""
    | ex == 1   = "x"
    | otherwise = "x^" ++ show ex

showTerms :: (Num a, Eq a, Show a) => Poly a -> [String]
showTerms (P as) = filter (/= "") . zipWith showTerm [0..] $ as

concatTerms :: [String] -> String
concatTerms = replaceEmptyWith0 . concat . reverse . intersperse " + "
    where
        replaceEmptyWith0 s
            | s == ""   = "0"
            | otherwise = s

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = concatTerms . showTerms

-- Exercise 4 -----------------------------------------

addLists :: Num a => [a] -> [a] -> [a]
addLists as [] = as
addLists [] bs = bs
addLists (a:as) (b:bs) = a + b : addLists as bs

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P as) (P bs) = P $ addLists as bs

-- Exercise 5 -----------------------------------------

multLists :: Num a => [a] -> [a] -> [[a]]
multLists [] _      = []
multLists (a:as) bs = map (*a) bs : multLists as (0:bs)

sumPolys :: Num a => [Poly a] -> Poly a
sumPolys = foldr (+) (P [0])

times :: Num a => Poly a -> Poly a -> Poly a
times (P as) (P bs) = sumPolys . map P $ multLists as bs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = (* P [-1])
    fromInteger = P . (:[]) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

