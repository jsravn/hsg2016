{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [fromInteger 0, fromInteger 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P as) (P bs) = trim as == trim bs
        where
            trim = reverse . dropWhile (== 0) . reverse

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P as) = emptyWith0 . concat . reverse . intersperse (" + ") . filter (/= "") . map showA . zip [0..] $ as
        where
            showA (i, a)
                | a == fromInteger 0 = ""
                | a == fromInteger 1
                    && i == 0        = "1"
                | a == fromInteger 1 = coeff i
                | otherwise          = show a ++ coeff i
            coeff :: Int -> String
            coeff i
                | i == 0    = ""
                | i == 1    = "x"
                | otherwise = "x^" ++ show i
            emptyWith0 s
                | s == ""   = "0"
                | otherwise = s

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
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

