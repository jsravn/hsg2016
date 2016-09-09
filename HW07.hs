{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m1 = m1 >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV idxA idxB v = liftM2 swapPure (v !? idxA) (v !? idxB)
  where
    swapPure a b = v // [(idxA, b), (idxB, a)]
--swapV idxA idxB v = do
--    a <- v !? idxA
--    b <- v !? idxB
--    return $ v // [(idxA, b), (idxB, a)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM mf as = sequence $ map mf as

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices v = mapM (v !?) indices

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  let l = V.length v
  r <- getRandomR (0, l)
  return $ v !? r

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = V.replicateM n $ getRandomR r

-- Exercise 5 -----------------------------------------

swapRnd :: Vector a -> Int -> Rnd (Vector a)
swapRnd v i = do
  j <- getRandomR (0, i)
  return $ v // [(i, v ! j), (j, v ! i)]

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = do
  let l = V.length v
  let indices = V.enumFromStepN (l - 1) (-1) (l - 2)
  V.foldM swapRnd v indices

-- Exercise 6 -----------------------------------------

partIt :: Ord a => a -> (Vector a, a, Vector a) -> (Vector a, a, Vector a)
partIt val (v1, pivotVal, v2)
  | val < pivotVal = (V.cons val v1, pivotVal, v2)
  | otherwise      = (v1, pivotVal, V.cons val v2)

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v pivot = V.foldr partIt initAcc v'
  where
    initAcc = (V.fromList [], v ! pivot, V.fromList [])
    v' = (V.++) (V.take pivot v) (V.drop (pivot + 1) v)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | v == V.empty = V.empty
  | otherwise =
    let x = V.head v
        xs = V.tail v
    in qsort [ y | y <- xs, y < x ]
       <> (x `cons` qsort [ y | y <- xs, y >= x ])


-- Exercise 8 -----------------------------------------

combineIt :: Rnd (Vector a) -> a -> Rnd (Vector a) -> Rnd (Vector a)
combineIt rndL val rndR = do
  l <- rndL
  r <- rndR
  return $ l <> (val `cons` r)

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | v == V.empty = return V.empty
  | otherwise = do
    let length = V.length v
    pivot <- getRandomR (0, length - 1)
    let (l, pv, r) = partitionAt v pivot
    combineIt (qsortR l) pv (qsortR r)

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select rank v
  | rank >= V.length v = return Nothing
  | otherwise = do
    pivot <- getRandomR (0, V.length v - 1)
    let (l, pv, r) = partitionAt v pivot
    let lengthL = V.length l
    if rank < lengthL then select rank l
    else if rank > lengthL then select (rank - lengthL - 1) r
    else return $ Just pv

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
