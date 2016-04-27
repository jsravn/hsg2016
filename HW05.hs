{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Control.Applicative
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret pathA pathB = liftA (BS.pack . filter (/= 0))
    $ liftA2 (BS.zipWith xor) (BS.readFile pathA) (BS.readFile pathB)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    let encPath = path ++ ".enc"
    encBs <- BS.readFile encPath
    let decBs = BS.pack $ BS.zipWith xor (BS.cycle key) encBs
    BS.writeFile path decBs

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = liftA decode . BS.readFile

-- Exercise 4 -----------------------------------------

filterTIds :: [TId] -> [Transaction] -> [Transaction]
filterTIds tids = filter ((`elem` tids) . tid)

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs pathA pathB = do
    victims <- parseFile pathA
    transactions <- parseFile pathB
    return $ liftA2 filterTIds victims transactions

-- Exercise 5 -----------------------------------------

insertT :: Transaction -> Map String Integer -> Map String Integer
insertT t = Map.insert (to t) (amount t) . Map.insert (from t) (-1 * amount t)

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr insertT Map.empty

-- Exercise 6 -----------------------------------------

reverseMap :: Map String Integer -> Map Integer String
reverseMap = Map.foldrWithKey (flip Map.insert) Map.empty


getCriminal :: Map String Integer -> String
getCriminal = snd . Map.findMax . reverseMap

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

