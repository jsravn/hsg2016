module HW05Tests where

import HW05
import Test.HUnit
import Parser
import qualified Data.Map.Strict as Map

flowTest :: Test
flowTest = TestCase (assertEqual ""
    (Map.fromList [ ("Haskell Curry", -5)
                  , ("Simon Peyton Jones", 5)
                  ])
    (getFlow  [ Transaction { from = "Haskell Curry"
                             , to = "Simon Peyton Jones"
                             , amount = 10
                             , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"
                             }
              , Transaction { from = "Simon Peyton Jones"
                             , to = "Haskell Curry"
                             , amount = 5
                             , tid = "9d74ae20-7e37-40e8-ad87-628d40f366e4"
                             }
              ]))

criminalTest :: Test
criminalTest = TestCase (assertEqual ""
    "Simon Peyton Jones"
    (getCriminal $ Map.fromList [ ("Haskell Curry", -10)
                                , ("Simon Peyton Jones", 10)
                                ]))

runTests :: IO Counts
runTests = runTestTT $ TestList
    [ flowTest
    , criminalTest
    ]
