module HW05Tests where

import HW05
import Test.HUnit
import Parser
import qualified Data.Map.Strict as Map

flowTest :: Test
flowTest = TestCase (assertEqual ""
    (Map.fromList [ ("Haskell Curry", -10)
                  , ("Simon Peyton Jones", 10)
                  ])
    (getFlow  [ Transaction { from = "Haskell Curry"
                             , to = "Simon Peyton Jones"
                             , amount = 10
                             , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"
                             }
              ]))

runTests :: IO Counts
runTests = runTestTT $ TestList
    [ flowTest
    ]
