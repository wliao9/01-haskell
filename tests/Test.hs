{-# LANGUAGE RankNTypes #-}
import Common
import Hw1 
import Hw1B
import Test.Tasty
import Common
import Data.List

main :: IO ()
main = runTests [ sumListGroup
                , digitsOfIntGroup
                , digitsGroup
                , additivePersistenceGroup
                , digitalRootGroup
                , reverseGroup
                , palindromeGroup
                ]
-- Points for each group are defined in README and should be 
-- kept in sync. Points for individual tests indicate their relative 
-- value to other tests in the group. The total number of passing 
-- tests divided by the total possible (individual) points is used 
-- to scale the group points awarded.
sumListGroup :: Score -> TestTree
sumListGroup score_ref = testGroup "sumList Tests" 
  [ testEach "sumList" (Just 10) (sumListTests sumList) score_ref ]

sumListTests :: ([Int] -> Int) -> [TestVector [Int] Int]
sumListTests testfn =
  [ TestVector testfn PassTest [] 0                   1 "sumList 1"
  , TestVector testfn PassTest [1, 2, 3, 4] 10        1 "sumList 2"
  , TestVector testfn PassTest [1, -2, 3, 5] 7        1 "sumList 3"
  , TestVector testfn FailTest [1, 3, 5, 7, 9, 11] 35 1 "sumList 4"
  ]

digitsOfIntGroup :: Score -> TestTree
digitsOfIntGroup score_ref = testGroup "digitsOfInt Tests" 
  [ testEach "digitsOfInt" (Just 8) (digitsOfIntTests digitsOfInt) score_ref ]

digitsOfIntTests :: (Int -> [Int]) -> [TestVector Int [Int]]
digitsOfIntTests testfn = 
  [ TestVector testfn PassTest 3124 [3,1,2,4]            1 "digitsOfInt 1"
  , TestVector testfn PassTest 352663 [3, 5, 2, 6, 6, 3] 1 "digitsOfInt 2"
  ]
  
digitsGroup :: Score -> TestTree
digitsGroup score_ref = testGroup "digits Tests" 
  [ testEach "digits" (Just 2) (digitsTests digits) score_ref ]

digitsTests :: (Int -> [Int]) -> [TestVector Int [Int]]
digitsTests testfn = 
  [ TestVector testfn PassTest 31243 [3, 1, 2, 4, 3]    1 "digits 1"
  , TestVector testfn PassTest (-23422) [2, 3, 4, 2, 2] 1 "digits 2"
  ]

additivePersistenceGroup :: Score -> TestTree
additivePersistenceGroup score_ref = testGroup "additivePersistence Tests" 
  [ testEach "additivePersistence" (Just 10) (additivePersistenceTests additivePersistence) score_ref ]

additivePersistenceTests :: (Int -> Int) -> [TestVector Int Int]
additivePersistenceTests testfn = 
  [ TestVector testfn PassTest 9876 2 1 "additivePersistence 1"
  , TestVector testfn PassTest 1 0    1 "additivePersistence 2"
  ]

digitalRootGroup :: Score -> TestTree
digitalRootGroup score_ref = testGroup "digitalRoot Tests" 
  [ testEach "digitalRoot" (Just 10) (digitalRootTests digitalRoot) score_ref ]

digitalRootTests :: (Int -> Int) -> [TestVector Int Int]
digitalRootTests testfn = 
  [ TestVector testfn PassTest 9876 3 1 "digitalRoot 1"
  , TestVector testfn PassTest 1 1    1 "digitalRoot 2"
  ]

reverseGroup :: Score -> TestTree
reverseGroup score_ref = testGroup "reverse Tests" 
 ( [ testEach "reverseInt" (Just 7.5) (reverseIntTests reverse) score_ref ]
  ++ [ testEach "reverseStr" (Just 7.5) (reverseStrTests reverse) score_ref ])

reverseIntTests :: ([Int] -> [Int]) -> [TestVector [Int] [Int]]
reverseIntTests testfn = [ TestVector testfn PassTest [1, 2, 3, 4] [4, 3, 2, 1] 1 "reverse 1" ]

reverseStrTests :: ([String] -> [String]) -> [TestVector [String] [String]]
reverseStrTests testfn = [ TestVector testfn PassTest ["a", "b", "c", "d"] ["d", "c", "b", "a"] 1 "reverse 2" ]

palindromeGroup :: Score -> TestTree
palindromeGroup score_ref = testGroup "palindrome Tests" 
  [ testEach "palindrome" (Just 10) (palindromeTests palindrome) score_ref ]

palindromeTests :: (String -> Bool) -> [TestVector String Bool]
palindromeTests testfn = 
  [ TestVector testfn PassTest "malayalam" True    1 "palindrome 1"
  , TestVector testfn PassTest "myxomatosis" False 1 "palindrome 2"
  ]
