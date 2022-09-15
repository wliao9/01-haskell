
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Common where

import Data.IORef
import Data.List
import Data.Maybe
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Runners
import System.Exit
import Control.Exception
import Control.Monad

type Score = IORef (Double, Double)

runTests :: [Score -> TestTree] -> IO ()
runTests groups = do
  sc <- initScore
  defaultMainWithIngredients (includingOptions coreOptions : defaultIngredients) 
    (tests sc groups) `catch` (\(e :: ExitCode) -> do
      (n, tot) <- readIORef sc
      putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
      throwIO e)

tests :: Score -> [Score -> TestTree] -> TestTree
tests x gs = testGroup "Tests" [ g x | g <- gs ]

data TestType = PassTest | FailTest

data TestVector a b = TestVector 
  { tv_testfn   :: (a -> b) 
  , tv_testType :: TestType
  , tv_input    :: a 
  , tv_expected :: b 
  , tv_points   :: Double
  , tv_name     :: String
  }

data TestCandidate a b = TestCandidate
  { tc_name     :: String
  , tc_golden   :: (a -> b) 
  , tc_flawed   :: (a -> b) 
  , tc_testType :: TestType
  , tc_input    :: a
  , tc_expected :: b
  }

flipType :: TestType -> TestType 
flipType t = case t of
  PassTest -> FailTest
  FailTest -> PassTest

testTestCandidate :: forall a b. (Show b, Eq b) => String -> TestCandidate a b -> [TestVector a b] -> Double -> Score -> TestTree
testTestCandidate name TestCandidate{..} currentTests points sc = testGroup name
    [ testAll name (tests :: [TestVector a b]) points sc ]
    where
        replaceFn newFn xs = map (\TestVector{tv_testfn=oldFn,..} -> TestVector{tv_testfn=newFn, ..}) xs
        -- add tests to ensure
        -- 1. candFlawed fails the test candidate
        -- 2. candGolden passes the test candidate
        -- 3. candFlawed and candGolden pass the existing tests
        tests    :: [TestVector a b]
        tests      =  (TestVector tc_flawed (flipType tc_testType) tc_input tc_expected 0 "Flawed fail test")
                   :  (TestVector tc_flawed (flipType tc_testType) tc_input tc_expected 0 "Golden pass test")
                   :  (replaceFn tc_flawed currentTests)
                   ++ (replaceFn tc_golden currentTests)

--------------------------------------------------------------------------------
-- | Test and score each test in listTests, optionally scaling to maxPts
--------------------------------------------------------------------------------
-- tally local score for this list and allow proportional scoring
-- eg 10pts total, 3/4 pass = 7.5 score.
testEach :: (Show b, Eq b) => String -> Maybe Double -> [TestVector a b] -> Score -> TestTree
testEach testName maxPts tvs sc = 
  withResource 
    initScore         -- runs before tests
    (\score_ref -> do -- runs after tests
      (correct, total) <- readIORef score_ref
      -- scale score to maxPts or just add to sc
      let pts = maybe correct (\max -> (correct/total)*max) maxPts
      updateTotal sc $ fromMaybe total maxPts 
      updateCurrent sc $ pts
    )
    (\group_score ->  -- runs the tests
      testGroup testName $ map 
        (\tv@TestVector{..} -> 
           testCase tv_name $ do
             score_ref <- group_score
             mkTest score_ref tv)
        tvs
    )
  
--------------------------------------------------------------------------------
-- | Test and score all tests in listTests as one combined test.
--------------------------------------------------------------------------------
testAll :: (Show b, Eq b) => String -> [TestVector a b] -> Double -> Score -> TestTree
testAll name listTests points sc = scoreAllTests sc (listTests, points, name)

--------------------------------------------------------------------------------
-- | Construct a single compiler test case from a `Program`
--------------------------------------------------------------------------------
mkTest :: (Show b, Eq b) => Score -> TestVector a b -> IO ()
--------------------------------------------------------------------------------
mkTest sc vector = scoreTest sc vector

--------------------------------------------------------------------------------
scoreTest :: (Show b, Eq b) => Score -> TestVector a b -> IO ()
--------------------------------------------------------------------------------
scoreTest sc TestVector{..} = do
    updateTotal sc tv_points
    case tv_testType of
      PassTest -> if tv_testfn tv_input == tv_expected
                then updateCurrent sc tv_points
                else assertFailure "Wrong Result"
      FailTest -> if tv_testfn tv_input /= tv_expected
                then updateCurrent sc tv_points
                else assertFailure "Succeeded Unexpectedly"

scoreAllTests :: (Show b, Eq b) => Score -> ([TestVector a b], Double, String) -> TestTree
scoreAllTests sc (tests, points, name) = 
  testCase name $ do
    updateTotal sc points
    groupsc <- initScore
    forM_ tests (\TestVector{..} -> do
      updateTotal groupsc points
      case tv_testType of
        PassTest -> if tv_testfn tv_input == tv_expected
                  then updateCurrent groupsc tv_points
                  else return ()
        FailTest -> if tv_testfn tv_input /= tv_expected
                  then updateCurrent groupsc points
                  else return ())
    (correct, total) <- readIORef groupsc
    if correct == total
      then updateCurrent sc points
      else assertFailure "Not all tests passed"

flattenTestGroup :: [[TestTree]] -> [TestTree]
flattenTestGroup g = foldr (++) [] g

updateTotal :: Score -> Double -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Double -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0.0, 0.0)
