import qualified Data.Text as T
import MyUtil (debug, replaceOrInsertElem)
import RIO (trace)
import Test.QuickCheck.Property ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testGroup
        "replaceOrInsertElem"
        [ testCase "can replace item" $ (replaceOrInsertElem 22 (== 2) [1, 2, 3] == [1, 22, 3]) @?= True,
          testCase "should append unmatched item" $ (replaceOrInsertElem 7 (== 7) [1, 2, 3] == [1, 2, 3, 7]) @?= True
        ]
    ]