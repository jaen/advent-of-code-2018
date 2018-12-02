import           Test.Tasty
import           Test.Tasty.HUnit

import           Day1Spec

main :: IO ()
main = do
  defaultMain $
    testGroup "Advent of Code 2018 Tests"
      [(testGroup "Day1 Tests" [correctFrequency, correctRepeatingFrequency])]
