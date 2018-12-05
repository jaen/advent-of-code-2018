import           Test.Tasty
import           Test.Tasty.HUnit

import           Day1Spec
import           Day2Spec
import           Day3Spec

main :: IO ()
main = do
  defaultMain $
    testGroup "Advent of Code 2018 Tests"
      [ (testGroup "Day1 Tests" [ correctFrequency, correctRepeatingFrequency ])
      , (testGroup "Day2 Tests" [ correctChecksum, correctCommonIdCharacters ])
      , (testGroup "Day3 Tests" [ correctOverlappingClaimArea, correctNonOverlappingClaimId ]) ]
