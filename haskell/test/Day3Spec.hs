module Day3Spec
  ( correctOverlappingClaimArea
  , correctNonOverlappingClaimId
  )
where

import           Flow
import           Test.Tasty
import           Test.Tasty.HUnit

import           Day3

correctOverlappingClaimArea :: TestTree
correctOverlappingClaimArea =
  testGroup "Testing overlappingClaimArea for sample claims"
    $ [ testCase "overlapping claim count is 4" $ 4 @?= (subject input)
      ]
 where
  subject = parseInput .> overlappingClaimArea
  input = "#1 @ 1,3: 4x4\n\
          \#2 @ 3,1: 4x4\n\
          \#3 @ 5,5: 2x2"

correctNonOverlappingClaimId :: TestTree
correctNonOverlappingClaimId =
  testGroup "Testing nonOverlappingClaimId for sample claims"
    $ [ testCase "overlapping claim count is 3" $ 3 @?= (subject input)
      ]
 where
  subject = parseInput .> nonOverlappingClaimId
  input = "#1 @ 1,3: 4x4\n\
          \#2 @ 3,1: 4x4\n\
          \#3 @ 5,5: 2x2"
