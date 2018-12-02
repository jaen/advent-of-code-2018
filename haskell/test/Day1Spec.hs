module Day1Spec
  ( correctFrequency
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Day1

correctFrequency :: TestTree
correctFrequency =
  testGroup "Testing resultingFrequency for sample frequencies"
    $ [ testCase "+1 +1 -2 =  0" $ 0 @?= (subject "+1 +1 -2")
      , testCase "+1 +1 +1 =  3" $ 3 @?= (subject "+1 +1 +1")
      , testCase "-1 -2 -3 = -6" $ -6 @?= (subject "-1 -2 -3")
      ]
 where
  subject = resultingFrequency . parseInput . map spaceToNewline
  spaceToNewline ' ' = '\n'
  spaceToNewline c   = c
