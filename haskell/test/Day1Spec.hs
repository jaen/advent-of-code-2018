module Day1Spec
  ( correctFrequency
  , correctRepeatingFrequency
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Day1

spaceToNewline :: Char -> Char
spaceToNewline ' ' = '\n'
spaceToNewline c   = c

correctFrequency :: TestTree
correctFrequency =
  testGroup "Testing repeatingFrequency for sample frequencies"
    $ [ testCase "+1 +1 -2 =  0" $  0 @?= (subject "+1 +1 -2")
      , testCase "+1 +1 +1 =  3" $  3 @?= (subject "+1 +1 +1")
      , testCase "-1 -2 -3 = -6" $ -6 @?= (subject "-1 -2 -3")
      ]
 where
  subject = resultingFrequency . parseInput . map spaceToNewline

correctRepeatingFrequency :: TestTree
correctRepeatingFrequency =
  testGroup "Testing repeatingFrequency for sample frequencies"
    $ [ testCase "+1 -1          =  0" $ (Just  0) @?= (subject "+1 -1")
      , testCase "+3 +3 +4 -2 -4 = 10" $ (Just 10) @?= (subject "+3 +3 +4 -2 -4")
      , testCase "-6 +3 +8 +5 -6 =  5" $ (Just  5) @?= (subject "-6 +3 +8 +5 -6")
      , testCase "+7 +7 -2 -7 -4 = 14" $ (Just 14) @?= (subject "+7 +7 -2 -7 -4")
      ]
 where
  subject = repeatingFrequency . parseInput . map spaceToNewline
