module Day2Spec
  ( correctChecksum
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Day2

correctChecksum :: TestTree
correctChecksum =
  testGroup "Testing repeatingFrequency for sample frequencies"
    $ [ testCase "checksum of input is 12" $ 12 @?= (subject input)
      ]
 where
  subject = checksum
  input = [ "abcdef"
          , "bababc"
          , "abbcde"
          , "abcccd"
          , "aabcdd"
          , "abcdee"
          , "ababab" ]
