module Day2Spec
  ( correctChecksum
  , correctCommonIdCharacters
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

correctCommonIdCharacters :: TestTree
correctCommonIdCharacters =
    testGroup "Testing commonIdCharacters for sample frequencies"
      $ [ testCase "common characters of input are " $ "fgij" @?= (subject input)
        ]
   where
    subject = commonIdCharacters
    input = [ "abcde"
            , "fghij"
            , "klmno"
            , "pqrst"
            , "fguij"
            , "axcye"
            , "wvxyz" ]
