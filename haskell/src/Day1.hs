module Day1
  ( resultingFrequency
  , parseInput
  )
where

import           Flow
import qualified Text.Megaparsec                 as P
import           Control.Applicative.Combinators as AC
import           Text.Megaparsec.Char            as C
import           Data.Either                     as E
import qualified Data.Set                        as S
import           Data.Maybe
import           Data.Void

-- solution

resultingFrequency :: (Num a, Eq a) => [a] -> a
resultingFrequency =
  foldr (+) 0

-- parsing

type Parser = P.Parsec Void String

inputParser :: Parser [Int]
inputParser =
    number `AC.sepBy` C.newline
  where
    number         = positiveNumber <|> negativeNumber

    positiveNumber = C.char '+' >> AC.many C.digitChar >>= return . read
    negativeNumber = C.char '-' >> AC.many C.digitChar >>= return . negate . read

parseInput :: String -> [Int]
parseInput =
  P.parse inputParser "" .> E.fromRight []

