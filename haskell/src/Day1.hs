{-# LANGUAGE ScopedTypeVariables #-}

module Day1
  ( resultingFrequency
  , repeatingFrequency
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
  foldl (+) 0

intermediateFrequencies :: (Num a, Eq a, Ord a) => [a] -> [a]
intermediateFrequencies =
  cycle .> scanl (+) 0

repeatingFrequency :: (Num a, Eq a, Ord a) => [a] -> Maybe a
repeatingFrequency =
    intermediateFrequencies .> findFrequency S.empty
  where
    -- findFrequency :: S.Set a -> [a] -> Maybe a
    findFrequency _    []     = Nothing
    findFrequency seen (x:xs)
      | S.member  x seen = Just x
      | otherwise        = findFrequency (S.insert x seen) xs

-- parsing

type Parser = P.Parsec Void String

inputParser :: Parser [Int]
inputParser =
    number `AC.sepBy` C.newline
  where
    number = positiveNumber <|> negativeNumber

    positiveNumber = C.char '+' >> AC.many C.digitChar >>= return . read
    negativeNumber = C.char '-' >> AC.many C.digitChar >>= return . negate . read

parseInput :: String -> [Int]
parseInput =
  P.parse inputParser "" .> E.fromRight []

