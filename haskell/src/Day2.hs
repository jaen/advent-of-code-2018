{-# LANGUAGE LambdaCase #-}

module Day2
  ( checksum
  , commonIdCharacters
  , parseInput
  )
where

import           Flow
import           Data.List
import           Data.Bifunctor
import           Data.Biapplicative
import qualified Data.Set as S

-- solution

checksum :: [String] -> Int
checksum ids =
    countDoubles * countTriples
  where
    (countDoubles, countTriples) = counts

    counts :: (Int, Int)
    counts =
      ids
        |> map countRepetitions
        |> foldl sumPairs (0, 0)

    sumPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
    sumPairs x =
      ((bimap (+) (+) x) <<*>>)

    countRepetitions :: String -> (Int, Int)
    countRepetitions id =
        (doubles, triples)
      where
        grouped = map length $ group $ sort id
        doubles = if any (== 2) grouped then 1 else 0
        triples = if any (== 3) grouped then 1 else 0

commonIdCharacters :: [String] -> String
commonIdCharacters ids =
    commonCharacters
  where
    commonCharacters = filter (flip S.member $ otherChars) one
      where
        otherChars = S.fromList other


    (one, other) =
      head [ (one, other) | one <- ids
                          , other <- ids
                          , diffCount one other == 1 ]

    diffCount :: String -> String -> Int
    diffCount xs ys =
      (zip xs ys)
        |> map pairDiffs
        |> sum

    pairDiffs :: (Char, Char) -> Int
    pairDiffs = ((uncurry (/=)) .> (\case True -> 1; False -> 0))

-- parsing

parseInput :: String -> [String]
parseInput =
  lines

