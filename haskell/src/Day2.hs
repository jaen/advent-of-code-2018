module Day2
  ( checksum
  , parseInput
  )
where

import           Flow
import           Data.List

-- solution

checksum :: [String] -> Int
checksum ids = countDoubles * countTriples
  where
    (countDoubles, countTriples) = counts

    counts :: (Int, Int)
    counts = ids
              |> map countRepetitions
              |> foldl (\(accd, acct) (d, t) -> (accd + d, acct + t))
                       (0, 0)

    countRepetitions :: String -> (Int, Int)
    countRepetitions id = (doubles, triples)
      where
        grouped = map length $ group $ sort id
        doubles = if any (== 2) grouped then 1 else 0
        triples = if any (== 3) grouped then 1 else 0

-- parsing

parseInput :: String -> [String]
parseInput =
  lines

