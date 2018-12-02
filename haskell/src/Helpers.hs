module Helpers
  ( readInput
  )
where

readInput :: String -> IO String
readInput day =
  readFile $ "../input/" ++ day ++ "Input"
