import           Day1
import           Helpers

import Data.Maybe

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day1"

  putStrLn $ show $ resultingFrequency input
  putStrLn $ fromMaybe "Not found" $ fmap show $ repeatingFrequency input
