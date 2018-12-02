import           Day1
import           Helpers

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day1"

  putStrLn $ show $ resultingFrequency input
