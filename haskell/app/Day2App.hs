import           Day2
import           Helpers

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day2"

  putStrLn $ show $ checksum input
