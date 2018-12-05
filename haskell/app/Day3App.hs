import           Day3
import           Helpers

main :: IO ()
main = do
  input <- parseInput <$> readInput "Day3"

  putStrLn $ show $ overlappingClaimArea input
  putStrLn $ show $ nonOverlappingClaimId input


