import System.IO
import Control.Arrow

zipSkipping :: Int -> [Int] -> ([Int], [Int])
zipSkipping = (&&&) id . drop

(.:) = (.) . (.)

solve :: Int -> [Int] -> Int
solve = (length . filter id . uncurry (zipWith (<))) .: zipSkipping

part1 = solve 1
part2 = solve 3

main :: IO ()
main = (print . (part1 &&& part2) . map read . lines) =<< getContents

