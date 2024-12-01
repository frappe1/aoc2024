import Data.List(sort)

part1 :: IO()
part1 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

part2 :: IO()
part2 = do
  contents <- readFile "input.txt"
  print $ part2solution contents

part1solution :: String -> Int
part1solution input = sum $ zipWith compareDist (sort list1) (sort list2)
    where 
        (list1, list2) = parse input
        compareDist a b = abs $ a - b

part2solution :: String -> Int
part2solution input = sum $ map (similarityScore list2) list1
    where 
        (list1, list2) = parse input
        similarityScore list x = length (filter (==x) list) * x

parse :: String -> ([Int], [Int])
parse input = (everyOther rawParse, everyOther (drop 1 rawParse))
    where rawParse = map read $ words input 

everyOther :: [Int] -> [Int]
everyOther (x:y:xs) = x : everyOther xs
everyOther xs = xs