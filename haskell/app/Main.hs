module Main where

main :: IO ()
main = do
    input <- readFile "input/one.txt"
    let dayOneInput = map (read :: String->Int) (lines input) 
    print $ dayOne dayOneInput
    print $ dayOnePartTwo dayOneInput

-- |'dayOne' counts pairwise elements in our 'input' that are strictly increasing in value
dayOne :: (Ord b, Num a) => [b] -> a
dayOne xs = foldl (\x y -> if (snd y) > (fst y) then x + 1 else x) 0 (zip xs (tail xs))

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : (partition n (tail xs))

-- |sums triplets in 'input' before counting consecutive pairs that are strictly increasing in value
dayOnePartTwo :: (Ord b, Num b, Num a) => [b] -> a
dayOnePartTwo input = dayOne $ map sum (partition 3 input)
