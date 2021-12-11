module Main where

main :: IO ()
main = do
    input <- readFile "input/one.txt"
    let dayOneInput = formatInput (read :: String->Int) input
    print $ dayOne dayOneInput
    print $ dayOnePartTwo dayOneInput

-- | applies 'f' to each line of our puzzle input, formating it for our solution function
formatInput :: (String -> a) -> String -> [a]
formatInput f input = map f (lines input)

-- |'dayOne' counts pairwise elements in our 'input' that are strictly increasing in value
dayOne :: (Ord a, Num b) => [a] -> b
dayOne xs = foldl (\count pair -> if (snd pair) > (fst pair) then count + 1 else count) 0 (zip xs (tail xs))

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : (partition n (tail xs))

-- |sums triplets in 'input' before counting consecutive pairs that are strictly increasing in value
dayOnePartTwo :: (Ord a, Num a, Num b) => [a] -> b
dayOnePartTwo input = dayOne $ map sum (partition 3 input)
