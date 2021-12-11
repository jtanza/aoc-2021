module Main where

main :: IO ()
main = do
    input <- readFile "input/two.txt"
    print $ dayTwo $ formatInput id input

-- |Applies 'f' to each line of our puzzle input, formating it for our solution function
formatInput :: (String -> a) -> String -> [a]
formatInput f input = map f (lines input)

-- |'dayOne' counts pairwise elements in our 'input' that are strictly increasing in value
dayOne :: (Ord a, Num b) => [a] -> b
dayOne xs = foldl (\count pair -> if (snd pair) > (fst pair) then count + 1 else count) 0 (zip xs (tail xs))

-- |Sums triplets in 'input' before counting consecutive pairs that are strictly increasing in value
dayOnePartTwo :: (Ord a, Num a, Num b) => [a] -> b
dayOnePartTwo input = dayOne $ map sum (partition 3 input)

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : (partition n (tail xs))

-- |Given a series of commands of the form '(direction, magnitude)' returns the product of both after applying
-- all commands in the sequence to the starting position '(0,0)'
dayTwo :: (Num a, Read a) => [String] -> a
dayTwo input = horizontalPos * depth
  where (horizontalPos, depth) = foldl (\coord command -> parseCommand command coord) (0,0) $ map words input

parseCommand :: (Num a, Num b, Read a, Read b) => [String] -> (a, b) -> (a, b)
parseCommand xs coord  = case xs of
                  ("forward":n:_) -> (x  + read n, y)
                  ("down":n:_)    -> (x, (y + read n))
                  ("up":n:_)      -> (x, (y - read n))
  where (x, y) = coord
  
