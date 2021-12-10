module Main where

main :: IO ()
main = do
    input <- readFile "input/one.txt"
    print $ dayOne input

-- |'dayOne' counts pairwise elements in our `input` that are strictly increasing in value
dayOne :: Num a => [Char] -> a
dayOne input = foldl (\x y -> if (snd y) > (fst y) then x + 1 else x) 0 (zip xs (tail xs))
  where xs = map (read :: String->Int) (lines input)



