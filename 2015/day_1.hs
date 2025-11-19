import Data.List (findIndex)

solve :: String -> Int
solve = foldl (+) 0 . map step_parens

step_parens :: Char -> Int
step_parens '(' = 1
step_parens ')' = -1
step_parens _ = 0 

findbasement :: String -> Maybe Int
findbasement = findIndex (== -1) . scanl (+) 0 . map step_parens






main :: IO ()
main = do
  input <- readFile "../input/2015_1.txt" -- reading a file
  -- putStrLn input -- this prints to stdout and adds a newline
  putStrLn $ show $ solve input
  putStrLn $ show $ findbasement input