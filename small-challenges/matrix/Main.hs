row :: String -> Int -> [Int]
row input index = parseMatrix input !! (index - 1)

column :: String -> Int -> [Int]
column input index = map (!! (index - 1)) $ parseMatrix input

parseMatrix :: String -> [[Int]]
parseMatrix input = map (map (read :: String -> Int) . words) (lines input)