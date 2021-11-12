-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
data Number =
    Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    deriving (Enum, Show)

englishDigit :: Int -> String
englishDigit x = if x `elem` digits then show (toEnum x :: Number) else "unknown"
    where digits = [0..9]

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = 0.0
divTuple (x, y) = x / y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList _ = False