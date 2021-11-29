-- 1) Find the last element of a list
myLast :: [a] -> a
myLast [] = error "must pass a non empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2) Find the last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "must pass a non empty list"
myButLast [x] = error "must pass at least 2 elements"
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs

-- 3) Find the K'th element of a list. The first element in the list is number 1
elementAt :: [a] -> Int -> a
elementAt [] _ = error "must pass a non empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) k
    | k < 1 = error "k must be greater than zero"
    | otherwise = elementAt xs (k - 1)

-- 4) Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5) Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x)
isPalindrome :: (Ord a ) => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- 7) Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- 8) Eliminate consecutive duplicates of list elementss
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = x : compress xs
    | otherwise = x : compress (y:xs)

-- 9) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (f, r) = span (==x) xs
                in (x:f) : pack r

-- 10) Run-length encoding of a list. Use the result of problem P09 to implement the so-called 
-- run-length encoding data compression method. Consecutive duplicates of elements are encoded
-- as lists (N E) where N is the number of duplicates of the element E
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = [(length x, head x) | x <- pack xs]