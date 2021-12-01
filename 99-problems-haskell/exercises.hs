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

-- 11) Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists
data ListElem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListElem a]
encodeModified xs = map encoder $ encode xs
    where   encoder (1, a) = Single a
            encoder (n, a) = Multiple n a

-- 12) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version
decodeModified :: [ListElem a] -> [a]
decodeModified = concatMap decoder
    where   decoder (Single a) = [a]
            decoder (Multiple n a) = replicate n a

-- 13) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. 
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, 
-- but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X

-- 14) Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- 15) Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16) Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n n
    where   dropEvery' [] _ _ = []
            dropEvery' (x:xs) n 1 = dropEvery' xs n n
            dropEvery' (x:xs) n c = x : dropEvery' xs n (c - 1)

-- 17) Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a], [a])
split (x:xs) n
    | n > 0 =   let (fp, l) = split xs (n - 1) 
                in (x : fp, l)
split xs _  =   ([], xs)

-- 18) Extract a slice from a list
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element 
-- of the original list (both limits included). Start counting the elements with 1
slice :: [a] -> Int -> Int -> [a]
slice xs s e = drop (s - 1) $ take e xs

-- 19) Rotate a list N places to the left
-- Hint: Use the predefined functions length and (++)
rotate :: [a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
    where n' = n `mod` length xs

-- 20) Remove the K'th element from a list
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs | n > 0 = (Just (xs !! (n - 1)), take (n - 1) xs ++ drop n xs)
removeAt _ xs = (Nothing, xs)