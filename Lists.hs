
-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (hd:tl) = myLast tl

-- Problem 2
myButLast :: [a] -> a
myButLast l = (reverse l) !! 1

-- Problem 3
elementAt :: [a] -> Integer -> a
elementAt (x:_) 1 = x
elementAt [] n    = error("Index out of bounds")
elementAt (x:xs) n
  | n <= 0        = error("Index 0 or Negative")
  | otherwise     = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Integer
myLength l = foldl (\x y -> x + 1) 0 l

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = last l : myReverse (init l)

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == (reverse l)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a)      = [a]
myFlatten (List n)      = foldl (\l nl -> l ++ (myFlatten nl)) [] n

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress l = foldl (\l x -> if x /= last l then l ++ [x] else l) [head l] (tail l)

-- Problem 9
countDuplicatesAtStart :: Eq a => [a] -> Int
countDuplicatesAtStart [] = 0
countDuplicatesAtStart [x] = 1
countDuplicatesAtStart l  = if head l == (l !! 1) then 1 + countDuplicatesAtStart (tail l) else 1

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l  = let duplicates = countDuplicatesAtStart l in (take duplicates l) : pack (drop duplicates l)

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode l = map (\l -> (length l, head l)) (pack l)

-- Problem 11
data OneOrMany a = One a | Many (Int, a) deriving (Show)
encodeModified :: Eq a => [a] -> [OneOrMany a]
encodeModified [] = []
encodeModified l = map (\x -> if fst x == 1 then One (snd(x)) else Many x) (encode l)

-- Problem 12
decodeModified :: [OneOrMany a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of One x      -> x : decodeModified xs
                                  Many (n,x) -> take n (repeat x) ++ decodeModified xs

-- Problem 13
encodeDirect :: Eq a => [a] -> [OneOrMany a]
encodeDirect [] = []
encodeDirect l@(x:xs) = let duplicates = countDuplicatesAtStart l in
                        (if duplicates == 1 then One x else Many (duplicates, x)) : encodeDirect (drop duplicates l)

-- Problem 14
dupli :: [a] -> [a]
dupli l = concatMap (\x -> [x,x]) l

-- Problem 15
repli :: [a] -> Int -> [a]
repli l n = concatMap (\x -> n `take` repeat x) l

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = map snd (filter (\x -> fst x `mod` n /= 0) (zip [1..] l))

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split l 0 = ([], l)
split (x:xs) n = let splitTail = split (xs) (n-1) in ((x: fst splitTail), (snd splitTail))

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice l i k = take (k-i-1) (drop (i-1) l)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate l n = take (length l) (drop n (cycle l))

-- Problem 20
removeAt :: Int -> [a] -> [a]
removeAt 1 l = tail l
removeAt n l
  | n <= 0    = error("Element to remove must be greater than 0")
  | null l    = error("Index out of bounds")
  | otherwise = head l : removeAt (n-1) (tail l)

-- Problem 21
insertAt :: a ->[a] -> Int -> [a]
insertAt x l n = (take (n-1) l) ++ [x] ++ (drop (n-1) l)

-- Problem 22
range :: Int -> Int -> [Int]
range n m
  | n < 0 || m < 0 = error("Won't work for negative numbers")
  | n > m          = error("Size of range is negative")
  | otherwise      = take (m-n+1) (drop n [0..])

-- Problems 23,24,25 require random number generation, which is a bit of a pain in Haskell.

-- Problem 26
listify :: [a] -> [[a]]
listify [] = []
listify (x:xs) = [x] : (listify xs)

combinations :: Int -> [a] -> [[a]]
combinations n [] = []
combinations n l@(x:xs)
  | n == 0    = []
  | n == 1    = listify l
  | otherwise = map (\y -> x : y) (combinations (n-1) xs) ++ (combinations n xs)

-- Problem 28
qsort :: Ord a => [b] -> (b -> a) -> [b]
qsort [] _ = []
qsort (x:xs) order = let lessThan = [y | y <- xs, order y < order x]
                         greaterThanOrEqual = [y | y <- xs, order y >= order x]
                     in (qsort lessThan order) ++ [x] ++ (qsort greaterThanOrEqual order)

lsort :: [[a]] -> [[a]]
lsort l = qsort l length
