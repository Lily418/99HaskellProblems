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
