-- Problem 1 (*) Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No last element in an empty list"
myLast xs = xs !! (length xs - 1)

myLast' :: [a] -> a
myLast' []     = error "No but last element in an empty list"
myLast' [x]    = x
myLast' (_:xs) = myLast' xs


-- Problem 2 (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast []  = error "No but last element in an empty list"
myButLast [_] = error "No but last element in a list with one element"
myButLast xs  = xs !! (length xs - 2)

myButLast' :: [a] -> a
myButLast' []     = error "No but last element in an empty list"
myButLast' [_]    = error "No but last element in a list with one element"
myButLast' [x, _] = x
myButLast' (_:xs) = myButLast' xs


-- Problem 3 (*) Find the K'th element of a list where the first number is 1.
elementAt :: [a] -> Int -> a
elementAt _ 0  = error "k cannot be 0"
elementAt xs k
  | length xs < k = error "k cannot be bigger than the length of the list"
  | otherwise     = xs !! (k - 1)

elementAt' :: [a] -> Int -> a
elementAt' _ 0      = error "k cannot be 0"
elementAt' [] _     = error "k cannot be larger than the length of the list"
elementAt' (x:_) 1  = x
elementAt' (_:xs) k = elementAt' xs (k - 1)


-- Problem 4 (*) Find the number of elements in a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = sum [1 | _ <- xs]

myLength'' :: [a] -> Int
myLength'' xs = foldl (\length _ -> length + 1) 0 xs


-- Problem 5 (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' xs = foldl (\acc x -> x : acc) [] xs


-- Problem 6 (*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs
