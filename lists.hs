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
myLength'' xs = myFoldl (\length _ -> length + 1) 0 xs


-- Problem 5 (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


-- Problem 6 (*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs


-- Extra solutions from reading learnyouahaskell.com.

-- Zip two lists with a function.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _ []          = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys


-- Take a function and return a function with the arguments flipped.
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x


-- Map.
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs


-- Filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x       = x : myFilter f xs
  | otherwise = myFilter f xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f xs = [x | x <- xs, f x]


-- Fold left.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ y []     = y
myFoldl f y (x:xs) = myFoldl f (f y x) xs
