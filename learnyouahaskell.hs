-- Extra solutions from reading learnyouahaskell.com.

-- Take.
myTake :: Int -> [a] -> [a]
myTake i (x:xs)
  | i == 0    = []
  | i == 1    = [x]
  | otherwise = x : myTake (i - 1) xs


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

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = myFoldr (\x acc -> f x : acc) [] xs


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
myFoldl _ acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


-- Fold right.
myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr _ acc []     = acc
myFoldr f acc (x:xs) = f x $ myFoldr f acc xs


-- Maximum using folds.
myMax :: (Ord a) => [a] -> a
myMax (x:xs) = myFoldl (\acc x -> if x < acc then acc else x) x xs


-- Scan left.
myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f acc xs = myFoldl g [acc] xs
  where g = \accList x -> accList ++ [f (accList !! (length accList - 1)) x]


-- Scan right.
myScanr :: (b -> a -> a) -> a -> [b] -> [a]
myScanr f acc xs = myFoldr g [acc] xs
  where g = \x accList -> f x (accList !! 0) : accList


-- Intersperse.
myIntersperse :: a -> [a] -> [a]
myIntersperse _ (x:[])     = [x]
myIntersperse inter (x:xs) = x : inter : myIntersperse inter xs


-- Concat.
myConcat :: [[a]] -> [a]
myConcat = myFoldl (\acc xs -> acc ++ xs) []
