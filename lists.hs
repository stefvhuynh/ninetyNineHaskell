-- Problem 1 (*) Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No last element in an empty list"
myLast xs = xs !! (length xs - 1)

myLast' :: [a] -> a
myLast' [] = error "No but last element in an empty list"
myLast' [x] = x
myLast' (_:xs) = myLast' xs

-- Problem 2 (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "No but last element in an empty list"
myButLast [_] = error "No but last element in a list with one element"
myButLast xs = xs !! (length xs - 2)

myButLast' :: [a] -> a
myButLast' [] = error "No but last element in an empty list"
myButLast' [_] = error "No but last element in a list with one element"
myButLast' [x, _] = x
myButLast' (_:xs) = myButLast' xs
