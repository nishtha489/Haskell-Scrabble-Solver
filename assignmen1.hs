myLast :: (Eq a) => [a] -> a
myLast (x:xs) = if xs == [] then x else myLast xs


myRev :: [a] -> [a]
myRev [] = []
myRev (y : x) = myRev x ++ [y]


isPalindrome :: (Eq x) => [x] -> Bool
isPalindrome x = x == reverse x


compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = if x `elem` xs then compress xs
                  else [x] ++ compress xs

duplicate ::[a] -> [a]
duplicate [] = []
duplicate (x:xs) = take 2 (repeat x) ++ duplicate xs


rotate :: (Show a) => [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = drop n xs ++ take n xs


insertAt :: Char -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt 'z' xs n = take (n-1) xs ++ [z] ++ drop (n-1) xs

isPrime :: (Integral x) => x -> Bool
isPrime x = not(0 `elem` [rem x y | y <- [2 .. (x-1)]])
