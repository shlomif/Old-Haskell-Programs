palindrome :: [a] -> [a]
palindrome xs = xs ++ (reverse xs)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (xs == (reverse xs))
