intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse delim (x:[]) = x
intersperse delim (x:xs) = x ++ (delim:(intersperse xs))

