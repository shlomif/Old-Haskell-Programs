import Data.List

ordByLen :: [a] -> [a] -> Ordering
ordByLen xs ys
    | xl > yl  = GT
    | xl < yl  = LT
    | xl == yl = EQ
        where
            xl = length xs
            yl = length ys

sortBySubListsLen :: [[a]] -> [[a]]
sortBySubListsLen xs = sortBy ordByLen xs
