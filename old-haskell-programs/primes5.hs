import Array

how_much :: Int
how_much = 1000000

initial_primes_map :: Array Int Bool
initial_primes_map = array (2, how_much) [ (i,True) | i <- [2 .. how_much] ]

mybound :: Int
mybound = ceiling(sqrt(fromIntegral(how_much)))

get_primes_map :: Array Int Bool
get_primes_map = (next_primes_map 2 initial_primes_map) where
    next_primes_map a primes_map =
        if (a == mybound)
        then primes_map
        else next_primes_map (a+1) (
            if primes_map!a
            then primes_map // [ (i*a, False) | i <- [a .. (how_much `div` a)] ]
            else primes_map
            )



list_primes :: Array Int Bool-> [Int]
list_primes primes_map = filter (primes_map!) (indices primes_map)

main = print (list_primes get_primes_map)


