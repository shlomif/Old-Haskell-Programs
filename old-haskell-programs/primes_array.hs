
import Array

primes_map :: Int -> Array Int Bool
primes_map how_much = accumArray (\x -> \y -> y) True (2,how_much)
    [ (p*i,False) | p <- (primes mybound), i <- (multipliers p) ]
    where
        mybound :: Int
        mybound = ceiling(sqrt(fromIntegral how_much))
        multipliers :: Int -> [Int]
        --multipliers p = [p .. how_much `div` p]
        multipliers p = [ (i*step+p) | i <- [0 .. mylimit] ] where
            step = (if (p == 2) then 1 else 2)
            mylimit = (how_much `div` p - p) `div` step
            

get_primes_list :: Array Int Bool -> [Int]
get_primes_list pmap = (filter (pmap!) (indices pmap))

primes :: Int -> [Int]
primes 2 = [2]
primes how_much = get_primes_list (primes_map how_much)

main = print (primes 100000)
