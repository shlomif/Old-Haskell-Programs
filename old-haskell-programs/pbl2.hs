import IO

primes :: Int -> [Int]
primes how_much =  (iterate 2 initial_map) where
    initial_map :: [Bool]
    initial_map = (map (\x -> True) [ 0 .. how_much])
    iterate :: Int -> [Bool] -> [Int]
    iterate p (a:as) = (if (p > mybound) 
                       then process_map p (a:as)
                       else if a 
                       then p:(iterate (p+1) (mymark (p+1) (p*p) as))
                       else (iterate (p+1) as)) where
        step :: Int
        step = (if p == 2 then p else 2*p)
        mymark :: Int -> Int -> [Bool] -> [Bool]
        mymark cur_pos next_pos [] = []
        mymark cur_pos next_pos (a:as) = 
            if (cur_pos == next_pos) 
            then False:(mymark (cur_pos+1) (cur_pos+step) as)
            else a:(mymark (cur_pos+1) next_pos as)
    mybound :: Int
    mybound = ceiling(sqrt(fromIntegral(how_much)))
    process_map :: Int -> [Bool] -> [Int]
    process_map cur_pos [] = []
    process_map cur_pos (a:as) = 
        if a 
        then cur_pos:(process_map (cur_pos+1) as)
        else (process_map (cur_pos+1) as)

main = print (length (primes 1000000))
          

