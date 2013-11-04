

prime_bound :: Int -> Int
prime_bound a = (floor(fromIntegral(how_much)/fromIntegral(a)))

primes_remove :: Int -> [Int] -> [Int]
primes_remove p primes = 
    primes_remove_helper (p*p) primes where
    primes_remove_helper remove_what (a:as) = 
        if (a < remove_what)
        then (primes_remove_helper remove_what as)
        else if (a == remove_what)
             then (prime_remove_helper (remove_what+2*p) as)
             else a:(prime_remove_helper (remove_what+2*p) as)
    primes_remove_helper remove_what [] = []

primes = sieve [2..how_much] where
         sieve (p:x) = p : if p <= mybound
--                           then sieve (remove [ n*p | n <- [p..prime_bound(p)] ] x)
                           then sieve (primes_remove p x)
                           else x
         sieve [] = []
--         remove (b:bs) (a:as) | a == b = remove bs as
--                              | a < b = a : (remove (b:bs) as)
--                              | b < a = remove bs (a:as)
--         remove _ a = a

how_much :: Int
how_much = 10000 

mybound :: Int
mybound = ceiling(sqrt(fromIntegral how_much))

