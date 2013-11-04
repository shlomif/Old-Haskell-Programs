primes = sieve [2..how_much] where
         sieve (p:x) = p : if p <= mybound
                           then sieve (remove [ n*p | n <- [2..] ] x)
                           else x
         sieve [] = []
         remove (b:bs) (a:as) | a == b = remove bs as
                              | a < b = a : (remove (b:bs) as)
                              | b < a = remove bs (a:as)
         remove _ a = a

how_much :: Int
how_much = 500000 

mybound :: Int
mybound = ceiling(sqrt(fromIntegral how_much))

