
primes = sieve [2..how_much] where
         sieve (p:x) =
            p : (if p <= mybound
                 then sieve (remove (p*p) x)
                 else x) where
                     remove what (a:as) =
                         if (a < what)
                         then a:(remove what as)
                         else if (a == what)
                         then (remove (what+step) as)
                         else a:(remove (what+step) as)
                     remove what [] = []
                     step = (if (p == 2) then p else (2*p))
         sieve [] = []

how_much :: Int
how_much = 10000

mybound :: Int
mybound = ceiling(sqrt(fromIntegral how_much))

