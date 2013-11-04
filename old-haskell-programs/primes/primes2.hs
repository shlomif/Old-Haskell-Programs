primes = sieve [2..] where
    sieve (p:xs) =
        p : (sieve (remove p xs)) where
            remove what (a:as)  | a < what = a:(remove what as)
                                | a == what = (remove (what+p) as)
                                | a > what = (remove (what+p) (a:as))
