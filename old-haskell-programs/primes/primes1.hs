primes = sieve [2..] where
    sieve (x:xs) = x:(sieve [a | a <- xs, a `mod` x /= 0 ])
