allTrue = True : allTrue

primes = sieve 2 allTrue

sieve p (False : marks) = sieve (p+1) marks
sieve p (True  : marks) =
        p : sieve (p+1) (mark p marks)
        where mark 1 (u : marks) = False : mark p marks
              mark n (m : marks) = m : mark (n-1) marks

