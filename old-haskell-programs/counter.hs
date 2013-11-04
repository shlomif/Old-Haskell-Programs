
newtype Counter = Counter (Int -> (Int, Counter))

count :: Counter -> Int -> (Int, Counter)
count (Counter c) = c

counter n = (n, Counter next)
  where next k = counter (n+k)

xs = let (num1, next1) = (counter 5)
         (num2, next2) = (next1 `count` 100)
         (num3, next3) = (next2 `count` 50) in
       [num1,num2,num3]

