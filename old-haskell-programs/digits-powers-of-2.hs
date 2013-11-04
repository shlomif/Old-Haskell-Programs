myp :: Integer -> [Integer]
myp a = a:(myp (a*2))

powers_of_2 = myp 1

digits_sum 0 = 0
digits_sum x = (x `mod` 10) + digits_sum(x `div` 10)

pds = (map digits_sum powers_of_2)
