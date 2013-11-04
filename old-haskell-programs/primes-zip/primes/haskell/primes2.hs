module Primes where

import Prelude
import Array

how_much :: Int
how_much = 1000 

initial_primes_map :: Array Int Int
initial_primes_map = array (1, how_much) [ (i,1) | i <- [1 .. how_much] ]

mybound :: Int
--mybound :: Floating Int -> RealFrac Int
mybound = ceiling(sqrt(fromInteger(toInteger(how_much))))
--mybound = sqrt(how_much)

next_primes_map :: Int -> Array Int Int -> Array Int Int 
next_primes_map a prev_primes_map = if (a == mybound) then prev_primes_map else next_primes_map (a+1) (generate_next_primes_map a prev_primes_map)

generate_next_primes_map :: Int -> Array Int Int -> Array Int Int
generate_next_primes_map a primes_map = if (primes_map!a) == 1 then (on_prime_next_primes_map a primes_map) else primes_map

mark_as_zero :: Int -> Int -> Array Int Int-> Array Int Int
mark_as_zero prime a primes_map = if (a > how_much) then primes_map else (mark_as_zero prime (a+prime) (primes_map // [(a,0)]))


on_prime_next_primes_map :: Int -> Array Int Int -> Array Int Int
on_prime_next_primes_map a primes_map = mark_as_zero a (a*a) primes_map

get_primes_map :: Array Int Int
get_primes_map = (next_primes_map 2 initial_primes_map)

list_primes :: Array Int Int -> Int -> [Int]
list_primes primes_map n = if (n > how_much) then [] else (if (primes_map!n == 1) then n:(list_primes primes_map (n+1)) else list_primes primes_map (n+1))

show_primes = show (list_primes get_primes_map 2)

