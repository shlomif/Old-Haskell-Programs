module Primes where

import Prelude
import Array

how_much :: Int
how_much = 1000 

initial_primes_map :: Array Int Bool 
initial_primes_map = array (1, how_much) [ (i,True) | i <- [1 .. how_much] ]

mybound :: Int
mybound = ceiling(sqrt(fromInteger(toInteger(how_much))))

next_primes_map :: Int -> Array Int Bool -> Array Int Bool
next_primes_map a prev_primes_map = 
    if (a == mybound) 
    then prev_primes_map 
    else next_primes_map (a+1) (generate_next_primes_map a prev_primes_map)

generate_next_primes_map :: Int -> Array Int Bool -> Array Int Bool
generate_next_primes_map a primes_map = 
    if (primes_map!a) 
    then (on_prime_next_primes_map a primes_map) 
    else primes_map

prime_bound :: Int -> Int
prime_bound a = (floor(fromInteger(toInteger(how_much))/fromInteger(toInteger(a))))

on_prime_next_primes_map :: Int -> Array Int Bool -> Array Int Bool
on_prime_next_primes_map a primes_map = 
    primes_map // [ (i*a, False) | i <- [a .. (prime_bound a)] ]

get_primes_map :: Array Int Bool
get_primes_map = (next_primes_map 2 initial_primes_map)

list_primes :: Array Int Bool -> Int -> [Int]
list_primes primes_map n = 
    if (n > how_much) 
    then [] 
    else 
    (
        if primes_map!n 
        then n:(list_primes primes_map (n+1)) 
        else list_primes primes_map (n+1)
    )

show_primes = show (list_primes get_primes_map 2)

