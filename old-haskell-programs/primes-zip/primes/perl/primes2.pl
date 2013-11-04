#!/usr/bin/perl

use strict;

my (@primes, $a, $p);
@primes = (2);
MAIN_LOOP: 
for($a = 3; $a < 1000; $a++)
{
    foreach $p (@primes)
    {
        if ($a % $p == 0)
        {
            next MAIN_LOOP;
        }
    }
    push @primes, $a;
}
print join(", ", @primes);
