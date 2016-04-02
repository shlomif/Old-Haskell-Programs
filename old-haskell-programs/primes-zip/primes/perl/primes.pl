#!/usr/bin/perl

use strict;

sub primes
{
    my $how_much = shift;

    my (@array, $bound, $a, $b, @primes);

    @array = (1) x $how_much;

    $bound = int(sqrt($how_much))+1;

    for($a=2;$a<=$bound;$a++)
    {
        if ($array[$a])
        {
            for($b=$a*$a;$b<$how_much;$b+=$a)
            {
                $array[$b] = 0;
            }
            push @primes, $a;
        }
    }
    for(;$a<$how_much;$a++)
    {
        if ($array[$a])
        {
            push @primes, $a;
        }
    }

    return @primes;
}

print join(", ", primes(1000));
