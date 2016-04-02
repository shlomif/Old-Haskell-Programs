#!/usr/bin/perl

sub primes
{
    my $how_much = shift;

    my @primes;

    my ($what, $step, $index, $prev_index);

    my @primes_linked_list = (1 .. ($how_much+1));

    my $prime = 2;
    my $limit = int(sqrt($how_much));
    while ($prime < $limit)
    {
        $what = $prime*$prime;
        $step = (($prime == 2) ? $prime : ($prime*2));
        $index = $primes_linked_list[$prime];
        while ($what < $how_much)
        {
            while ($what > $index)
            {
                $prev_index = $index;
                $index = $primes_linked_list[$index];
            }
            if ($index == $what)
            {
                $index = $primes_linked_list[$prev_index] = $primes_linked_list[$index];
            }
            $what += $step;
        }

        push @primes, $prime;
        $prime = $primes_linked_list[$prime];
    }
    while ($prime < $how_much)
    {
        push @primes, $prime;
        $prime = $primes_linked_list[$prime];
    }

    return \@primes;
}

print join(", ", @{primes(30000)});
