
sub counter
{
    my $a = shift;

    my $next = sub {
        my $to_add = shift ;
        return counter($to_add+$a);
    };

    return ($a, $next);
}

my ($result,$next) = counter(5);
my ($result2, $next2) = $next->(100);
my ($result3, $next3) = $next2->(50);
my ($result4, $next4) = $next->(30);

print "\$result=$result\n\$result2=$result2\n\$result3=$result3\n\$result4=$result4\n";

