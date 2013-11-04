sub cartesian_product
{
    if (scalar(@_) == 0)
    {
        return ([]);
    }
    else
    {
        return 
        (map 
        { 
            my $i = $_ ; 
            (map 
            { 
                my $is = $_; 
                [$i, @{$is}] 
            } 
            cartesian_product(@_[1..$#_])
            )
        } 
        @{$_[0]}
        );
    }
}

use Data::Dumper;

my @ret = cartesian_product([4,5,6],[3,7], [8,9,10]);
my $d = Data::Dumper->new([\@ret],["\@ret"]);

print $d->Dump();

