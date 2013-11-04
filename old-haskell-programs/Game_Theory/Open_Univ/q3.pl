#!/usr/bin/perl

use strict;

sub gen_perms
{
    my $prefix_ptr = shift;
    my $set_ptr = shift;

    if (! scalar(@$set_ptr))
    {
        print join(",",@$prefix_ptr),"\n";
        return;
    }
    
    my $elem;
    my @prev_elems;
    
    while ($elem = shift(@$set_ptr))
    {
        &gen_perms([ @$prefix_ptr, $elem ], [@prev_elems, @$set_ptr]);
        push @prev_elems, $elem;
    }
}

my $how_many = shift || 4;

gen_perms([ ], [1 .. $how_many]);


