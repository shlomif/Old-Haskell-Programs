#!/usr/bin/perl -I /home/httpd/cgi-bin/shlomi/lecture2

use Quad::Pres::Url;

$mode = shift;
$src_path_str = shift;
$src_is_dir = shift;
$dest_path_str = shift;
$dest_is_dir = shift;

@src_path = split(/\//, $src_path_str);
@dest_path = split(/\//, $dest_path_str);

$source = Quad::Pres::Url->new(
    \@src_path,
    $src_is_dir,
    $mode
    );

$dest = Quad::Pres::Url->new(
    \@dest_path,
    $dest_is_dir,
    $mode
    );

print $source->get_relative_url($dest, ($src_path_str =~ /\/$/));
print "\n";

$haskell_mode = $mode;
substr($haskell_mode, 0, 1) =~ tr/a-z/A-Z/;

$cmd = "";

$cmd = "echo 'get_relative_url ";

$cmd .= "(MakeUrl [ ";
$cmd .= join(", ", (map { '"' . $_ . '"' } @src_path));
$cmd .= " ] ";
$cmd .= $src_is_dir ? "True" : "False";
$cmd .= " ";
$cmd .= $haskell_mode . ") ";

$cmd .= "(MakeUrl [ ";
$cmd .= join(", ", (map { '"' . $_ . '"' } @dest_path));
$cmd .= " ] ";
$cmd .= $dest_is_dir ? "True" : "False";
$cmd .= " ";
$cmd .= $haskell_mode . ") ";

$cmd .= $slash_terminated ? "True" : "False";
$cmd .= "' | hugs Url.hs | tail +18 | head -1 | sed 's/\"//g'";
system($cmd);

