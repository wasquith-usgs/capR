#!/usr/bin/perl -w
use strict;
use File::Find;
my @files = ();
find sub { my $n = $File::Find::name;
           push(@files, $n) if($n =~ /\.R$/);
         }, ".";
unlink("capR.txt");
foreach my $file (@files) {
  print STDERR "Sourcing $file\n";
  system("cat $file >> capR.txt");
}
