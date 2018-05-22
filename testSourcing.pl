#!/usr/bin/perl -w
use strict;
use File::Find;
my @files = ();
find sub { my $n = $File::Find::name;
           push(@files, $n) if($n =~ /\.R$/);
         }, ".";
foreach my $file (@files) {
  print STDERR "Sourcing $file\n";
  my $g = `R --vanilla < $file\n`;
}
