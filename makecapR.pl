#!/usr/bin/perl -w
use strict;
use File::Find;
my @files = ();
find sub { my $n = $File::Find::name;
           push(@files, $n) if($n =~ /\.R$/);
         }, ".";
unlink("capR.src");
foreach my $file (@files) {
  print STDERR "Sourcing $file\n";
  open(FH, ">>capR.src") or die "DIED: capR.src not opened because $!\n";
  print FH "\n\n",
  "################################################################\n",
  "##    capR Source File $file\n",
  "################################################################\n";
  system("cat $file >> capR.src");
}

