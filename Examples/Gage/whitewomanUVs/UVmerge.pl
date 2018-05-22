#!/usr/bin/perl -w
use strict;

my @files = @ARGV;
die "DIED: need two files of UVs from command line\n"
  if(@files != 2);
  
my %DATA = ();
my @labels = ();
my %line = ();

open(FH, "<$files[0]") or die "DIED: $files[0] not opened because $!\n";
while(<FH>) {
  next if(/^#/);
  chomp;
  @labels = split(/\t/, $_);
  $_ = <FH>;
  last;
}
while(<FH>) {
  chomp;
  @line{@labels} = split(/\t/,$_);
  $DATA{$line{'DATETIME'}}->{-us} = $line{'VALUE'}; 
}
close(FH);
open(FH, "<$files[1]") or die "DIED: $files[1] not opened because $!\n";
while(<FH>) {
  next if(/^#/);
  chomp;
  @labels = split(/\t/, $_);
  $_ = <FH>;
  last;
}
while(<FH>) {
  chomp;
  @line{@labels} = split(/\t/,$_);
  $DATA{$line{'DATETIME'}}->{-ds} = $line{'VALUE'}; 
}
close(FH);

print "DATETIME,US,DS\n";
foreach my $datetime (sort keys %DATA) {
  my $us = $DATA{$datetime}->{-us};
  $us = "NA" if(not defined $us);
  my $ds = $DATA{$datetime}->{-ds};
  $ds = "NA" if(not defined $ds); 
  if($us ne "NA" and $ds ne "NA") {
    if($ds >= $us) {
      ($us, $ds) = ("NA", "NA");
    }
  }
  print "$datetime,$us,$ds\n";
}




