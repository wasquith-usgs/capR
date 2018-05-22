#!/usr/bin/perl -w
use strict;

use Getopt::Long;
my %OPTS = (); # command line options
my @options = qw( user=s host=s b=s e=s noget workingok
                  hw=i tw=i h s=s a=s wy=i yy=i z); # these are the valid command line options
&GetOptions(\%OPTS, @options); # parse the command line options

&Help() if($OPTS{'h'});

use Date::Calc qw( Delta_Days
                   Add_Delta_Days);

use constant S24 => scalar 24;
use constant S60 => scalar 60;

my $isApproved = ($OPTS{'workingok'}) ? 0 : 1;
my $tsprogram = "/usr/opt/bin/nwts2rdb";

my $Z  = ($OPTS{'z'}) ? "-C " : ""; # compression for ssh, might not help on fast networks
my $AG = ($OPTS{'a'}) ? uc($OPTS{'a'}) : "USGS"; # which agency code in the database
my $S  = ($OPTS{'s'}) ? uc($OPTS{'s'}) : "C";

my $host = ($OPTS{'host'}) ? $OPTS{'host'} : 0;
die "DIED: need NWIS host name by -host=string option\n" unless($host);

my $user = ($OPTS{'user'}) ? $OPTS{'user'} : 0;
die "DIED: need valid user name on NWIS host -user=string option\n" unless($user);

my ($station) = shift(@ARGV); # station comes on command line
die "DIED: station not defined as argument on command line\n"
    if(not defined $station);

# determine the year and check status
my $yy = ($OPTS{'yy'}) ? $OPTS{'yy'} : 0;
my $wy = ($OPTS{'wy'}) ? $OPTS{'wy'} : 0;
unless($wy || $yy) {
  die "DIED: need water year by -wy=integer option or year by ".
      "-yy=integer option\n";
}
my $wyminus1 = ($wy) ? $wy - 1 : 0;
my $beg = ($OPTS{'b'} and not $wy) ? $OPTS{'b'} : "1001 ";
my $end = ($OPTS{'e'} and not $wy) ? $OPTS{'e'} : "0930 ";

if($wy) {
  $beg = "1001 ";
  $end = "0930 ";
} else { # must by if($yy);
  ($wy, $wyminus1) = ($yy, $yy)
}

unless($OPTS{'noget'}) {
   ############################
   ##  HEADWATER RETRIEVAL
   ############################
   my $DD = ($OPTS{'hw'}) ? $OPTS{'hw'} : 0; # Specify various codes for the retrieval
   die "DIED: need headwater data descriptor by -hw=interger option\n"
       unless($DD);
   my $com = "ssh -v $Z $user\@$host $tsprogram -c -tuv -d$DD -aUSGS ".
             "-n$station -s$S ".
             "-b$wyminus1$beg -e$wy$end ".
             "2>TMP4capR_STDERR.txt ".
             " >TMP4capR_uvHW.rdb";

   print STDERR "EXTERNAL COMMAND: $com\n";
   system("$com");

   ############################
   ##  TAILWATER RETRIEVAL
   ############################
   $DD = ($OPTS{tw}) ? $OPTS{tw} : 0; # Specify various codes for the retrieval
   die "DIED: need headwater data descriptor by -hw=interger option\n"
       unless($DD);

   $com = "ssh -v $Z $user\@$host $tsprogram -c -tuv -d$DD -aUSGS ".
          "-n$station -s$S ".
          "-b$wyminus1$beg -e$wy$end ".
          "2>TMP4capR_STDERR.txt ".
          " >TMP4capR_uvTW.rdb";
   print STDERR "EXTERNAL COMMAND: $com\n";
   system("$com");
}

###################################
## PROCESSING FOR HAND OFF TO R
###################################
my $HW = readRDB("TMP4capR_uvHW.rdb"); # read HW data
my $TW = readRDB("TMP4capR_uvTW.rdb"); # read TW data

# let us determine the unique keys, which are datetime values
# between both retrievals---remember, the files might not have same
# time stamps (seconds were ignored)
my %dates = ();
map { $dates{$_}++ } sort (keys %$HW, keys %$TW);
# the union of the dates of the two times series occurs here

# Now output the unique date list and store hash for lookup
my $earliest_date = 1;
my ($epoch_yyyy, $epoch_mm, $epoch_dy, $epoch_hh, $epoch_mn) = (1900,1,1,0,0);
my %human_date = ();
my %epoch_date = ();
open(FH, ">TMP4capR_alldates.txt") or
     die "DIED: TMP4capR_alldates.txt not opened because $!\n";
   print FH "DATE|DAYS\n";
   foreach my $date (sort keys %dates) {
      if(not $earliest_date) {
         $earliest_date = $date;
         ($epoch_yyyy, $epoch_mm, $epoch_dy, $epoch_hh, $epoch_mn) =
                                           unpack("A4A2A2A2A2", $earliest_date);
      }
      my ($yyyy, $mm, $dy, $hh, $mn) = unpack("A4A2A2A2A2", $date);
      #print STDERR "$date: $yyyy, $mm, $dy, $hh, $mn\n";
      #print STDERR "epoch: $epoch_yyyy, $epoch_mm, $epoch_dy\n";
      my $day   = &Delta_Days( $epoch_yyyy, $epoch_mm, $epoch_dy,
                               $yyyy, $mm, $dy );
      my $days  = &dayhhmmss2days( $day, $hh, $mn, 0 );
      $epoch_date{$date} = sprintf("%0.9f",$days);
      my $dt = "$yyyy-$mm-$dy $hh:$mn"; # build a more human date
      $human_date{$date} = $dt;
      print FH "$dt|$days\n";
   }
close(FH); # close the file

# let us make a full dump of the headwater data
open(FH, ">TMP4capR_HW.txt") or
     die "DIED: TMP4capR_HW.txt not opened because $!\n";
   print FH "DATE|DAYS|HW\n";
   foreach my $date (sort keys %$HW) {
      print FH "$human_date{$date}|$epoch_date{$date}|$HW->{$date}\n";
   }
close(FH); # close the file

# let us make a full dump of the tailwater data
open(FH, ">TMP4capR_TW.txt") or
     die "DIED: TMP4capR_TW.txt not opened because $!\n";
   print FH "DATE|DAYS|TW\n";
   foreach my $date (sort keys %$TW) {
      print FH "$human_date{$date}|$epoch_date{$date}|$TW->{$date}\n";
   }
close(FH); # close the file

# let us make a dump of the INTERSECTION (matching datetimes) for
# headwater and tailwater
open(FI, ">TMP4capR_HWTWinter.txt") or
     die "DIED: TMP4capR_HWTWinter.txt not opened because $!\n";
# let us make a dump of the UNION (fill in gaps of the other) for
# headwater and tailwater
open(FU, ">TMP4capR_HWTWunion.txt") or
     die "DIED: TMP4capR_HWTWunion.txt not opened because $!\n";
   print FI "DATE|DAYS|HW|TW\n";
   print FU "DATE|DAYS|HW|TW\n";
   foreach my $date (sort keys %dates) {
      my ($hw, $tw) = ($HW->{$date}, $TW->{$date}); # extract the pairing
      $hw = "NA" if(not defined $hw);
      $tw = "NA" if(not defined $tw);
      print FU "$human_date{$date}|$epoch_date{$date}|$hw|$tw\n"; # output
      next unless($hw ne "NA" and $tw ne "NA"); # intersection of the time series occurs here
      print FI "$human_date{$date}|$epoch_date{$date}|$hw|$tw\n"; # output
   }
close(FI); # close the file
close(FU); # close the file

########################### SUBROUTINES #####################
sub readRDB {
  my $file = shift; # which file to process
  open(FH, "<$file") or die "DIED: $file not opened because $!\n";
  my @labels = (); # the label line in the RDB
  my %line = (); # a hash of each line in the file
  my $data = {}; # a hash reference to return from subroutine
  while(<FH>) { # while a line can be read
    next if(/^#/); # skip comments
    chomp; # remove \n
    @labels = split(/\t/, $_); # split line on the tabs
    $_ = <FH>; # extract the useless format line in the RDB
    last; # break out of this loop
  }
  while(<FH>) { # while a line can be read
    next if(/^#/); # skip comments
    chomp; # remove \n
    my @vals = split(/\t/, $_); # split line on the tabs
    next if($isApproved and $vals[$#vals] ne "A");
    @line{@labels} = @vals; # slice the values into the line hash
    #print "LINE $_\n";
    #next;
    #print STDERR "$line{'DATETIME'} and $line{'VALUE'}\n";
    my ($datetime) = unpack("A12", $line{DATETIME}); # strip seconds
    my $val = $line{VALUE}; # extract the value of interest, yes extra step but VALUE
    next if(not defined $val or $val eq ""); # skip missing or empty should they be there
    $data->{$datetime} = $line{VALUE}; # store it
    # the date is not turned to a human form until later so that the simple alphabetical
    # sort keys %hash idioms, which will come after this subroutine can be used
    # and provide very clear code
  }
  close(FH); # close the file
  return($data); # return the data
}


sub Help {

}


# hhmmss2fracday:
# convert a list of hours, minutes, and seconds to a
# decimal fraction of a day, note that 24,00,00 converts to
# 1 (the next day).
sub hhmmss2fracday {
   return ($_[0]+(($_[1]+($_[2]/S60))/S60))/S24;
}

# dayhhmmss2days:
# convert a list of (days, hours, minutes, seconds) to
# a real number days.frac
sub dayhhmmss2days {
   return ($_[0]+($_[1]+(($_[2]+($_[3]/S60))/S60))/S24);
}










__END__
# source('capR.out'); # load capR!

# declare cross section of approach
XY <- data.frame(X=c( -104, -61.6, -39.9, 11.4, 35.4, 50.6, 69.1, 96.3),
                 Y=c(11.56,  8.96,  5.76, 2.72, 2.72, 3.59, 7.65, 9.05));

# set the approach
my.approach <- setApproach(station.id="08123620",
                           station.name="Sulphur Springs Draw near Wellman, Texas",
                           Lapproach=25,
                           nvalue=0.03,
                           XY=XY);

# create a single culvert
my.culvert <- setCulvert(name="The only culvert",
                           type="box",
                           material="concrete",
                           width=24,
                           diameter=6,
                           web=3,
                           theta=60,
                           nvalue=0.02,
                           beveling=0.03,
                           zusinvert=2.20,
                           zdsinvert=2.55,
                           inlet.depression=0.45,
                           outlet.depression=0.85,
                           L=36.5);

# set the pin elevations of the CSG pipes
setMinRecordables(hwpin=3.03, twpin=2.74, culvert=my.culvert); # Pins 05/18/2006

# NOW in the shell
# capRwyuv.pl -wy=2007  -hw=1  -tw=4  08123620
nwts2capR(unlink=FALSE, wy=2008, hw=1, tw=4, station="08123620", noget=TRUE);

# back to R
uvs <- read.nwts2capR(unlink=FALSE);
HWTW <- uvs$union.of.head.and.tailwater
wy <- capgraph(h1=HWTW$HW,
               h4=HWTW$TW,
               datetime=HWTW$DATE,
               culvert=my.culvert, approach=my.approach,
               silent=TRUE);

plot.capgraph(wy, culvert=my.culvert, index=37000:41000)

