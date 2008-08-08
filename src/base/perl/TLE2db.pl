#!/usr/bin/perl -w
#################################################################
#
# This script reads in TLE's and stores the data in a database.
# Additional useful information is computed from the TLEs 
# such as the height of perigee and apogee, period, and ballistic 
# coefficient.
#
# Written by Matt Wilkins, Naval Postgraduate School
# September 2005
#
#
# NORAD Two-Line Element Set Format
#
# Data for each satellite consists of three lines in the following format:
#
# AAAAAAAAAAAAAAAAAAAAAAAA
# 1 NNNNNU NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN
# 2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN
#
# Line 0 is a twenty-four character name (to be consistent with the name
# length in the NORAD SATCAT).
#
# Lines 1 and 2 are the standard Two-Line Orbital Element Set Format
# identical to that used by NORAD and NASA. The format description is:
#
# Line 1
# Column 	Description
# ---------     ------------------------------------------------------------
# 01 	        Line Number of Element Data
# 03-07 	Satellite Number
# 08 	        Classification (U=Unclassified)
# 10-11 	International Designator (Last two digits of launch year)
# 12-14 	International Designator (Launch number of the year)
# 15-17 	International Designator (Piece of the launch)
# 19-20 	Epoch Year (Last two digits of year)
# 21-32 	Epoch (Day of the year and fractional portion of the day)
# 34-43 	First Time Derivative of the Mean Motion
# 45-52 	Second Time Derivative of Mean Motion (decimal point assumed)
# 54-61 	BSTAR drag term (decimal point assumed)
# 63 	        Ephemeris type
# 65-68 	Element number
# 69 	        Checksum (Modulo 10)
# (Letters, blanks, periods, plus signs = 0; minus signs = 1)
#
# Line 2
# Column 	Description
# ---------     ------------------------------------------------------------
# 01 	        Line Number of Element Data
# 03-07 	Satellite Number
# 09-16 	Inclination [Degrees]
# 18-25 	Right Ascension of the Ascending Node [Degrees]
# 27-33 	Eccentricity (decimal point assumed)
# 35-42 	Argument of Perigee [Degrees]
# 44-51 	Mean Anomaly [Degrees]
# 53-63 	Mean Motion [Revs per day]
# 64-68 	Revolution number at epoch [Revs]
# 69 	        Checksum (Modulo 10)
#
# All other columns are blank or fixed.
#
# Example:
#
# NOAA 14                 
# 1 23455U 94089A   97320.90946019  .00000140  00000-0  10191-3 0  2621
# 2 23455  99.0090 272.6745 0008546 223.1686 136.8816 14.11711747148495
#
# For further information, see "Frequently Asked Questions: Two-Line
# Element Set Format" </columns/v04n03/> in the /Computers & Satellites/
# column of /Satellite Times/, Volume 4 Number 3.
#
#------------------------------------------------------------------------
#     Format description taken from:
#     http://celestrak.com 	
#     Dr. T.S. Kelso [TS.Kelso@celestrak.com]
#     Last updated: 2004 August 30 17:25:47 UTC
#
#

# USED ON GUPPY ONLY
BEGIN {
    push @INC, "/home/mwilkins/include";
    push @INC, "/home/mwilkins/PerlMPI/lib64/perl5/site_perl/5.8.5/x86_64-linux-thread-multi";
    push @INC, "/home/mwilkins/DBD-mysql-4.001/lib64/perl5/site_perl/5.8.5/x86_64-linux-thread-multi";
}

# USED ON JAWS ONLY
#BEGIN {
#    push @INC, "/home/mwilkins/include";
#    push @INC, "/home/mwilkins/PerlMPI/lib64/perl5/site_perl/5.8.5/x86_64-linux-thread-multi";
#    push @INC, "/home/mwilkins/DBDMysql/lib64/perl5/site_perl/5.8.5/x86_64-linux-thread-multi";
#}

use strict;
use warnings;
use Math::Trig;
use POSIX qw(floor ceil);
use Dates;
use mean2osc;
use el2el;
use convert;
use Math::BigFloat;
use DBI;
#use Constvar;
use Tie::File;

# Set program defaults

my $cleartables = 1;
my $oflag = "i";
my $startsat = 1;
my $testingflag = 0;
my $dir = "/home/mwilkins/TLE2db";
my $dbname = "original";
my $computeddb = "computed";
my $osculatingdb = "osculating";
my $backup = 0;
my $separg = ";";
my $subdirexp = "^\\d+\$";
my $fileexp = "^S\\d+\$";
my $numrec = 500000;
my $m2oflag = 0;
my $twothreeflag = 2;
my $debug = 0;

@ARGV >= 1 or die "Usage: TLE2db.com -[options]
       --sat=[starting catalog number] 
	--dir=[TLE directory] 
        --subdir=[Regular Expression for Allowable Subdirectories]
	--file=[Regular Expression for Allowable TLE Filenames]
	--db=[name of TLE database] 
        --cmpdb=[name of database for derived data]
	--oscdb=[name of database for osculating elements]
	--separg=[Character to separate flat file fields] 
        --twothreeflag=[2 = two line elsets, 3 = three line elsets]
        --numrec=[Number of records per flat file]
        --debug [Add this flag to turn on debugging]


		[options]
		-b Backup databases before modification
		-w Write new TLE database, old database will be discarded
		-a Append to current TLE database
		-o Overwrite duplicate data while appending
		-t Testing mode: Execute code without executing database insertions
		-d Run script with default settings, no other options need be specified
		-m2o Compute the osculating elements

		[defaults]
		--dir		$dir
		--subdir 	$subdirexp
		--file 		$fileexp
		--db 		$dbname
		--cmpdb 	$computeddb
		--oscdb 	$osculatingdb
		--sat 		$startsat
		--separg 	$separg
                --twothreeflag  $twothreeflag
		--numrec 	$numrec\n\n";

my $arg;

# Parse through arguments

ARGUMENT: foreach $arg (@ARGV) {

	if ($arg =~ /^-([a-z]+)/) {

		my $list = $1;

		while ($list =~ /(.)/g) {

			if ($list eq "d") { # Use default settings

				last ARGUMENT;

			} elsif ($list eq "m2o") {
				
				$m2oflag = 1;				

			} elsif ($list eq "b") {
				
				$backup = 1;

			} elsif ($list eq "w") {

				###########################################
				#
				#     Clear Tables Flag
				#
				# WARNING: SET THIS FLAG TO 0 IF YOU WANT
				# TO KEEP YOUR CURRENT DATA
				#
	
				$cleartables = 1;


			} elsif ($list eq "a") {

				###########################################
				#
				#     Clear Tables Flag
				#
				# WARNING: SET THIS FLAG TO 0 IF YOU WANT
				# TO KEEP YOUR CURRENT DATA
				#

				$cleartables = 0;

			} elsif ($list eq "o") {

				$oflag = "r" unless $cleartables; # Only need to overwrite if appending

			} elsif ($list eq "t") {

				###########################################
				#
				#     Execute Data Insertion Flag
				#
				# Set this flag to 1 if you just
				# want to run the script in test mode
				# and not actually put data in the database.
				#

				$testingflag = 1;

			} else {

				die "Invalid option specified.  Type TLE2db.com for usage.\n Died ";

			}

		}

	} elsif ($arg =~  /^--debug/) {

		###########################################
		#
		# This flag turns on debugging
		#

		$debug = 1;

	} elsif ($arg =~  /^--numrec[\s*=?\s*](\d+)/) {

		###########################################
		#
		# Number of records to write in flatfile
		# before uploading to the database
		#

		$numrec = $1;

	} elsif ($arg =~  /^--separg[\s*=?\s*](.).*/) {

		###########################################
		#
		# Separator argument for flat files
		#

		$separg = $1;

	} elsif ($arg =~  /^--subdir[\s*=?\s*](.+)/) {

		###########################################
		#
		# Directory location of TLE files
		#

		$subdirexp = $1;

	} elsif ($arg =~  /^--file[\s*=?\s*](.+)/) {

		###########################################
		#
		# Directory location of TLE files
		#

		$fileexp = $1;

	} elsif ($arg =~  /^--dir[\s*=?\s*](.+)/) {

		###########################################
		#
		# Directory location of TLE files
		#

		$dir = $1;

	} elsif ($arg =~ /^--db[\s*=?\s*]([a-zA-Z][a-zA-Z0-9]*)/) {

		###########################################
		#
		# Database name for TLE data
		#

		$dbname = $1;

	} elsif ($arg =~ /^--oscdb[\s*=?\s*]([a-zA-Z][a-zA-Z0-9]*)/) {

		###########################################
		#
		# Database name for osculating elements
		#

		$osculatingdb = $1;

	} elsif ($arg =~ /^--cmpdb[\s*=?\s*]([a-zA-Z][a-zA-Z0-9]*)/) {

		###########################################
		#
		#  Database name for derived data
		#

		$computeddb = $1;

	} elsif ($arg =~ /^--twothreeflag[\s*=?\s*]([0-9]+)/) {

		###########################################
		#
		#  Specify if file has standard two line
                #  elsets or an extra third line
		#

		$twothreeflag = $1;

	} elsif ($arg =~ /^--sat[\s*=?\s*]([0-9]+)/) {

		###########################################
		#
		#     Starting Satnum
		#
		# Set this flag to the satellite number
		# from which you wish to start data input.
		# Note that all current data in the table
		# for this satellite and any satellites 
		# whose satnum is greater will be cleansed.
		#
				
		$startsat = $1;
	
		if ($startsat != 1) { 
			$cleartables = 0; # Failsafe
		}

	} else {

		die "Invalid argument $arg specified.  Type TLE2db.com for usage.\n Died ";

	}

}

print "TLE directory: $dir\n";
print "Sub directory RegExp: $subdirexp\n";
print "TLE filename RegExp: $fileexp\n";
print "Datbase name: $dbname\n";
print "Osc datbase name: $osculatingdb\n";
print "Computed datbase name: $computeddb\n";
print sprintf("Testing: %s\n",$testingflag ? "True" : "False");
print sprintf("Write/Append: %s\n",$cleartables ? "Writing" : "Appending");
print sprintf("Overwrite: %s\n", ($oflag eq "r") ? "True" : "False") unless $cleartables;
print sprintf("Backup databases: %s\n",$backup ? "True" : "False");
print "Starting space object: $startsat\n";
print "Separator argument: $separg\n";
print "Max records per flat file insertion: $numrec\n";
print sprintf("Compute osculating elements: %s\n",$m2oflag ? "True" : "False");
print sprintf("Debugging is on: %s\n",$debug ? "True" : "False");

###########################################
#
#     Backup Databses
#

if ($backup) {

	print "\nBacking up databases.  This may take a few minutes!\n";
	system("cp /usr/local/mysql/5.0.27/data/tle/$dbname.* /home/mwilkins/scratch/data") == 0 or die "Unable to backup databases. Died";
	system("cp /usr/local/mysql/5.0.27/data/tle/$computeddb.* /home/mwilkins/scratch/data") == 0 or die "Unable to backup databases. Died";
	system("cp /usr/local/mysql/5.0.27/data/tle/$osculatingdb.* /home/mwilkins/scratch/data") == 0 or warn "Unable to backup $osculatingdb. Died";

}

###########################################
#
#     Define some constant parameters
#

#tie my $MUE, Tie::Constvar, 398600.4415;
#tie my $RE, Tie::Constvar, 6378.1363;
#tie my $PI, Tie::Constvar, 4 * atan2 1, 1;

my ($MUE,$RE,$PI);

$MUE = 398600.4415;
$RE = 6378.1363;
$PI = 4 * atan2 1, 1;

# Convert revs per day to rad per sec
sub rpd2rps { 2 * $PI * $_[0] / 86400; } 

###########################################
#
# Directories to search for TLEs
#

my($subdir,@subdirs,$file,@files,$count);

print "\nLocating TLE directories...\n";

opendir(DIR,$dir) or die "Can't opendir $dir: $!";

@subdirs = ();
@subdirs = map { $_->[1] } 		# extract pathnames
        sort { $a->[0] <=> $b->[0] }	# sort names numeric
        grep { -d $_->[1] }  		# path is a dir
        map { [ $_, "$dir/$_" ] }	# form (name,path)
	# Use this search to parse through all directories
	grep { /$subdirexp/ }		# just numerical names
	# Use this search for a specific folder
	#grep {/^18300$/}
        readdir(DIR);			# all files remaining

# This opens the errorlog datafile to collect problem TLEs.

my $logfile = "TLEerrlog.log";

if (-r $logfile && -w $logfile ) {
	open(ERRLOG, ">>$logfile") || warn "Unable to open logfile for appending.\n";
}
else {
	open(ERRLOG, ">$logfile") || warn "Unable to open logfile for writing.\n";
}

# This opens a log that tracks which satellites have been processed

my $logfile2 = "TLElog.log";

if (-r $logfile2 && -w $logfile2 ) {
        open(LOGFILE, ">>$logfile2") || warn "Unable to open logfile for appending.\n";
}
else {
        open(LOGFILE, ">$logfile2") || warn "Unable to open logfile for writing.\n";
}

#########################################################
#
# This connects to the MySQL database
#
#

print "\nConnecting to the database....\n";

#my $host = "ho616n6.hoku.mhpcc.hpc.mil";
my $host = "localhost";

my $dbh = DBI->connect("dbi:mysql:database=tle;host=$host;port=3306;mysql_compression=1;mysql_auto_reconnect=1",
            'mwilkins','I$gotmysql',{ RaiseError => 1, AutoCommit => 1}) || die $DBI::errstr;

#my $dbh =
# localhost (fastest but server must reside on same machine you execute script from)
#DBI->connect('dbi:mysql:mysql_socket=/home/mwilkins/mysql/tmp/mysql.sock:tle:localhost:3306:1',
# ho323n6
#DBI->connect('dbi:mysql:mysql_socket=/home/mwilkins/mysql/tmp/mysql.sock:tle:140.31.196.197:3306:1',
# ho612n6
#DBI->connect('dbi:mysql:mysql_socket=/home/mwilkins/mysql/tmp/mysql.sock:tle:10.128.38.70:3306:1',
#           'root','I$gotmysql',
#           { RaiseError => 1, AutoCommit => 1}) || die $DBI::errstr;

# Clear out the tables so that we do not have duplicate data.

if ($cleartables && $startsat == 1) {

	print "Deleting existing databases and creating fresh database tables....\n";

	# WARNING: ALL CURRENT DATA IN TABLES WILL BE LOST.

	$dbh->do("DROP TABLE IF EXISTS $dbname") unless $testingflag;

	my $create = "CREATE TABLE `$dbname` ( 
		`Idnum` int(11) NOT NULL auto_increment,
		`Satnum` int(5) NOT NULL default '0', 
		`Classification` char(1) NOT NULL default '',
 		`IntlDesig` varchar(15) NOT NULL default '',
		`IDLaunchYear` int(2) default NULL,
		`IDLaunchNum` int(3) default NULL,
		`IDPieceNum` char(3) default NULL,
		`Year` int(2) NOT NULL default '0',
		`DayOfYear` double(12,8) NOT NULL default '0.00000000',
		`Ndotby2` double NOT NULL default '0',
		`Nddotby6` double NOT NULL default '0',
		`Bstar` float NOT NULL default '0',
		`EphemType` char(1) NOT NULL default '',
		`ElNum` int(4) NOT NULL default '0',
		`Inclination` float(8,4) NOT NULL default '0.0000',
		`RAAN` float(8,4) NOT NULL default '0.0000',
		`Eccentricity` float(9,7) NOT NULL default '0.0000000',
		`ArgPer` float(8,4) NOT NULL default '0.0000',
		`MeanAnomaly` float(8,4) NOT NULL default '0.0000',
		`MeanMotion` double(17,14) NOT NULL default '0.00000000000000',
		`RevNum` int(5) NOT NULL default '0',
		`ModJulianDate` double NOT NULL default '0',
		UNIQUE KEY `origindex` ( `Satnum`,
		`Year`,`DayOfYear`,`Ndotby2`,`Nddotby6`,`Bstar`,
		`Inclination`,`RAAN`,`Eccentricity`,`ArgPer`,`MeanAnomaly`,`MeanMotion`,`RevNum`),
		UNIQUE KEY `Idnum` (`Idnum`)
		) ENGINE=MyISAM DEFAULT CHARSET=latin1";

	$dbh->do($create) unless $testingflag;

	$dbh->do("DROP TABLE IF EXISTS $computeddb") unless $testingflag;

	$create = "CREATE TABLE `$computeddb` (
		`Idnum` int(11) NOT NULL default '0',
		`Satnum` int(5) NOT NULL default '0',
		`BC` double NOT NULL default '0',
		`SemiMajorAxis` double NOT NULL default '0',
		`ApogeeHeight` double NOT NULL default '0',
		`PerigeeHeight` double NOT NULL default '0',
		`Period` double NOT NULL default '0',
		`MeanMotionRPS` double(20,14) NOT NULL default '0.00000000000000',
		`EccentricAnomaly` double NOT NULL default '0',
		`TrueAnomaly` double NOT NULL default '0',
		`EpochYear` int(4) NOT NULL default '0',
		`EpochMonth` int(2) NOT NULL default '0',
		`EpochDay` int(2) NOT NULL default '0',
		`EpochHour` int(2) NOT NULL default '0',
		`EpochMinute` int(2) NOT NULL default '0',
		`EpochSec` float NOT NULL default '0',
		`ModJulianDate` double NOT NULL default '0',
		UNIQUE KEY `Idnum` (`Idnum`)
		) ENGINE=MyISAM DEFAULT CHARSET=latin1";

	$dbh->do($create) unless $testingflag;

	$dbh->do("DROP TABLE IF EXISTS $osculatingdb") unless $testingflag;

	$create = "CREATE TABLE `$osculatingdb` (
		`Idnum` int(11) NOT NULL default '0',
		`Satnum` int(5) NOT NULL default '0',
		`ModJulianDate` double NOT NULL default '0',
		`SemiMajorAxis` double NOT NULL default '0',
		`Eccentricity` double NOT NULL default '0',
		`Inclination` double NOT NULL default '0',
		`ArgPer` double NOT NULL default '0',
		`RAAN` double NOT NULL default '0',
		`MeanAnomaly` double NOT NULL default '0',
		`EccentricAnomaly` double NOT NULL default '0',
		`TrueAnomaly` double NOT NULL default '0',
		`Delaunay_capL` double NOT NULL default '0',
		`Delaunay_capG` double NOT NULL default '0',
		`Delaunay_capH` double NOT NULL default '0',
		`Delaunay_l` double NOT NULL default '0',
		`Delaunay_g` double NOT NULL default '0',
		`Delaunay_h` double NOT NULL default '0',
		`Equinoctial_a` double NOT NULL default '0',
		`Equinoctial_P1` double NOT NULL default '0',
		`Equinoctial_P2` double NOT NULL default '0',
		`Equinoctial_Q1` double NOT NULL default '0',
		`Equinoctial_Q2` double NOT NULL default '0',
		`Equinoctial_theta` double NOT NULL default '0',
		UNIQUE KEY `Idnum` (`Idnum`)
		) ENGINE=MyISAM DEFAULT CHARSET=latin1";

	$dbh->do($create) unless $testingflag;

} elsif ($cleartables && $startsat > 1) {

	print "Removing any data from the databases for satellite #$startsat onward....\n";

	# WARNING: ALL DATA FOR Satnum >= $startsat WILL BE LOST.

	$dbh->do("DELETE FROM $dbname WHERE Satnum >= $startsat") unless $testingflag;
	$dbh->do("DELETE FROM $computeddb WHERE Satnum >= $startsat") unless $testingflag;
	$dbh->do("DELETE FROM $osculatingdb WHERE Satnum >= $startsat") unless $testingflag;

}	

my ($satnum,$satnum_1,$classification,$classification_1,$intldesig,$IDLaunchYear,$IDLaunchNum,$IDPieceNum);
my ($yy,$ddd,$ndotby2,$nddotby6,$bstar,$ephemtype,$ephemtype_1,$elnum,$elnum_1);
my ($satnum2,$incl,$raan,$ecc,$argper,$ma,$n,$revnum);
my ($linenum,$ndotby2_1,$nddotby6_1,$bstar_1);
my ($BC,$semimaj,$hp,$ha,$E,$nu,$period,$n_rps,$temp);
my ($year,$doy,$hh,$hour,$mm,$min,$sec,$month,$day,$juldate,$mjd,$startjul,$span,$freq);

my ($dbflag,$currentline,$prevline,$satcheck,$datafile,$datafile2,$datafile3,$load,@data,@data2,@data3);

my ($t,$debugging,$blflag);

my ($semimaj_osc,$ecc_osc,$i_osc,$argper_osc,$RAAN_osc,$MA_osc,$E_osc,$nu_osc);
my ($capL,$capG,$capH,$l,$g,$h);
my ($a_equi,$P1,$P2,$Q1,$Q2,$theta_equi);

my $idnum = 0; # initialize
my $idnum_N = "\\N";
my $default = "NotAvailable";
my $idcheckflag = 1;
my $skipflag = 0;
my $executedflag = 0;
my $check;
my $reccount = 0;

foreach $subdir (@subdirs) {
    opendir(DIR,$subdir) or die "can't opendir $subdir: $!";
    
    @files = ();
    @files = map { $_->[1] } 		# extract pathnames
              sort { $a->[0] cmp $b->[0] }	# sort filenames
              map { [ $_, "$subdir/$_" ] }	# form (name,path)
              # Use this search to parse through all satellites
              grep { /$fileexp/ }		# make sure filenames fit correct pattern
              # Use this search to find a particular satellite
              #grep { /^S18361$/ }
    readdir(DIR);			# all files remaining
    closedir(DIR);
    
    print LOGFILE "\nFound the following input files in directory $subdir:\n"; 
    foreach $file (@files) {
	print LOGFILE "$file\n";
    }
    print LOGFILE "\n";
    
    my @check = $dbh->selectrow_array("SELECT max(Idnum) FROM $dbname") unless $testingflag;
    $idnum = $check[0] || 0;
    
    # This opens a data dump file that is used to directly load data into the database
    
    $datafile = "$dir/".$dbname.".csv";
    open(DATA, ">$datafile") || warn "Unable to open $datafile for writing.\n";
    $datafile2 = "$dir/".$computeddb.".csv";
    open(DATA2, ">$datafile2") || warn "Unable to open $datafile2 for writing.\n";
    
    if ($m2oflag) {
	$datafile3 = "$dir/".$osculatingdb.".csv";
	open(DATA3, ">$datafile3") || warn "Unable to open $datafile3 for writing.\n";
    }
    
    # Parse the individual TLE files in each subdirectory 
    
    foreach $file (@files) {
	
	$executedflag = 1;
	
	#($satcheck = $file) =~ s/^.*S0{0,5}(\d*).*/$1/; # Find satnumber in filename
	
	#if ($satcheck < $startsat) {  $skipflag = 1; next; } # Check if we should skip over this file
	#else { $skipflag = 0; $satnum = 1}
	
	open(FID,$file) or die "Can't open $file: $!";
	
	print "Processing $file\n";
	
	$count = 0; # This keeps track of which TLE we are processing for a given satellite
	$startjul = 0; # This variable is set to the modified Julian date of the first TLE of record for a given satellite
	$dbflag = 1; # Assume that the data is good and we will write it to the database
	$intldesig = '';
	print "Start idnum of this rawdata: ".($idnum+1)."\n";
	
	while (<FID>) {
	    
	    chomp; # remove newline character
	    $currentline = $_;
	    
	    print "$currentline\n" if $testingflag;
	    
	    if ($currentline eq "") { next; }
	    
	    $linenum = substr($currentline,0,1); #note, indexing starts at 0 not 1
	    if (($check = $linenum) =~ /^[a-zA-Z].*/) { $linenum = 3 }
	    elsif (($check = $linenum) =~ /^[=<> ].*/) { next; };
	    
	    if ($linenum == 1 && length($currentline) < 43) { # this length is rather arbitrary
		print ERRLOG $currentline." TLE line length check failed.\n"; 
		$dbflag = 0;
		next;
	    }
	    if ($linenum == 2 && length($currentline) < 43) { # more important for line 2
		print ERRLOG $prevline.$currentline." TLE line length check failed.\n"; 
		$dbflag = 0;
		next;
	    }

	    print ERRLOG "LINENUM = $linenum\n" if $debug;
	    print ERRLOG "DBFLAG = $dbflag\n" if $debug;
	    
	    if ($linenum == 1) {

		print ERRLOG "PROCESSING LINE 1\n" if $debug;
		
		$idnum = $idnum + 1;
		push(@data,$idnum);
		push(@data2,$idnum);
		if ($m2oflag) { push(@data3,$idnum); }

		print ERRLOG "Extracting SATNUM\n" if $debug;
		
		if (length($currentline) >= 7) { 
		    $satnum_1 = trim(substr($currentline,2,5));
		    if ($satnum_1 =~ /^\d+$/) { # make sure satnum only has numbers in it, if not, then don't write to the database
			$satnum = $satnum_1;
			push(@data,$satnum);
		    } else {  
			$dbflag = 0; # ill formed data - don't write to database
			#print ERRLOG $prevline.$currentline." Satnum check failed.\n"; 
			print ERRLOG $currentline." Satnum check failed.\n"; 
			next; 
		    }
		} else {
		    $dbflag = 0; # ill formed data - don't write to database
		    #print ERRLOG $prevline.$currentline."Satnum length check failed.\n"; 
		    print ERRLOG $currentline."Satnum length check failed.\n"; 
		    next; 
		}

		print ERRLOG "Extracting CLASSIFICATION\n" if $debug;
		
		if (length($currentline) >= 8) { 
		    $classification_1 = trim(substr($currentline,7,1));
		    if ($classification_1 ne "") {
			$classification = $classification_1;
		    }
		    else { $classification = 0; }
		    push(@data,$dbh->quote($classification));
		} else {
		    $dbflag = 0; # ill formed data - don't write to database
		    #print ERRLOG $prevline.$currentline."Classification length check failed.\n"; 
		    print ERRLOG $currentline."Classification length check failed.\n"; 
		    next; 
		}
		
		print ERRLOG "Extracting INTERNATIONAL DESIGNATOR\n" if $debug;

		if (length($currentline) >= 17) { 
		    $temp = trim(substr($currentline,9,8));
		    if ($intldesig eq '' || $intldesig eq $default || $intldesig ne $temp) { 
			$intldesig =  $temp || $default;
		    }
		    push(@data,$dbh->quote($intldesig));
		    
		    if ($intldesig ne $default && !($intldesig =~ /^[ a-zA-Z]+$/)) {
			($IDLaunchYear = $intldesig) =~ s/^(\d{2}).*$/$1/;
			($IDLaunchNum = $intldesig) =~ s/^\d{2}\d{0,3}\s*?(\d{1,3}).*$/$1/;
			if ($IDLaunchNum =~ m/$intldesig/) {
			    ($IDLaunchNum = $intldesig) =~ s/^\d{2}[ a-zA-Z\-_]{0,3}\s*?(\d+)$/$1/;
			    if ($IDLaunchNum =~ m/$intldesig/) { # Last resort assign error value
				$IDLaunchNum = "\\N";
			    }
			}
			($IDPieceNum = $intldesig) =~ s/^\d{2}\d{0,3}\s*?\d{0,3}\s*?([a-zA-Z\-_]{0,3})$/$1/;
			if ($IDPieceNum =~ m/$intldesig/) {
			    ($IDPieceNum = $intldesig) =~ s/^\d{2}([ a-zA-Z\-_]{0,3})\s*?\d+$/$1/;
			    if ($IDPieceNum =~ m/$intldesig/) { # Last resort assign error value
				$IDPieceNum = " ";
			    }
			}
		    } else {
			$IDLaunchYear = "\\N";
			$IDLaunchNum = "\\N";
			$IDPieceNum = " ";
		    }
		    
		    push(@data,$IDLaunchYear);
		    push(@data,$IDLaunchNum);
		    push(@data,$dbh->quote($IDPieceNum));
		} else {
		    $dbflag = 0; # ill formed data - don't write to database
		    #print ERRLOG $prevline.$currentline."International designator length check failed.\n"; 
		    print ERRLOG $currentline."International designator length check failed.\n"; 
		    next; 
		}
		
		print ERRLOG "Extracting EPOCH\n" if $debug;

		if (length($currentline) >= 33) { 
		    $temp = trim(substr($currentline,18,15));
		    $temp =~ s/ /0/; $temp =~ s/ /0/; # sometimes zeros are blanks
		    $_ = $temp;
		    ($yy,$ddd) = /(\d{2})(\d{3}\.\d{0,8})/;
		    if (defined($yy) && defined($ddd)) {
			push(@data,$yy);
			push(@data,$ddd);
		    } else {
			$dbflag = 0; # ill formed data - don't write to database
			#print ERRLOG $prevline.$currentline." Epoch check failed.\n"; 
			print ERRLOG $currentline." Epoch check failed.\n"; 
			next; 
		    }
		} else {
		    $dbflag = 0; # ill formed data - don't write to database
		    #print ERRLOG $prevline.$currentline." Epoch length check failed.\n"; 
		    print ERRLOG $currentline." Epoch length check failed.\n"; 
		    next; 
		}
		
		print ERRLOG "Extracting NDOTBY2\n" if $debug;

		if (length($currentline) >= 43) { 

		    $ndotby2_1 = trim(substr($currentline,33,10));
		    if($ndotby2_1 eq "" || $ndotby2_1 =~ /^0.0+$/ ) { 
			$ndotby2 = 0; 
			push(@data,$ndotby2);
		    } else {
			if ( $ndotby2_1 =~  /^([\d+-]?)(.\d{0,8})$/ ) {
			    $ndotby2 = $1.$2; 
			    push(@data,$ndotby2);
			} else { 
			    $dbflag = 0; # ill formed data - don't write to database 
			    #print ERRLOG $prevline.$currentline." ndotby2 check failed.\n";
			    print ERRLOG $currentline." ndotby2 check failed.\n";
			    next; 
			}
		    }

		} else { 

		    print ERRLOG "LINE 1 CONTAINS NO DATA AFTER COLUMN 43\n" if $debug;
		    push(@data,0);
		    push(@data,0);
		    push(@data,0);
		    push(@data,"\\N");
		    push(@data,0);
		    next; 

		}
		
		print ERRLOG "Extracting NDDOTBY6\n" if $debug;

		if (length($currentline) >= 52) { 
		    $nddotby6_1 = trim(substr($currentline,44,8));
		    if ($nddotby6_1 eq "" || $nddotby6_1 =~ /^[+-]?0{5}[ +-]?0?$/ ) { 
			$nddotby6 = 0; 
			push(@data,$nddotby6);
		    } else {
			if ( $nddotby6_1 =~ /^([+-]?)(\d{0,5})([ +-0]\d?)$/ ) {
			    $nddotby6 =($1.".".$2)*(10**($3)); # add assumed decimal point
			    push(@data,$nddotby6);
			} else { 
			    $dbflag = 0; # ill formed data - don't write to database 
			    #print ERRLOG $prevline.$currentline." nddotby6 check failed.\n";
			    print ERRLOG $currentline." nddotby6 check failed.\n";
			    next; 
			}
		    }
		} else { 
		    print ERRLOG "LINE 1 CONTAINS NO DATA AFTER COLUMN 52\n" if $debug;
		    push(@data,0);
		    push(@data,0);
		    push(@data,"\\N");
		    push(@data,0);
		    next; 
		}
		
		print ERRLOG "Extracting BSTAR\n" if $debug;

		if (length($currentline) >= 62) { 
		    $bstar_1 = trim(substr($currentline,53,9)); # some data shifted by a space so read up to and including buffer space
		    if($bstar_1 eq "" || $bstar_1  =~ /^[+-]?0{5}[ +-]?0?$/) { 
			$bstar = 0; 
			push(@data,$bstar);
		    } else {
			if ( $bstar_1 =~ /^([ +-]?)(\d{0,5})([ +-0]\d?)$/ ) {
			    $bstar = ($1.".".$2)*(10**($3)); # add assumed decimal point
			    push(@data,$bstar);
			} else { 
			    $dbflag = 0; # ill formed data - don't write to database
			    #print ERRLOG $prevline.$currentline." bstar check failed.\n"; 
			    print ERRLOG $currentline." bstar check failed.\n"; 
			    next; 
			}
		    }
		} else { 
		    print ERRLOG "LINE 1 CONTAINS NO DATA AFTER COLUMN 62\n" if $debug;
		    push(@data,0);
		    push(@data,"\\N");
		    push(@data,0);
		    next; 
		}
		
		print ERRLOG "Extracting EPHEM TYPE\n" if $debug;

		if (length($currentline) >= 63) { 
		    $ephemtype_1 = substr($currentline,62,1);  
		    if ($ephemtype_1 ne "") {
			$ephemtype = $ephemtype_1;
		    } else { $ephemtype = "\\N"; }
		    push(@data,$dbh->quote($ephemtype)); 
		} else {
		    print ERRLOG "LINE 1 CONTAINS NO DATA AFTER COLUMN 63\n" if $debug;
		    push(@data,"\\N");
		    push(@data,0);
		    next;
		}
		
		print ERRLOG "Extracting ELSET NUMBER\n" if $debug;

		if (length($currentline) >= 68) { 
		    $elnum_1 = trim(substr($currentline,64,4));
		    if ($elnum_1 ne "") {
			$elnum = $elnum_1;
			push(@data,$elnum);
		    }
		} else { 
		    print ERRLOG "LINE 1 CONTAINS NO DATA AFTER COLUMN 68\n" if $debug;
		    $elnum = "\\N"; 
		    push(@data,$elnum);
		}
		
		$satnum2 = ""; # make sure sat num from line 2 is empty
		$prevline = $currentline."\n";

		print ERRLOG "SATNUM CLASS INTLDESIG YY DDD NDOTBY2 NDDOTBY6 BSTAR EPHNUM ELNUM\n" if $debug;
		print ERRLOG "$satnum $classification $intldesig $yy $ddd $ndotby2 $nddotby6 $bstar $ephemtype $elnum\n" if $debug;
		
	    } elsif ($linenum == 2) {

		print ERRLOG "LINE 2: DBFLAG = $dbflag\n" if $debug;

		# First line already bad, just print second line.
		if (!$dbflag) { 
		    print ERRLOG $currentline." This TLE failed in first line already.\n"; 
		    if ($twothreeflag == 2) { 
			# Reset database write flag since we are done with this TLE
			$dbflag = 1; 
		    }
		    next; 
		}

		print ERRLOG "LINE 2: TESTING SATNUM FOR MATCH WITH LINE 1\n" if $debug; 

		if (length($currentline) >= 7) {			
		    $satnum2 = trim(substr($currentline,2,5));
		    if ($satnum !~ m/$satnum2/) { 
			print "Sat numbers do not match! $satnum <> $satnum2\n"; 
			print ERRLOG $prevline.$currentline." Satnum comparison check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline."Satnum length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		print ERRLOG "Extracting INCLINATION\n" if $debug;		

		if (length($currentline) >= 17) {			
		    $incl = trim(substr($currentline,8,9)); 
		    $incl =~ s/ //; # For some reason, some of the inclination data has a space in it. This will fix it.
		    if ($incl =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/) { # make sure that the inclination is a decimal number
			push(@data,$incl);
		    } else {
			print ERRLOG $prevline.$currentline." Inclination check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." Inclination length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		print ERRLOG "Extracting RAAN\n" if $debug;

		if (length($currentline) >= 25) {			
		    $raan = trim(substr($currentline,17,8));  
		    $raan =~ s/ /0/; $raan =~ s/ /0/; # sometimes 0's are omitted so add them back
		    if ($raan =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/) { # make sure that the raan is a decimal number
			push(@data,$raan);
		    } else {
			print ERRLOG $prevline.$currentline." RAAN check failed.\n"; 
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." RAAN length check failed.\n"; 
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}

		print ERRLOG "Extracting ECCENTRICITY\n" if $debug;
		
		if (length($currentline) >= 33) {			
		    $ecc = ".".trim(substr($currentline,26,7)); # assumed decimal point added
		    if ($ecc =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/) { # make sure that the eccentricity is a decimal number
			push(@data,$ecc);
		    } else {
			print ERRLOG $prevline.$currentline." Eccentricity check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." Eccentricity length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		print ERRLOG "Extracting ARGUMENT OF THE PERIGEE\n" if $debug;

		if (length($currentline) >= 42) {
		    $argper = trim(substr($currentline,34,8));
		    $argper =~ s/ /0/; $argper =~ s/ /0/; # sometimes 0's are omitted so add them back
		    if ($argper =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/) { # make sure that the argument of perigee is a decimal number
			push(@data,$argper);
		    } else {
			print ERRLOG $prevline.$currentline." Argument of the perigee check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." Argument of the perigee length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		print ERRLOG "Extracting MEAN ANOMALY\n" if $debug;

		if (length($currentline) >= 51) {
		    $ma = trim(substr($currentline,43,8));
		    $ma =~ s/ /0/; $ma =~ s/ /0/; # sometimes 0's are omitted so add them back
		    if ($ma =~ /^-?(?:\d+(?:\.\d*)?|\.\d+)$/) { # make sure that the mean anomaly is a decimal number
			push(@data,$ma);
		    } else {
			print ERRLOG $prevline.$currentline." Mean anomaly check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." Mean anomaly length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		print ERRLOG "Extracting MEAN MOTION\n" if $debug;

		if (length($currentline) >= 63) {
		    $n = trim(substr($currentline,52,11)); 
		    $n =~ s/ /0/; # sometimes 0's are omitted so add them back
		    if ($n =~ /^[+-]?\d{0,3}\.\d{0,10}$/) { # make sure that the mean motion is a decimal number   
			push(@data,$n);
		    } else {
			print ERRLOG $prevline.$currentline." Mean motion check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." Mean motion length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		print ERRLOG "Extracting REV NUMBER\n" if $debug;

		if (length($currentline) >= 63) {
		    $revnum = trim(substr($currentline,63,5));
		    $revnum =~ s/ /0/; $revnum =~ s/ /0/; # sometimes 0's are omitted so add them back
		    if ($revnum eq "") {
			$revnum = -1; # if revnum is blank then assign -1 to indicate unavailable data
			push(@data,$revnum);
		    } elsif ($revnum =~ /^\d+$/) { # make sure that the rev number is a natural number
			push(@data,$revnum);
		    } else {
			print ERRLOG $prevline.$currentline." Rev number check failed.\n";
			$dbflag = 0; # ill formed data - don't write to database
			next;
		    }
		} else {
		    print ERRLOG $prevline.$currentline." Rev number length check failed.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		push(@data2,$satnum);
		$BC = $bstar/(2.461E-5*$RE); 
		push(@data2,$BC);
		
		$n_rps = rpd2rps($n);
		if ($n_rps != 0) {
		    $semimaj = ($MUE/($n_rps*$n_rps))**(1/3);  
		    push(@data2,$semimaj);
		    $ha = $semimaj*(1+$ecc) - $RE;  
		    push(@data2,$ha);
		    $hp = $semimaj*(1-$ecc) - $RE;  
		    push(@data2,$hp);
		    $period = 2*$PI/$n_rps;  
		    push(@data2,$period);
		    push(@data2,$n_rps); # needs to be here for proper data output order
		} else {
		    print ERRLOG $prevline.$currentline." Mean motion is zero.\n";
		    $dbflag = 0; # ill formed data - don't write to database
		    next;
		}
		
		$E = M2E(deg2rad($ma),$ecc); 
		$nu = E2nu($E,$ecc); 
		$E = rad2deg($E); $nu = rad2deg($nu);
		if ($E < 0) {$E = $E + 360;}
		if ($nu < 0) {$nu = $nu + 360;}
		push(@data2,$E);
		push(@data2,$nu);
		
		if ( $yy < 57 ) { $year = $yy + 2000; } else { $year = $yy + 1900; }
		
		$doy = floor($ddd);
		$hh = ($ddd - $doy) * 24;
		$hour = floor($hh);
		$mm = ($hh - $hour) * 60;
		$min = floor($mm);
		$sec = ($mm - $min) * 60;
		
		( $year, $month, $day ) = doy_ymd ( $year , $doy );
		
		$juldate = cal2jul ( $year,$month,$day,$hour,$min,$sec );
		$mjd = $juldate - 2400000.5;
		if ($count == 1) { $startjul = $mjd; }
		
		push(@data2,$year);
		push(@data2,$month);
		push(@data2,$day);
		push(@data2,$hour);
		push(@data2,$min);
		push(@data2,$sec);
		push(@data,$mjd);
		push(@data2,$mjd);
		
		print ERRLOG "PRINTING DATA TO CSV FILE\n" if $debug;

		# Theoretically, if we got to this point, all the data should be
		# in an acceptable format and there should not be any computational warnings
		
		print DATA join($separg,@data)."\n";
		$reccount++;
		
		print DATA2 join($separg,@data2)."$separg\n";
		
		if ($m2oflag) {
		    
		    print ERRLOG "COMPUTING EXTRA INFORMATION\n" if $debug;

		    push(@data3,$satnum);
		    push(@data3,$mjd);
		    
		    # convert from mean elements to osculating elements
		    
		    ($semimaj_osc,$ecc_osc,$i_osc,$argper_osc,$RAAN_osc,$MA_osc,$blflag) = brouwer_lyddane($semimaj,$ecc,
		                                                       deg2rad($incl),deg2rad($argper),deg2rad($raan),deg2rad($ma));
		    
		    push(@data3,$semimaj_osc);
		    push(@data3,$ecc_osc);
		    push(@data3,rad2deg($i_osc));
		    push(@data3,rad2deg($argper_osc));
		    push(@data3,rad2deg($RAAN_osc));
		    push(@data3,rad2deg($MA_osc));

		    # BLFLAG = 0 means that the object was within 0.1 degrees of the critical inclinations or to 0 or pi
		    # where the BL theory breaks down.
		    if ($blflag == 1) {
			if ($ecc_osc < 1) {
			    $E_osc = M2E(deg2rad($MA_osc),$ecc_osc); 
			    $nu_osc = E2nu(deg2rad($E_osc),$ecc_osc); 
			    $E_osc = rad2deg($E_osc); 
			    $nu_osc = rad2deg($nu_osc);
			    if ($E_osc < 0) {$E_osc = $E_osc + 360;}
			    if ($nu_osc < 0) {$nu_osc = $nu_osc + 360;}
			    push(@data3,$E_osc);
			    push(@data3,$nu_osc);
			} else {
			    push(@data3,-999999);
			    push(@data3,-999999);
			}
		    } else {
			push(@data3,0);
			push(@data3,0);
		    }	
		    
		    if ($ecc_osc < 1) {
			# compute Delaunay elements
			($capL,$capG,$capH,$l,$g,$h) = orb2delaunay($semimaj_osc,$ecc_osc,deg2rad($i_osc),
								    deg2rad($argper_osc),deg2rad($RAAN_osc),deg2rad($MA_osc));
			
			push(@data3,$capL);
			push(@data3,$capG);
			push(@data3,$capH);
			push(@data3,$l);
			push(@data3,$g);
			push(@data3,$h);
			
			# compute Equinoctial elements
			($a_equi,$P1,$P2,$Q1,$Q2,$theta_equi) = orb2equi($semimaj_osc,$ecc_osc,deg2rad($i_osc),
									 deg2rad($argper_osc),deg2rad($RAAN_osc),deg2rad($MA_osc));
			
			push(@data3,$a_equi);
			push(@data3,$P1);
			push(@data3,$P2);
			push(@data3,$Q1);
			push(@data3,$Q2);
			push(@data3,$theta_equi);
			
			print DATA3 join($separg,@data3)."$separg\n";
			
		    } else {
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
			push(@data3,-999999);
		    }	
		    
		}
		
		$count = $count + 1;
		
		if ($reccount == $numrec) {
		    
		    close(DATA);
		    close(DATA2);
		    if ($m2oflag) { close(DATA3); }
		    
		    print "Inserting data into database...\n";
		    
		    if (!$skipflag && !$testingflag) {
			
			system("mysqlimport -L -$oflag --host=$host --fields-terminated-by=\"$separg\" --fields-optionally-enclosed-by=\"'\" --fields-escaped-by='\\' --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql tle $datafile") == 0 or die "system failed: $?";
			system("mysqlimport -L -$oflag --host=$host --fields-terminated-by=\"$separg\" --fields-optionally-enclosed-by=\"'\" --fields-escaped-by='\\' --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql tle $datafile2") == 0 or die "system failed: $?";
			if ($m2oflag) { system("mysqlimport -L -$oflag --host=$host --fields-terminated-by=\"$separg\" --fields-optionally-enclosed-by=\"'\" --fields-escaped-by='\\' --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql tle $datafile3") == 0 or die "system failed: $?"; }
			
			$dbh->do("analyze table $dbname");
			$dbh->do("analyze table $computeddb");
			if ($m2oflag) { $dbh->do("analyze table $osculatingdb"); }
			
		    }
		    
		    # Delete the datafiles so that we don't exceed disk quota
		    
		    unlink($datafile) unless $testingflag;
		    unlink($datafile2) unless $testingflag;
		    if ($m2oflag) { unlink($datafile3) unless $testingflag; }
		    
		    open(DATA, ">$datafile") || warn "Unable to open $datafile for writing.\n";
		    open(DATA2, ">$datafile2") || warn "Unable to open $datafile2 for writing.\n";
		    if ($m2oflag) { open(DATA3, ">$datafile3") || warn "Unable to open $datafile3 for writing.\n"; }
		    
		    $reccount = 0;
		    print "\tBack to processing $file\n";
		    
		}
		
		if ($twothreeflag == 2) {
		    
		    @data = (); # Initialize
		    @data2 = (); # Initialize
		    if ($m2oflag) { @data3 = (); } # Initialize
		    
		    $prevline = $currentline."\n";
		    
		}
		
		
	    } elsif ($linenum == 3 && $twothreeflag == 3) { 
		
		print ERRLOG "PROCESSING LINE 3\n" if $debug;

		@data = (); # Initialize
		@data2 = (); # Initialize
		if ($m2oflag) { @data3 = (); } # Initialize
		
		$prevline = $currentline."\n";
		
		if (!$dbflag) { 
		    print ERRLOG $currentline." This is the third line of a 3-line TLE\n"; # ouput third line to logfile 
		    $dbflag = 1; # reset dbflag
		    $idnum = $idnum - 1; # back up counter since this one is not going in the database
		}	
	    }	
	}
	
	$dbh->do("UPDATE satdata SET NumTLEs = $count WHERE Satnum = $satnum");
	if ($count > 1 ) {
	    $span = $mjd - $startjul;
	} else {
	    $span = 1;
	}
	if ($span != 0) {
	    $freq = $count / $span; 
	    $dbh->do("UPDATE satdata SET UpdateFreq = $freq WHERE Satnum = $satnum");
	}
	
	print LOGFILE "Processed $file\n";
	
    }
    
    # Now we are ready to send the data to the database.
    
    if ($executedflag && $skipflag == 0 && $testingflag == 0) {
	
	print "Inserting data into database...\n";
	
	close(DATA);
	close(DATA2);
	if ($m2oflag) { close(DATA3); }
	
	system("mysqlimport -L -$oflag --host=$host --fields-terminated-by=\"$separg\" --fields-optionally-enclosed-by=\"'\" --fields-escaped-by='\\' --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql tle $datafile") == 0 or die "system failed: $?";
	system("mysqlimport -L -$oflag --host=$host --fields-terminated-by=\"$separg\" --fields-optionally-enclosed-by=\"'\" --fields-escaped-by='\\' --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql tle $datafile2") == 0 or die "system failed: $?";
	if ($m2oflag) { system("mysqlimport -L --host=$host -$oflag --fields-terminated-by=\"$separg\" --fields-optionally-enclosed-by=\"'\" --fields-escaped-by='\\' --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql tle $datafile3") == 0 or die "system failed: $?"; }
	
	# This is done to keep the table indexing organized as we keep adding data
	# Theoretically, this should speed up data insertion
	
	$dbh->do("analyze table $dbname");
	$dbh->do("analyze table $computeddb");
	if ($m2oflag) { $dbh->do("analyze table $osculatingdb"); }
	
    } elsif (!$executedflag) {
	
	print "\nNo TLE files found.  Nothing was inserted into the database.\n"; 
	print ERRLOG "\nNo TLE files found.  Nothing was inserted into the database.\n"; 
	print LOGFILE "\nNo TLE files found.  Nothing was inserted into the database.\n"; 
    }
    
    # Delete the datafiles so that we don't exceed disk quota
    
    unlink($datafile) unless $testingflag;
    unlink($datafile2) unless $testingflag;
    if ($m2oflag) { unlink($datafile3) unless $testingflag; }
    
}

##################################################################
#
#                      Clean up the computed table
#
# Because there is no easy way to link the mass insertion of
# data into the $dbname and $computeddb tables, we need to
# clean up the $computeddb table.  When data is inserted into
# $dbname, it is checked against a unique key.  Duplicate data is
# either ignored or overwritten depending upon the options selected
# when running this code.  This can mean that there will be data in
# the $computeddb table that does not correspond to anything in the
# $dbname table. The following query finds these rows and then
# deletes them from the $computeddb table.
#
#

print "\nCleaning up the $computeddb table. This could take a minute...\n";
$dbh->do("repair table $computeddb") or die $dbh->errstr();
$dbh->do("repair table $dbname") or die $dbh->errstr();
my $query = "delete from $computeddb using $computeddb left join $dbname on $computeddb.Idnum = $dbname.Idnum where $dbname.Idnum IS NULL"; 
my $numrows = $dbh->do($query) or die $dbh->errstr();

my $numrow = $numrows > 0 ? $numrows : 0;

print "Finished cleaning up $computeddb.  Number of rows affected: $numrows\n";
print "\nExiting TLE2db.com...\n";

print "\nCleaning up the $osculatingdb table. This could take a minute...\n";
$dbh->do("repair table $osculatingdb") or die $dbh->errstr();
$dbh->do("repair table $dbname") or die $dbh->errstr();
my $query = "delete from $osculatingdb using $osculatingdb left join $dbname on $osculatingdb.Idnum = $dbname.Idnum where $dbname.Idnum IS NULL"; 
my $numrows = $dbh->do($query) or die $dbh->errstr();

my $numrow = $numrows > 0 ? $numrows : 0;

print "Finished cleaning up $osculatingdb.  Number of rows affected: $numrows\n";
print "\nExiting TLE2db.com...\n";

$dbh->disconnect();
close(ERRLOG);
close(LOGFILE);

# This subroutine trims off leading and trailing blanks
sub trim {
    
    my @out = @_;
    for (@out) {
	s/^\s+//; #trim left
	s/\s+$//; #trim right
    }
    return @out == 1
	? $out[0] # only one to return
	: @out;   # or many
    
}
