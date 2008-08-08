#!/usr/bin/perl -w
#
#
#  VERSION 3
#
#  This version recursively searches the cddis.gsfc.nasa.gov 
#  directory structure as dumped from their ftp site. 
#  The normal point files contain the space object identification
#  number unlike the predict files. So, all that needs to be done
#  is compile a list of files to upload to the database. A flag
#  has been added to search for the most recent data added to
#  the database and only add more recent data to avoid excess
#  processing.
#
#  Insertion of predict files has been incorporated into this version.
#

BEGIN {
    push @INC, "/home/mwilkins/include";
    push @INC, "/home/mwilkins/DBD-mysql-4.001/lib64/perl5/site_perl/5.8.5/x86_64-linux-thread-multi";
}

use strict; # similar to implicit none of FORTRAN
use Dates; #date functions
use FileHandle; # For autoflush
use DBI; # Required for database commands
use Localmath; 
use Tie::File; # Required for efficient opening of files
use POSIX qw(floor ceil);
use File::Find;
use Time::localtime;
use Regexp::Common;

# Set program defaults

my $start_epoch = "20060901000000.0";
my $end_epoch   = "20060831000000.0";
my $testingflag = 0;
my $startsat = 1; # start processing from this space object number
my $cleartables = 1;
my $onlysats = 0; # flag to only process specific satellites
my $tablename1 = "NormalPointData"; 
my $tablename2 = "SampledEngineeringData"; 
my $tablename3 = "PredictData";
my $dbname = "SLR";
my @sats = ();
my $separg = ";";
my $slr_folder = "/home/mwilkins/SLR2db/cddis.gsfc.nasa.gov/slr/data/npt/";
my $slr_predict_folder = "/home/mwilkins/SLR2db/cddis.gsfc.nasa.gov/slr/predicts/";
my $slr_file = "";
my $fileflag = 0;
my $backup = 0;
my $debug = 0;
my $currentdataflag = 0;
my $monthlyfileflag = 0;
my $predictfileflag = 0;

@ARGV >= 1 or die "Usage: SLR2db2.com -[options] 
      --startsat=[Starting Catalog Number]
      --folder=[SLR Data Folder to Process]
      --predictfolder=[SLR Predict Folder to Process]
      --file=[Specific SLR Data File to Process]
      --db=[Name of SLR Database]
      --tb1=[Name of Table to Store Normal Point SLR Data]
      --tb2=[Name of Table to Store Sampled Engineering SLR Data]
      --tb3=[Name of Table to Store Predict Data]
      --se=[Starting Epoch YYYYDDMMHHMMSS.S]
      --ee=[Ending Epoch YYYYDDMMHHMMSS.S]  
      --satnums=[Specify Exact Satnums to Process]

                [options]
                -b Backup databases before modification
                -w Write new fitwindows database, old database will be discarded
                -a Append to current fitwindows database
                -t Testing mode: Execute code without executing database insertions
                -d Run script with default settings, no other options need be specified
                -c Only process current data
                -m Only process monthly data files
                -p Process predict files instead of data files

                [defaults]
                --folder        $slr_folder
                --predictfolder $slr_predict_folder
                --file          $slr_file
                --db            $dbname
                --tb1           $tablename1
                --tb2           $tablename2
                --tb3           $tablename3
                --startsat      $startsat
                --se            $start_epoch  
                --ee            $end_epoch\n";

my $arg;

# Parse through arguments

ARGUMENT: foreach $arg (@ARGV) {

        if ($arg =~ /^-([a-z]+)/) {

                my $list = $1;

                while ($list =~ /(.)/g) {

                        if ($list eq "d") { # Use default settings

                                last ARGUMENT;

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

                         } elsif ($list eq "c") {

                                ###########################################
                                #
                                #     Current Data Insertion Flag
                                #
                                # Set this flag to 1 if you just
                                # want to add the most current data
			        # available.
                                #

                                $currentdataflag = 1;

                        } elsif ($list eq "p") {

                                ###########################################
                                #
                                #     Predict File Flag
                                #
                                # Set this flag to 1 if you want
                                # to process SLR predicts instead
			        # of the regular data files.
                                #

                                $predictfileflag = 1;

                        } elsif ($list eq "m") {

                                ###########################################
                                #
                                #     Monthly File Flag
                                #
                                # Set this flag to 1 if you just
                                # want to process the monthly roll-up files
			        # This could exclude daily files not yet 
			        # added or other special files.
                                #

                                $monthlyfileflag = 1;

                        } else {

                                die "Invalid option specified.  Type genobs.com for usage.\n Died ";

                        }

                }

        } elsif ($arg =~ /^--folder[\s*=?\s*](.*)/) {

                ###########################################
                #
                # Folder name for SLR data
                #

                $slr_folder= $1;

         } elsif ($arg =~ /^--predictfolder[\s*=?\s*](.*)/) {

                ###########################################
                #
                # Folder name for SLR predicts
                #

                $slr_predict_folder= $1;

        } elsif ($arg =~ /^--file[\s*=?\s*](.*)/) {

                ###########################################
                #
                # File name for SLR data
                #

                $slr_file= $1;
		$fileflag = 1;

        } elsif ($arg =~ /^--db[\s*=?\s*]([a-zA-Z][a-zA-Z0-9]*)/) {

                ###########################################
                #
                # Database name for SLR data
                #

                $dbname = $1;

        } elsif ($arg =~ /^--tb1[\s*=?\s*]([a-zA-Z][a-zA-Z0-9_-]*)/) {

                ###########################################
                #
                # Table name for normal point data
                #

                $tablename1 = $1;

        } elsif ($arg =~ /^--tb2[\s*=?\s*]([a-zA-Z][a-zA-Z0-9_-]*)/) {

                ###########################################
                #
                # Table name for sampled engineering data
                #

                $tablename2 = $1;

        } elsif ($arg =~ /^--tb3[\s*=?\s*]([a-zA-Z][a-zA-Z0-9_-]*)/) {

                ###########################################
                #
                # Table name for predict data
                #

                $tablename3 = $1;

        } elsif ($arg =~ /^--se[\s*=?\s*](\d{14}(?:\.\d*)?)/) {

                ###########################################
                #
                # Starting epoch
                #

                $start_epoch = $1;

        } elsif ($arg =~ /^--ee[\s*=?\s*](\d{14}(?:\.\d*)?)/) {

                ###########################################
                #
                # Ending epoch
                #

                $end_epoch = $1;

        } elsif ($arg =~ /^--startsat[\s*=?\s*]([0-9]+)/) {

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

        } elsif ($arg =~ /^--satnums[\s*=?\s*]((?:[0-9]+,?)+)/) {

                ###########################################
                #
                #     Specific Space Objects to Process
                #
                # Set this flag to the satellite numbers   
                # for which you wish to specifically input data.
                # Note that all current data in the table
                # for these satellites will be deleted.
                #              

                @sats = split(/,/,$1);

                foreach my $sat (@sats) { print "Will process space object #$sat\n"; }

                if (@sats >= 1) {
                        $cleartables = 0; # Failsafe
                        $onlysats = 1;
                }

        } else {

                die "Invalid argument $arg specified.  Type SLR2db3.com for usage.\n Died ";

        }
 
}
 
print "SLR data folder to search: $slr_folder\n" unless $fileflag || $predictfileflag;
print "SLR predict folder to search: $slr_predict_folder\n" unless $fileflag;
print "SLR data file to process: $slr_file\n" if $fileflag;
print "SLR Database name: $dbname\n";
print "Normal Point Data table name: $tablename1\n" unless $predictfileflag;
print "Sampled Engineering Data table name: $tablename2\n" unless $predictfileflag;
print "Predict Data table name: $tablename3\n";
print sprintf("Testing: %s\n",$testingflag ? "True" : "False");
print sprintf("Write/Append: %s\n",$cleartables ? "Writing" : "Appending");
print sprintf("Backup databases: %s\n",$backup ? "True" : "False");
print "Starting space object: $startsat\n";

###########################################
#
#     Backup Databses
#
 
if ($backup) {

        print "\nBacking up databases.  This may take a few minutes!\n";
        system("cp /usr/local/mysql/5.0.27/data/$dbname/$tablename1.* /home/mwilkins/backup/data") == 0 or die "Unable to backup databases. Died";
        system("cp /usr/local/mysql/5.0.27/data/$dbname/$tablename2.* /home/mwilkins/backup/data") == 0 or die "Unable to backup databases. Died";
        system("cp /usr/local/mysql/5.0.27/data/$dbname/$tablename3.* /home/mwilkins/backup/data") == 0 or die "Unable to backup databases. Died";

}


##############################################################
#                                                            #
# Connect to TLE Database and Retrieved Required Data        #
#                                                            #
##############################################################

print "Connecting to the database....\n";
 
# localhost -> fastest but only available if both mysql server and script are executed on same node
my $dbh = DBI->connect('dbi:mysql:SLR:localhost:3306:1',
# ho323n6
#my $dbh = DBI->connect('dbi:mysql:mysql_socket=/home/mwilkins/mysql/tmp/mysql.sock:$dbname:140.31.196.197:3306:1',
# ho612n6
#my $dbh = DBI->connect('dbi:mysql:mysql_socket=/home/mwilkins/mysql/tmp/mysql.sock:$dbname:10.128.38.70:3306:1',
            'mwilkins','I$gotmysql',
            { RaiseError => 1, AutoCommit => 1}) || die $DBI::errstr;

print "Connected....\n";
 
if ($cleartables) {

        # Create table in which to store data

    if (!$predictfileflag) {

        $dbh->do("DROP TABLE IF EXISTS $tablename1") unless $testingflag;

        my $create = "CREATE TABLE $tablename1 (
  	`ILRSSatId` int(8) NOT NULL default '0',
  	`IntlDesig` varchar(15) NOT NULL,
	`Year` int(4) default NULL,
  	`DOY` int(3) default NULL,
	`CDPPId` int(5) default NULL,
  	`CDP2DSysNum` int(2) default NULL,
  	`CDP2DOSN` int(2) default NULL,
  	`LaserWaveLength` int(4) default NULL,
  	`CalSysDelay` int(8) default NULL,
  	`CalDelayShift` int(6) default NULL,
  	`SysDelayRMS` int(4) default NULL,
  	`NPWindowIndicator` int(1) default NULL,
  	`EpochTimeScale` int(1) default NULL,
  	`SysCalMethod` int(1) default NULL,
  	`SCH` int(1) default NULL,
  	`SCI` int(1) default NULL,
  	`PassRMS` int(4) default NULL,
  	`DataQual` int(1) default NULL,
  	`FormatRevNum` int(1) default NULL,
  	`TimeofDay` double default NULL,
  	`ModJulianDate` double default NULL,
  	`TwoWayTimeofFlight` double default NULL,
  	`TwoWayBinRMS` int(7) default NULL,
  	`SurfacePressure` int(5) default NULL,
  	`SurfaceTemp` int(2) default NULL,
  	`RelativeHumidity` int(3) default NULL,
  	`NumRawRanges` int(4) default NULL,
  	`DataFlag` int(1) default NULL,
  	`NRRExponent` int(1) default NULL,
       `Idnum` int(11) NOT NULL auto_increment,
       PRIMARY KEY (`Idnum`),
       KEY DateTime (`Year`,`DOY`,`TimeofDay`),
       KEY `MJD` USING BTREE (`ModJulianDate`),
       KEY `CPPID` USING BTREE (`CDPPId`),
       KEY `IntDes` USING BTREE (`IntlDesig`)
       ) ENGINE=MyISAM DEFAULT CHARSET=latin1";

        $dbh->do($create) unless $testingflag;

        $dbh->do("DROP TABLE IF EXISTS $tablename2") unless $testingflag;

        $create = "CREATE TABLE $tablename2 (
  	`ILRSSatId` int(7) NOT NULL default '0',
  	`IntlDesig` varchar(15) NOT NULL,
	`Year` int(2) default NULL,
  	`DOY` int(3) default NULL,
  	`CDPPId` int(4) default NULL,
  	`CDP2DSysNum` int(2) default NULL,
  	`CDP2DOSN` int(2) default NULL,
  	`LaserWaveLength` int(4) default NULL,
  	`CalSysDelay` int(8) default NULL,
  	`CalDelayShift` int(6) default NULL,
  	`SysDelayRMS` int(4) default NULL,
  	`NPWindowIndicator` int(1) default NULL,
  	`EpochTimeScale` int(1) default NULL,
  	`SysCalMethod` int(1) default NULL,
  	`SCH` int(1) default NULL,
  	`SCI` int(1) default NULL,
  	`PassRMS` int(4) default NULL,
  	`DataQual` int(1) default NULL,
  	`FormatRevNum` int(1) default NULL,
  	`TimeofDay` double default NULL,
  	`ModJulianDate` double default NULL,
  	`TwoWayTimeofFlight` double default NULL,
  	`SurfacePressure` int(5) default NULL,
  	`SurfaceTemp` int(2) default NULL,
 	`RelativeHumidity` int(3) default NULL,
  	`InternalBurstCalSysDelay` int(8) default NULL,
  	`SigStrength` int(4) default NULL,
  	`AngleOrigin` int(1) default NULL,
  	`Azimuth` int(7) default NULL,
 	`Elevation` int(6) default NULL,
       `Idnum` int(11) NOT NULL auto_increment,
       PRIMARY KEY (`Idnum`),
       KEY DateTime (`Year`,`DOY`,`TimeofDay`)
       ) ENGINE=MyISAM DEFAULT CHARSET=latin1";

         $dbh->do($create) unless $testingflag;

    } else {

        $dbh->do("DROP TABLE IF EXISTS $tablename3") unless $testingflag;

        my $create = "CREATE TABLE $tablename3 (
	`Satnum` int default NULL,
	`Year` int(4) default NULL,
	`Month` int(2) default NULL,
	`Day` int(2) default NULL,
	`Hour` int(2) default NULL,
	`Min` int(2) default NULL,
	`Sec` double default NULL,
  	`ModJulianDate` double default NULL,
	`X` double default NULL,
	`Y` double default NULL,
	`Z` double default NULL,
	`XD` double default NULL,
	`YD` double default NULL,
	`ZD` double default NULL,
       `Idnum` int(11) NOT NULL auto_increment,
       PRIMARY KEY (`Idnum`),
	KEY Satnum (`Satnum`),
  	KEY DateTime (`ModJulianDate`),
	UNIQUE KEY Sat (`Satnum`,`ModJulianDate`)
	) ENGINE=MyISAM DEFAULT CHARSET=latin1";

        $dbh->do($create) unless $testingflag;

    }

} else {

#        print "Cleaning up $tablename1 table....\n";
#        if (!$onlysats) {
#                $dbh->do("delete from $tablename1 where Satnum >= $startsat");
#        } else {
#                my $query = "delete from $tablename1 where Satnum = ".join(" or Satnum = ",@sats);
#                $dbh->do($query);
#        }

#        print "Cleaning up $tablename2 table....\n";
#        if (!$onlysats) {
#                $dbh->do("delete from $tablename2 where Satnum >= $startsat");
#        } else {
#                my $query = "delete from $tablename2 where Satnum = ".join(" or Satnum = ",@sats);
#                $dbh->do($query);
#        }

#        print "Cleaning up $tablename3 table....\n";
#        if (!$onlysats) {
#                $dbh->do("delete from $tablename3 where Satnum >= $startsat");
#        } else {
#                my $query = "delete from $tablename3 where Satnum = ".join(" or Satnum = ",@sats);
#                $dbh->do($query);
#        }

}

# This opens a log that tracks which satellites have been processed

my $logfile = "SLR2db3.log";

if (-r $logfile && -w $logfile ) {
        open(LOGFILE, ">>$logfile") || warn "Unable to open logfile for appending.\n";
}
else {
        open(LOGFILE, ">$logfile") || warn "Unable to open logfile for writing.\n";
}

# These need to be the same name as the table you are going to insert data
# into or mysqlimport will complain

my $file1 = "/home/mwilkins/SLR2db/$tablename1";
my $file2 = "/home/mwilkins/SLR2db/$tablename2";
my $file3 = "/home/mwilkins/SLR2db/$tablename3";

my @outfile1 = (); 
my @outfile2 = ();
my @outfile3 = (); 

if (!$predictfileflag) {
    
    if (-e $file1) {
	unlink ($file1);
    }
    if (-e $file2) {
	unlink ($file2);
    }
    tie @outfile1, "Tie::File", $file1 or die "Unable to Tie::File $file1, Died";
    tie @outfile2, "Tie::File", $file2 or die "Unable to Tie::File $file2, Died";

} else {

    if (-e $file3) {
	unlink ($file3);
    }
    tie @outfile3, "Tie::File", $file3 or die "Unable to Tie::File $file3, Died";
 
}
    
# Find current day, month, year
my $tm = localtime;
my ($DAY,$MONTH,$YEAR) = ($tm->mday,$tm->mon,$tm->year);
my $yr = substr($YEAR,-2);
my $compactfulldate = sprintf("%02d",$yr).sprintf("%02d",$MONTH).sprintf("%02d",$DAY);
my $compactdate = sprintf("%02d",$yr).sprintf("%02d",$MONTH);

# Folders to skip
my @skipthesefolders = qw( sum test moon maneuvers );

# Allowable predict file extensions
my @predictfiletypes = qw( gfz atsc htsi hts csr mcc nasda esoc g926 );

##############################################################
#                                                            #
#        Find any compressed files and uncompress them       #
#                                                            #
##############################################################

print "Uncompressing files...\n" unless $fileflag;
print LOGFILE "Uncompressing files...\n" unless $fileflag;

if (!$predictfileflag) {

    find(\&uncompress_files, $slr_folder) unless $fileflag;

} else {

    find(\&uncompress_files, $slr_predict_folder) unless $fileflag;

}

##############################################################
#                                                            #
#                 Initialize file counters                   #
#                                                            #
##############################################################

# These counters are line numbers in the output files
# The first line is always zero.
my $count1 = 0;
my $count2 = 0;
my $count3 = 0;

# These counters are the record numbers of each line in the output file
# the record numbers can be anything as long as each row is distinct
# If we are appending to the existing database, then query for the last
# idnumber. Otherwise, set to 1.

my ($idnum1,$idnum2,$idnum3);

if (!$predictfileflag) {

    if ($cleartables) {
	
	$idnum1 = 1;
	$idnum2 = 1;
	
    } else {
	
	my @check = $dbh->selectrow_array("SELECT max(Idnum) FROM $tablename1");
	$idnum1 = $check[0] || 1;
	@check = $dbh->selectrow_array("SELECT max(Idnum) FROM $tablename2");
	$idnum2 = $check[0] || 1;
	
    }

} else {

    if ($cleartables) {
	
	$idnum3 = 1;
	
    } else {
	
	my @check = $dbh->selectrow_array("SELECT max(Idnum) FROM $tablename3");
	$idnum3 = $check[0] || 1;
	
    }

}
    
##############################################################
#                                                            #
# Recursively search the folder and all sub folders for      # 
# normal point data files to process. Collect the file       #
# names, including directory location into a list.           #
#                                                            #
##############################################################

print "Processing data file(s)...\n";
print LOGFILE "Processing data file(s)...\n";

if ($currentdataflag && !$predictfileflag) {

    # Download latest data. The -nc (no clobber) option only downloads files that do not
    # exist on the local file system. Since all files have been downloaded, only the most
    # recent additions will be fetched.

    system("wget -r -nc ftp://anonymous:mpwilkins\@cddis.gsfc.nasa.gov/slr/data/npt/allsat");

    find(\&process_file, "/home/mwilkins/SLR2db/cddis.gsfc.nasa.gov/slr/data/npt/allsat");

} elsif (!$currentdataflag && !$predictfileflag) {

    find(\&process_file, $slr_file) if $fileflag;

    find(\&process_file, $slr_folder) unless $fileflag;

} elsif ($currentdataflag && $predictfileflag) {

    # Download latest data. The -nc (no clobber) option only downloads files that do not
    # exist on the local file system. Since all files have been downloaded, only the most
    # recent additions will be fetched.

    system("wget -r -nc ftp://anonymous:mpwilkins\@cddis.gsfc.nasa.gov/slr/predicts");

    find(\&process_predict_file, "/home/mwilkins/SLR2db/cddis.gsfc.nasa.gov/slr/predicts");

} else {

    find(\&process_predict_file, $slr_file) if $fileflag;

    find(\&process_predict_file, $slr_predict_folder) unless $fileflag;

}


##############################################################
#                                                            #
#             Remove duplicate SLR observations              #
#                                                            #
##############################################################

print "Checking for duplicate data...\n";
print LOGFILE "Checking for duplicate data...\n";

if (!$predictfileflag) {

    my $finddups1 = "select count(*) as NDUP, GROUP_CONCAT(Idnum) as 
               DUP_IDS FROM $tablename1 GROUP BY ILRSSatID,CDPPId,
               Year, DOY, TimeofDay, TwoWayTimeofFlight HAVING NDUP > 1";

    my $finddups2 = "select count(*) as NDUP, GROUP_CONCAT(Idnum) as 
               DUP_IDS FROM $tablename2 GROUP BY ILRSSatID,CDPPId,
               Year, DOY, TimeofDay, TwoWayTimeofFlight HAVING NDUP > 1";

    my $rows1 = $dbh->selectall_arrayref($finddups1);
    my $rows2 = $dbh->selectall_arrayref($finddups2);
    
    foreach my $row (@$rows1) {
	
	print "Removing duplicate data from $tablename1...\n";
	print LOGFILE "Removing duplicate data from $tablename1...\n";
	
	my @dupIDs = split(",",$row->[1]);
	my $delete = "delete from $tablename1 where Idnum = $dupIDs[1]";
	my $sep = " or Idnum = ";
	if ($#dupIDs == 2) {
	    $delete = $delete.$sep.$dupIDs[2];
	} elsif ($#dupIDs >= 3) {
	    $delete = $delete.join($sep,@dupIDs[2..$#dupIDs]);
	}
	print "Found duplicate SLRs with IDnums = $row->[1]\n";
	$dbh->do($delete);
	
    }
    
    foreach my $row (@$rows2) {
	
	print "Removing duplicate data from $tablename2...\n";
	print LOGFILE "Removing duplicate data from $tablename2...\n";
	
	my @dupIDs = split(",",$row->[1]);
	my $delete = "delete from $tablename2 where Idnum = $dupIDs[1]";
	my $sep = " or Idnum = ";
	if ($#dupIDs == 2) {
	    $delete = $delete.$sep.$dupIDs[2];
	} elsif ($#dupIDs >= 3) {
	    $delete = $delete.join($sep,@dupIDs[2..$#dupIDs]);
	}
	print "Found duplicate SLRs with IDnums = $row->[1]\n";
	$dbh->do($delete);
	
    }

} else {

    my $finddups3 = "select count(*) as NDUP, GROUP_CONCAT(Idnum) as 
               DUP_IDS FROM $tablename3 GROUP BY Satnum,ModJulianDate,
               X,Y,Z,XD,YD,ZD HAVING NDUP > 1";

    my $rows3 = $dbh->selectall_arrayref($finddups3);
   
    foreach my $row (@$rows3) {
	
	print "Removing duplicate predict data from $tablename3...\n";
	print LOGFILE "Removing duplicate predict data from $tablename3...\n";
	
	my @dupIDs = split(",",$row->[1]);
	my $delete = "delete from $tablename3 where Idnum = $dupIDs[1]";
	my $sep = " or Idnum = ";
	if ($#dupIDs == 2) {
	    $delete = $delete.$sep.$dupIDs[2];
	} elsif ($#dupIDs >= 3) {
	    $delete = $delete.join($sep,@dupIDs[2..$#dupIDs]);
	}
	print "Found duplicate predict SLRs with IDnums = $row->[1]\n";
	$dbh->do($delete);
	
    }
    
}

##############################################################
#                                                            #
#       Close out data files for upload into database        #
#                                                            #
##############################################################

if (!$predictfileflag) {

    untie @outfile1;
    untie @outfile2;
    
    print "Loading data into SLR database...\n";
    
    system("mysqlimport -L -h localhost -P 3306 -i --fields-terminated-by=\";\" --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql $dbname $file1") == 0 or die "system failed: $?"; 
    system("mysqlimport -L -h localhost -P 3306 -i --fields-terminated-by=\";\" --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql $dbname $file2") == 0 or die "system failed: $?"; 
    
} else {

    untie @outfile3;
    print "Loading data into SLR predict database...\n";
    
    system("mysqlimport -L -h localhost -P 3306 -i --fields-terminated-by=\";\" --lines-terminated-by=\"\\n\" -umwilkins -pI\\\$gotmysql $dbname $file3") == 0 or die "system failed: $?"; 


}

print "Processing done!\n";

##############################################################
##############################################################
##############################################################
#                                                            #
#                       END OF SCRIPT                        #
#                                                            #
##############################################################
##############################################################
##############################################################


sub uncompress_files {

    # $_ is set to the basename of the file/directory
    # $File::Find::name is set to the full path
    
    ###########################################################
    #                                                         #
    #    Weed out undesirable/unprocessable files.            #
    #                                                         #
    ###########################################################

    foreach my $skiptest (@skipthesefolders) {
	if ($_ eq $skiptest) { 	
	    print LOGFILE "Skipping $File::Find::name\n"; 
	    $File::Find::prune = 1; 
	}
    }

    ###########################################################
    #                                                         #
    #                 Uncompress files                        #
    #                                                         #
    ###########################################################

    return unless -f;

    if (/.*\.Z$/) {
	
	print LOGFILE "Attempting to unzip file $_\n";
	system("gunzip -f $_" );
	return;

    }

    if (/.*\.tar$/) {
	
	print LOGFILE "Attempting to untar file $_\n";
	system("tar -xf $_" );
	return;

    }

}

sub process_file {

    # $_ is set to the basename of the file/directory
    # $File::Find::name is set to the full path
    
    ###########################################################
    #                                                         #
    #    Weed out undesirable/unprocessable files.            #
    #                                                         #
    ###########################################################

    foreach my $skiptest (@skipthesefolders) {
	if ($_ eq $skiptest) { 	
	    print LOGFILE "Skipping $File::Find::name\n"; 
	    $File::Find::prune = 1; 
	}
    }

    # This checks the folder name against the current year
    # Setting the prune flag to true tells File::Find to skip
    # the folder and all its subdirectories

    if ($currentdataflag && $_ < $YEAR) {
	$File::Find::prune = 1;
    }

    # Here we test the filename against the current date
    # The filename has the convention satname.YYMM or satname.YYMMDD
    # The daily files are rolled up into a monthly file. We can
    # exclude processing based upon the current date using the
    # following tests.

    if ($currentdataflag && /^.*\.(\d{4})$/ < $compactdate) {
	print LOGFILE "Skipping $File::Find::name\n";
	return;
    }

    if ($currentdataflag && /^.*\.(\d{6})$/ < $compactfulldate) {
	print LOGFILE "Skipping $File::Find::name\n";
	return;
    } 

    # If we only want to process the monthly files we test for the
    # satname.YYMM form of the filename.
    
    if ($monthlyfileflag && !/^.*\.(\d{4})$/) {
	print LOGFILE "Skipping $File::Find::name\n";
	return;
    }

    if (/^#+.*#+/ or /.*_.*/ or /.*\.[zZ]$/ or /.*\.tar$/) { 
	print LOGFILE "Skipping $File::Find::name\n"; 
	return; 
    }

    ###########################################################
    #                                                         #
    #                                                         #
    #                                                         #
    ###########################################################

    ###########################################################
    #                                                         #
    #    We only want to process files so this command        # 
    #    exits the subroutine if not a file                   #
    #                                                         #
    ###########################################################

    return unless -f;

    my $slr_file = $File::Find::name;
    
    my @lines = ();
    tie @lines, "Tie::File", $slr_file or die "Unable to Tie::File $slr_file, Died";
    
    #**************************************************************************
    # Translate the SLR Records and Insert into the Database
    #**************************************************************************
    print "Parsing the SLR data contained in: $slr_file\n";
    print LOGFILE "Parsing the SLR data contained in: $slr_file\n";
    print LOGFILE "Number of lines in file: ".@lines."\n\n";
    
    my $type_flag = 0;
    my $i;
    
    my ($ILRSSatID,$Year,$DayofYr,$StatNum,$CDPSysNum,$CDPSeqNum,$Wavelength);
    my ($CalSysDelay,$CalDelayShift,$RawRMS,$NPWinInd,$EpochTimeScale,$DelaySwitch);
    my ($SCH,$SCI,$PassRMS,$DataQual,$FormatRevNum);
    my ($Time,$TwoWayTOF,$BinRMS,$SurfacePressure,$SurfaceTemp,$RelativeHumidity);
    my ($NumRawRanges,$DataReleaseFlag,$RawRangeFactor);
    my ($BurstCalSysDelay,$SigStrength,$AngleOrigin,$Az,$El);
    my ($yyyy,$mm,$dd,$hour,$min,$sec,$hrfrac,$minfrac,$MJD);
    
    my @header = ();
    my @record = ();
    my @output = ();
    
    my @alpha = ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');
    
    for ($i = 0; $i < $#lines; $i++) {
	
	if ( trim($lines[$i]) eq '99999' ) { 
	    
	    # Advance to next line which is the actual header record
	    $i++;
	    
	    if (length(trim($lines[$i])) < 54) {
		print LOGFILE "Skipping bad header record in file $slr_file\n";
		print LOGFILE "Line length = ".length(trim($lines[$i])).", expecting 54\n";
		print LOGFILE "$lines[$i]\n";
		$type_flag = -1; # Skip data until new header record is read in
		next;
	    }

	    # Replace extraneous "-" with the number 0
	    $lines[$i] =~ s/-/0/g;
	    
	    # If line contains anything but numbers skip
	    if (!(trim($lines[$i] ) =~ /^\d+$/)) {
		print LOGFILE "Skipping bad header record in file $slr_file\n";
		print LOGFILE "Non-numeric data found.\n";
		print LOGFILE "$lines[$i]\n";
		next;
	    } 

	    @header = ();
	    
	    $type_flag = 1; 
	    
	    #  Handle header record
	    
	    $ILRSSatID = substr($lines[$i],0,7);
	    my $Yr = substr($lines[$i],7,2);       
	    if ( $Yr < 50 ) {
		$Year = $Yr + 2000;
	    } else {
		$Year = $Yr + 1900;
	    }
	    
	    # Figure out International Designator from ILRS Satellite Ident Format 
	    my $Year2;
	    my $Yr2 = substr($ILRSSatID,0,2);
	    if ( $Yr2 < 50 ) {
		$Year2 = $Yr2 + 2000;
	    } else {
		$Year2 = $Yr2 + 1900;
	    }
	    my $launchalpha;
	    my $index = substr($ILRSSatID,5,2);
	    
	    if ($index <= 26) {
		
		# Account for zero indexed array so subtract 1
		$launchalpha = $alpha[$index-1];
		
	    } elsif ($index > 26) {
		
		my $index2 = -1;
		
		while ($index > 26) {
		    
		    $index = $index - 26;
		    $index2++;
		    
		}			
		
		$launchalpha = $alpha[$index2].$alpha[$index];
		
	    }
	    
	    my $IntlDesig = $Year2."-".substr($ILRSSatID,2,3).$launchalpha;

	    ###########################################################
	    #                                                         #
	    #          Test IntlDesig to see if valid                 #
	    #                                                         #
	    ###########################################################

	    my $query = "select count(*) from tle.satdata where IntlDesig like \"$IntlDesig\"";
	    my @idcheck = $dbh->selectrow_array($query);
	    if ($idcheck[0] != 1) {
		print LOGFILE "Skipping bad header record in file $slr_file\n";
		print LOGFILE "ILRS Sat Id $ILRSSatID = Intl Desig $IntlDesig does not appear to be valid.";
		print LOGFILE "$lines[$i]\n";
		$type_flag = -1; # Skip data until new header record is read in
		next;
	    }

	    $DayofYr = substr($lines[$i],9,3);
	    
	    ($yyyy,$mm,$dd) = doy_ymd($Year,$DayofYr);
	    
	    $StatNum = substr($lines[$i],12,4);
	    $CDPSysNum = substr($lines[$i],16,2);
	    $CDPSeqNum = substr($lines[$i],18,2);
	    $Wavelength = substr($lines[$i],20,4);
	    $CalSysDelay = substr($lines[$i],24,8);
	    $CalDelayShift = substr($lines[$i],32,6);
	    $RawRMS = substr($lines[$i],38,4);
	    $NPWinInd = substr($lines[$i],42,1);
	    $EpochTimeScale = substr($lines[$i],43,1);
	    $DelaySwitch = substr($lines[$i],44,1);
	    $SCH = substr($lines[$i],45,1);
	    $SCI = substr($lines[$i],46,1);
	    $PassRMS = substr($lines[$i],47,4);
	    $DataQual = substr($lines[$i],51,1);
	    if ($DataQual eq " ") {
		$DataQual = 0;
	    }
	    $FormatRevNum = substr($lines[$i],54,1);
	    if ($FormatRevNum eq " ") {
		$FormatRevNum = 0;
	    }
	    
	    @header = ($ILRSSatID,$IntlDesig,$Year,$DayofYr,$StatNum,$CDPSysNum,$CDPSeqNum,$Wavelength,
		       $CalSysDelay,$CalDelayShift,$RawRMS,$NPWinInd,$EpochTimeScale,
		       $DelaySwitch,$SCH,$SCI,$PassRMS,$DataQual,$FormatRevNum);
	    
	    next;
	    
	} elsif (trim($lines[$i]) eq '88888' ) { 

	    # Advance to next line which is the actual header record
	    $i++;
	    
	    if (length(trim($lines[$i])) < 54) {
		print LOGFILE "Skipping bad header record in file $slr_file\n";
		print LOGFILE "Line length = ".length(trim($lines[$i])).", expecting 54\n";
		print LOGFILE "$lines[$i]\n";
		$type_flag = -1; # Skip data until new header record is read in
		next;
	    }

	    # Replace extraneous "-" with the number 0
	    $lines[$i] =~ s/-/0/g;
	    
	    # If line contains anything but numbers skip
	    if (!(trim($lines[$i] ) =~ /^\d+$/)) {
		print LOGFILE "Skipping bad header record in file $slr_file\n";
		print LOGFILE "Non-numeric data found.\n";
		print LOGFILE "$lines[$i]\n";
		next;
	    } 

	    @header = ();
	    
	    $type_flag = 2; 
	    
	    #  Handle header record
	    
	    $ILRSSatID = substr($lines[$i],0,7);
	    my $Yr = substr($lines[$i],7,2);
	    if ( $Yr < 50 ) {
		$Year = $Yr + 2000;
	    } else {
		$Year = $Yr + 1900;
	    }
	    
	    # Figure out International Designator from ILRS Satellite Ident Format 
	    my $Year2;
	    my $Yr2 = substr($ILRSSatID,0,2);
	    if ( $Yr2 < 50 ) {
		$Year2 = $Yr2 + 2000;
	    } else {
		$Year2 = $Yr2 + 1900;
	    }
	    my $launchalpha;
	    my $index = substr($ILRSSatID,5,2);
	    
	    if ($index <= 26) {
		
		# Account for zero indexed array so subtract 1
		$launchalpha = $alpha[$index-1];
		
	    } elsif ($index > 26) {
		
		my $index2 = -1;
		
		while ($index > 26) {
		    
		    $index = $index - 26;
		    $index2++;
		    
		}			
		
		$launchalpha = $alpha[$index2].$alpha[$index];
		
	    }
	    
	    my $IntlDesig = $Year2."-".substr($ILRSSatID,2,3).$launchalpha;

	    ###########################################################
	    #                                                         #
	    #          Test IntlDesig to see if valid                 #
	    #                                                         #
	    ###########################################################

	    my $query = "select count(*) from tle.satdata where IntlDesig like \"$IntlDesig\"";
	    my @idcheck = $dbh->selectrow_array($query);
	    if ($idcheck[0] != 1) {
		print LOGFILE "Skipping bad header record in file $slr_file\n";
		print LOGFILE "ILRS Sat Id $ILRSSatID = Intl Desig $IntlDesig does not appear to be valid.";
		print LOGFILE "$lines[$i]\n";
		$type_flag = -1; # Skip data until new header record is read in
		next;
	    }

	    
	    $DayofYr = substr($lines[$i],9,3);
	    
	    ($yyyy,$mm,$dd) = doy_ymd($Year,$DayofYr);
	    
	    $StatNum = substr($lines[$i],12,4);
	    $CDPSysNum = substr($lines[$i],16,2);
	    $CDPSeqNum = substr($lines[$i],18,2);
	    $Wavelength = substr($lines[$i],20,4);
	    $CalSysDelay = substr($lines[$i],24,8);
	    $CalDelayShift = substr($lines[$i],32,6);
	    $RawRMS = substr($lines[$i],38,4);
	    $NPWinInd = substr($lines[$i],42,1);
	    $EpochTimeScale = substr($lines[$i],43,1);
	    $DelaySwitch = substr($lines[$i],44,1);
	    $SCH = substr($lines[$i],45,1);
	    $SCI = substr($lines[$i],46,1);
	    $PassRMS = substr($lines[$i],47,4);
	    $DataQual = substr($lines[$i],51,1);
	    if ($DataQual eq " ") {$DataQual = 0;}
	    $FormatRevNum = substr($lines[$i],54,1);
	    
	    @header = ($ILRSSatID,$IntlDesig,$Year,$DayofYr,$StatNum,$CDPSysNum,$CDPSeqNum,$Wavelength,
		       $CalSysDelay,$CalDelayShift,$RawRMS,$NPWinInd,$EpochTimeScale,
		       $DelaySwitch,$SCH,$SCI,$PassRMS,$DataQual,$FormatRevNum);
	    
	    next;
	    
	}
	
	if ( $type_flag == 1 ) {
	    
	    if (length(trim($lines[$i])) < 54) {
		print LOGFILE "Skipping bad data in file $slr_file\n";
		print LOGFILE "Line length = ".length(trim($lines[$i])).", expecting 54\n";
		print LOGFILE "$lines[$i]\n";
		next;
	    }

	    # Replace extraneous "-" with the number 0
	    $lines[$i] =~ s/-/0/g;
	    
	    # If line contains anything but numbers skip
	    if (!(trim($lines[$i] ) =~ /^\d+$/)) {
		print LOGFILE "Skipping bad data in file $slr_file\n";
		print LOGFILE "Non-numeric data found.\n";
		print LOGFILE "$lines[$i]\n";
		next;
	    } 

	    @record = ();
	    @output = ();
	    
	    $Time = substr($lines[$i],0,12);
	    $Time *= 1.0e-7; # Convert to seconds from 0.1 microseconds
	    $hour = floor($Time/3600);
	    $hrfrac = $Time/3600-$hour;
	    $min = floor($hrfrac*60);
	    $minfrac = $hrfrac*60-$min;
	    $sec = $minfrac*60;
	    
	    $MJD = cal2jul($yyyy,$mm,$dd,$hour,$min,$sec) - 2400000.5;
	    
	    $TwoWayTOF = substr($lines[$i],12,12);
	    $TwoWayTOF *= 1.0e-12; # Convert from picoseconds
	    $BinRMS = substr($lines[$i],24,7);
	    $SurfacePressure = substr($lines[$i],31,5);
	    $SurfaceTemp = substr($lines[$i],36,4);
	    $RelativeHumidity = substr($lines[$i],40,3);
	    $NumRawRanges= substr($lines[$i],43,4);
	    $DataReleaseFlag = substr($lines[$i],47,1);
	    $RawRangeFactor = substr($lines[$i],48,1);
	    
	    @record = ($Time,$MJD,$TwoWayTOF,$BinRMS,$SurfacePressure,
		       $SurfaceTemp,$RelativeHumidity,$NumRawRanges,$DataReleaseFlag,$RawRangeFactor,$idnum1++);
	    
	    @output = (@header, @record);
	    $outfile1[$count1++] = join($separg,@output)."$separg\n";
	    
	} elsif ( $type_flag == 2 ) {  
	    
	    if (length(trim($lines[$i])) < 69) {
		print LOGFILE "Skipping bad data in file $slr_file\n";
		print LOGFILE "Line length = ".length(trim($lines[$i])).", expecting 69\n";
		print LOGFILE "$lines[$i]\n";
		next;
	    }

	    # Replace extraneous "-" with the number 0
	    $lines[$i] =~ s/-/0/g;
	    
	    # If line contains anything but numbers skip
	    if (!(trim($lines[$i] ) =~ /^\d+$/)) {
		print LOGFILE "Skipping bad data in file $slr_file\n";
		print LOGFILE "Non-numeric data found.\n";
		print LOGFILE "$lines[$i]\n";
		next;
	    } 

	    @record = ();
	    @output = ();
	    
	    $Time = substr($lines[$i],0,12);
	    $Time *= 1.0e-7; # Convert to seconds from 0.1 microseconds
	    
	    $hour = floor($Time/3600);
	    $hrfrac = $Time/3600-$hour;
	    $min = floor($hrfrac*60);
	    $minfrac = $hrfrac*60-$min;
	    $sec = $minfrac*60;
	    
	    $MJD = cal2jul($yyyy,$mm,$dd,$hour,$min,$sec) - 2400000.5;
	    
	    $TwoWayTOF = substr($lines[$i],12,12);
	    $TwoWayTOF *= 1.0e-12; # Convert from picoseconds
	    $SurfacePressure = substr($lines[$i],24,5);
	    $SurfaceTemp = substr($lines[$i],29,4);
	    $RelativeHumidity = substr($lines[$i],33,3);
	    $BurstCalSysDelay = substr($lines[$i],36,8);
	    $SigStrength = substr($lines[$i],44,4);
	    $AngleOrigin = substr($lines[$i],48,1);
	    $Az = substr($lines[$i],49,7);
	    $El = substr($lines[$i],56,6);
	    
	    @record = ($Time,$MJD,$TwoWayTOF,$SurfacePressure,$SurfaceTemp,$RelativeHumidity,$BurstCalSysDelay,$SigStrength,$AngleOrigin,$Az,$El,$idnum2++);
	    
	    @output = (@header, @record);
	    $outfile2[$count2++] = join($separg,@output)."$separg\n";
	    
	} elsif ( $type_flag == -1 ) {  

	    print LOGFILE "Trying to process $File::Find::name\n";
	    print LOGFILE "Error 002: Skipping this data because of bad header record.\n";
	    print LOGFILE $lines[$i]."\n";

	} else { 
	    
	    print "Trying to process $File::Find::name\n";
	    print "Error 001: Skipping this data because this line is not recognizable.\n";
	    print $lines[$i]."\n";

	    print LOGFILE "Trying to process $File::Find::name\n";
	    print LOGFILE "Error 001: Skipping this data because this line is not recognizable.\n";
	    print LOGFILE $lines[$i]."\n";
	    
	}
	
    }
    
    # Close out $slr_file so that we can open the next one for processing
    untie @lines;	
    
}

sub process_predict_file {

    # $_ is set to the basename of the file/directory
    # $File::Find::name is set to the full path
    
    ###########################################################
    #                                                         #
    #    Weed out undesirable/unprocessable files.            #
    #                                                         #
    ###########################################################

    foreach my $skiptest (@skipthesefolders) {
	if ($_ eq $skiptest) { 	
	    print LOGFILE "Skipping $File::Find::name\n"; 
	    $File::Find::prune = 1; 
	}
    }

    # This checks the folder name against the current year
    # Setting the prune flag to true tells File::Find to skip
    # the folder and all its subdirectories

    if ($currentdataflag && $_ < $YEAR) {
	$File::Find::prune = 1;
    }

    # Here we test the filename against the current date
    # The filename has the convention satname.YYMM or satname.YYMMDD
    # The daily files are rolled up into a monthly file. We can
    # exclude processing based upon the current date using the
    # following tests.

    if ($currentdataflag && /^.*\.(\d{4})$/ < $compactdate) {
	print LOGFILE "Skipping $File::Find::name\n";
	return;
    }

    if ($currentdataflag && /^.*\.(\d{6})$/ < $compactfulldate) {
	print LOGFILE "Skipping $File::Find::name\n";
	return;
    } 
  
    ###########################################################
    #                                                         #
    #                                                         #
    #                                                         #
    ###########################################################

    ###########################################################
    #                                                         #
    #    We only want to process files so this command        # 
    #    exits the subroutine if not a file                   #
    #                                                         #
    ###########################################################

    return unless -f;

    # Skip these files
    if (/^\#+.*\#+/ or /_pole_/ or /_maneuver_/ or /_drag_/ or /_tle_/  or /_sao_/ or /\.[zZ]$/ or /\.tar$/) { 
	print LOGFILE "Skipping $File::Find::name\n"; 
	return; 
    }

    my $checkftype = 0;
    foreach my $ftype (@predictfiletypes) {
	if (/$ftype$/) {
	    $checkftype = 1;
	    last;
	}
    }
    if (!$checkftype) {
	print "Skipping $File::Find::name\n";
	print "Unsupported file extension. Currently supporting: gfz, hts, htsi, atsc.\n";
	print LOGFILE "Unsupported file extension. Currently supporting: gfz, hts, htsi, atsc.\n";
	print LOGFILE "Skipping $File::Find::name\n"; 
	return;
    }
 
    my $slr_file = $File::Find::name;

    my @break = split(/_/,$_);
    my $Satname = $break[0] ? $break[0] : "NULL";

    if ($Satname eq "NULL") { 
	print "Could not determine the satellite name in the SLR predict file $slr_file. Skipping...\n";
	print LOGFILE "Could not determine the satellite name in the SLR predict file $slr_file. Skipping...\n";
	return;
    }

    my $query = "select Satnum from tle.satdata where SLR and lower(ILRSSatName) like \"$Satname%\"";
    print $query."\n" if $debug;
    my $satnumresult = $dbh->selectrow_arrayref($query);
    my $satnum = $satnumresult->[0] ? $satnumresult->[0] : "NULL";
    
    if ($satnum eq "NULL") {
	print "Could not determine the satellite number from the name $Satname in the file $slr_file. Skipping...\n";
	print LOGFILE "Could not determine the satellite number from the name $Satname in the file $slr_file. Skipping...\n";
	return;
    }

    my @SLRlines = ();
    tie @SLRlines, "Tie::File", $slr_file or die "Unable to Tie::File $slr_file, Died";
    
    #**************************************************************************
    # Translate the SLR Records and Insert into the Database
    #**************************************************************************
    print "Parsing the SLR predict data contained in: $slr_file\n";
    print LOGFILE "Parsing the SLR predict data contained in: $slr_file\n";
    print LOGFILE "Number of lines in file: ".@SLRlines."\n\n";
    
    my $type_flag = 0;
    my $i;
    
    my ($yyyy,$mm,$dd,$hour,$min,$sec,$MJD);
    my ($X,$Y,$Z,$XD,$YD,$ZD);
    
    my @header = ();
    my @record = ();
    my @output = ();
        
    my ($name,$filetype) = split(/\./,trim($slr_file));
    
#    if ($filetype eq "gfz") {
#	
#	for (my $ii = 0; $ii < $#SLRlines; $ii++) {
#
#	    if (trim($SLRlines[$ii]) eq "") { next; }
#	    if (trim($SLRlines[$ii]) =~ /^[a-zA-Z]+$/) { next; }
#	    	    
#	    my @fields1 = split(/\s+/,trim($SLRlines[$ii]));
#	    
#	    $fields1[0] = $fields1[0] ? $fields1[0] : "empty";
#	    
#	    if ($fields1[0] =~ /^$RE{num}{int}$/x) {
#		
#		my @fields2 = split(/\s+/,trim($SLRlines[$ii+1]));
#		
#		$yyyy = $fields1[0];
#		$mm = $fields1[1];
#		$dd = $fields1[2];
#		$hour = $fields1[3];
#		$min = $fields1[4];
#		$sec = $fields1[5];
#		
#		$MJD = cal2jul($yyyy,$mm,$dd,$hour,$min,$sec) - 2400000.5;
#		
#		$X = $fields1[6]/1000; # Convert from meters to km
#		$Y = $fields1[7]/1000;
#		$Z = $fields1[8]/1000;
#		$XD = $fields2[3]/1000; # Convert from meters/sec to km/sec
#		$YD = $fields2[4]/1000;
#		$ZD = $fields2[5]/1000;
#		
#		@output = ($satnum,$yyyy,$mm,$dd,$hour,$min,$sec,$MJD,$X,$Y,$Z,$XD,$YD,$ZD);
#		$outfile3[$linenum++] = join($separg,@output)."$separg\n";
#		
#		$ii = $ii + 2;
#		
#	    }
#
#	}
#	
#    } else {

	for (my $ii = 0; $ii < $#SLRlines; $ii++) {

	    if (trim($SLRlines[$ii]) eq "") { next; }
	    if (trim($SLRlines[$ii] =~ /^[a-zA-Z\(\)\{\}\[\]-_\.]/)) { next; }

	    my @fields1 = split(/\s+/,trim($SLRlines[$ii++]));

	    if($fields1[0] =~ /^\d+/) {

		my @fields2 = split(/\s+/,trim($SLRlines[$ii++]));

		print $SLRlines[$ii-1]."\n".$SLRlines[$ii]."\n" if $debug;
	    
		$yyyy = $fields1[0];
		$mm = $fields1[1];
		$dd = $fields1[2];
		$hour = $fields1[3];
		$min = $fields1[4];
		$sec = $fields1[5];
		
		$MJD = cal2jul($yyyy,$mm,$dd,$hour,$min,$sec) - 2400000.5;
	    
		$X = $fields1[6]/1000; # Convert from meters to km
		$Y = $fields1[7]/1000;
		$Z = $fields1[8]/1000;
		$XD = $fields2[3]/1000; # Convert from meters/sec to km/sec
		$YD = $fields2[4]/1000;
		$ZD = $fields2[5]/1000;
		
		@output = ($satnum,$yyyy,$mm,$dd,$hour,$min,$sec,$MJD,$X,$Y,$Z,$XD,$YD,$ZD,$idnum3++);
		$outfile3[$count3++] = join($separg,@output)."$separg\n";
	    }
	    
	}
	    
#    } 

    # Close out $slr_predict_file so that we can open the next one for processing
    untie @SLRlines;
    
}

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
