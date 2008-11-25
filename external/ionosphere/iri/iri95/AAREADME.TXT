			IRI-95 Software
			---------------

This directory includes the programs and indices for the 
International Reference Ionosphere 1995 (IRI-95) model. The files can
be retrieved by anonymous ftp. You can use our on-the-fly compression
facility, which compresses the file(s) before transmitting them. To
do this you have to enter the following command at the ftp prompt:
> get filename.gz (e.g. iris13.for.gz)
This will work with the extension '.Z' as well. The tar option can be
used to retrieve a whole subdirectory:
> get subdir.tar      or   > get subdir.tar.gz   for additional compression.
To unzip and untar these files you have to use the following commands:
> gunzip subdir.tar.gz	    and then     > tar -xf subdir.tar

The IRI-95 code is available in Fortran (directory fortran_code) and in C
(directory c_code). Please note that the C code version of the IRI code is 
only updated up to April 1997. The more recent changes and corrections
are not included in this version. You can find a list of the changes
in the COMMENT section of IRIF13.FOR and IRIS13.FOR in the fortran_code
directory.


			Directory Contents:
			-------------------

c_code:
-------

AAREADME.TXT, f2c.h, iri_all.c


fortran_code:
-------------

AAREADME.TXT	Readme file 
IRIS13.FOR	This file contains the main subroutine IRIS13. It computes 
		the IRI output parameters (Ne, Te, Ti, Ni) for specified 
		date and location.

IRIF13.FOR	This file contains all additional subroutines and
		functions that are required for running IRIS13.

IRIT13.FOR 	This new IRI file includes subroutines for computing
		the total electron content. Please NOTE that the
		subroutine IRIT13 uses subroutines IRIS13 and IRI_TEC.

CIRA86.FOR	This file includes the subroutines and functions for
		obtaining the COSPAR International Reference Ionosphere 1986
		(CIRA-86) neutral temperature. 

IG_RZ.DAT	This file contains the solar and ionospheric indices
		(IG12, Rz12) for the time period from Jan 1958 several
		months into the future. This file is updated quaterly
		and includes predicted values for all times 6 months
		prior to the current date. This file is read by subroutine 
		TCON in IRIF13.FOR. The first date on the file is the
		date of the last update.

IRIS13_TEST.FOR Test program for the IRIS13 subroutine.
IRIS13_TEST.OUT Output from the test run.

IRIT13_TEST.FOR Test program for the IRIT13 subroutine.
IRIT13_TEST.OUT Output form the test run.

CCIR%%.ASC	Coefficient files for the global representation of F2
URSI%%.ASC	peak parameters (foF2, M3000); %% = Month+10; ASCII 

Please consult the 'listing of changes' in the comments section at the top 
of these programs for recent corrections and improvements.

More information about the IRI project can be found at
	http://nssdc.gsfc.nasa.gov/space/models/iri.html
(click on Brief Description)
This pages also let you compute model values online.

An IRI Newsletter is published quarterly. To be included on the
distribution or to submit articles of IRI-interest please e-mail
to the Newsletter Editor, K. Oyama, at oyama@bochan.ted.isas.ac.jp

An IRI electronic mailer keeps IRI users informed about model and
software updates. To subscribe please send e-mail to 
Majordomo@nssdc.gsfc.nasa.gov with the following text in the body of
the message: subscribe iri (use 'unsubscribe iri' later if you want
to remove yourself from the distribution).

------------------------ dieter bilitza ------------------ May 1998
