=========================================================================
National Space Science Data Center      Data set  MN-17A         Nov 1989 
=========================================================================

NAME:	     COSPAR International Reference Atmosphere 1986, 0 - 120 km

SCIENTIFIC CONTACT:	Sushil Chandra, Goddard Space Flight Center, 
			code 616, Greenbelt, MD 20771

TECHNICAL CONTACT:	Eric L. Fleming, Applied Research Corporation
			Landover, MD 20785

NSSDC CONTACT:		D. Bilitza, GSFC/NSSDC code 632, Greenbelt,
     			MD 20771, tel. (301) 286-0190
        		bilitza@gsfc.nasa.gov

FILES:          Parameter tables varying with height            bytes
	     (1)	for January               ch11.dat      5034
	     (2)        for February              ch12.dat      5034
                            ....                     ....
	    (12)        for December              ch22.dat	5034
	        Parameter tables varying with pressure
            (13)	for January               cp11.dat     28998
	    (14)        for February              cp12.dat     28998
                            ....                     ....
            (24)        for December              cp22.dat     28998
            (25) FORTRAN driver program, allows   cirat.for     4953 	  
                 fast display of CIRA values
            (26) this file		       aareadme.doc    4596

	    All data files are in VAX/VMS binary format.
 
BRIEF DESCRIPTION:

The COSPAR International Reference Atmosphere (CIRA) provides empirical
models of atmospheric temperature and densities as recommended by the
Committee on Space Research (COSPAR). Since the early sixties different
editions of CIRA have been published. The CIRA Working Group meets bi-
annual during the COSPAR General Assemblies. CIRA-86 is described in 
Advances in Space Research, Volume 8, Numbers 5-6, 1988. In the thermosphere (above 
about 100 km) CIRA-86 is identical with the MSIS model, which is also 
available from NSSDC (MI-91E). 

The lower part (0-120 km) of CIRA-86 consists of tables of the monthly 
mean values of temperature and zonal wind with almost global coverage 
(80N - 80S). Two sets of files were compiled by Fleming et al. (1988), one in 
pressure coordinates including also the geopotential heights, and one in 
height coordinates including also the pressure values. These tables were
generated from several global data compilations including ground-based
and satellite (Nimbus 5,6,7) measurements: Oort (1983), Labitzke et al.
(1985). The lower part was merged with MSIS-86 at 120 km altitude. In
general, hydrostatic and thermal wind balance are maintained at all levels.
The model accurately reproduces most of the characteristic features of
the atmosphere such as the equatorial wind and the general structure 
of the tropopause, stratopause, and mesopause.

The driver program CIRAT developed at NSSDC allows fast and easy display 
of density, temperature and pressure for specified conditions. 

----------------------------- REFERENCES --------------------------------

CIRA 1972, A.C. Strickland (ed.), Akademie Verlag, Berlin, G.D.R., 1972

CIRA 1986, D. Rees (ed.), Advances in Space Research, Volume 8, Number
5-6, 1988

A.H. Oort, Global Atmospheric Circulation Statistics 1958-1983, National
Oceanic and Atmospheric Administration, Professional Paper 14, 180 pp,
U.S. Government Printing Office, Washington, D.C., 1983

K. Labitzke, J.J. Barnett, and B. Edwards (eds.), Middle Atmosphere Program,
MAP Handbook, Volume 16, University of Illinois, Urbana, 1985

K. Rawer, C.M. Minnis, K.S.W. Champion, and M. Roemer (eds.), Models of 
the Atmosphere and Ionosphere, Advances in Space Research, Volume 5,
Number 7, 1985

K.U. Grossmann, K.S.W. Champion, M. Roemer, W.L. Oliver, and T.A. Blix
(eds.), The Earth's Middle and Upper Atmosphere, Advances in Space 
Research, Volume 7, Number 10, 1987

E.L. Fleming, S. Chandra, M.R. Shoeberl, and J.J. Barnett, Monthly mean
global climatology of temperature, wind, geopotential height and pressure
for 0-120 km, National Aeronautics and Space Administration, Technical
Memorandum 100697, Washington, D.C., 1988
=========================================================================
National Space Science Data Center      Data set  MN-17A         Nov 1989 
==========================================================================



