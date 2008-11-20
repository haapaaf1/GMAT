      BLOCK DATA ITERATS
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       25-Sep-1995  1.3
C                                 Removed references to INCLUDE file
C                                 "cinit.inc" since it is no longer used.
C                                 Removed initialization of variables ESCLLAT,
C                                 FSCLLAT, USUE, USUF, and ONLYOP since they
C                                 have been removed from common block INDIRECT.
C     L. Brown       23-Jan-1996  1.3 ==> 1.4
C                                 Variables MSAT, NSAT, SATLAT, and SATLON
C                                 renamed to MPR, NPR, PRLAT, and PRLON in
C                                 common block GRID.
C                                 Variables RADLAT and RADLON renamed to OBSLAT
C                                 and OBSLON in common block GRID.
C     L. Brown       14-Feb-1997  1.4 ==> 1.6
C                                 Added points to altitude grid stored in
C                                 array ZOUT in common block GRID to include
C                                 plasmaspheric altitudes.
C
      INCLUDE 'aindex.inc'
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
C
C     DATA statements for AINDEX.INC
C
      DATA ATR/24.4, 2.12/
      DATA A/20.9, 1.7,13.4, 1.7,
     6       20.9, 1.7,13.4, 1.7/
C
C     DATA statements for GRID.INC
C
      DATA GGLAT0,GGLON0,GGLATF,GGLONF,DLAT,DLON,NUMLAT,NUMLON,NALT/
     1 -89.,5.,89.,355.,2.,10.,90,36,80/
C
      DATA NPR/1/
      DATA PRLAT/MPR*0./
      DATA PRLON/MPR*0./
C
      DATA OBSLAT/0./
      DATA OBSLON/0./
      DATA NAZ/1/
      DATA SAZ/0./
      DATA DAZ/1./
      DATA NEL/1/
      DATA SEL/0./
      DATA DEL/1./
C
      DATA ZOUT/90.,95.,100.,105.,110.,115.,120.,125.,130.,
     1 135., 140.,145.,150.,160.,170.,180.,190.,200.,210.,220.,
     2 230.,240.,250.,260.,270.,280.,290.,300.,320.,340.,360.,
     3 380.,400.,450.,500.,550.,600.,650.,700.,750.,
     4 800.,850.,900.,1000.,1100.,1200.,1300.,1400.,1500.,1600.,1700.,
     & 1800.,1900.,2000.,2100.,2200.,2300.,2400.,2500.,3000.,3500.,
     & 4000.,4500.,5000.,5500.,6000.,6500.,7000.,7500.,8000.,8500.,
     & 9000.,9500.,10000.,12500.,15000.,17500.,20000.,22500.,25000.,
     5 20*0.0/
C
C
C                    DATA statements for INDIRECT.INC
C            1    2   3  4     5     6
      DATA F10P7,EKP,BY,KP(0),KP(1),KP(2)/
C       1     2    3    4    5    6
     1  145., 2.0, 1.0, 1.9, 2.0, 2.1/
C
      END
C
      BLOCK DATA LOWF
C
C  VERSION
C     1.2   27-April-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       27-Apr-1995  1.2
C                                 Changed initialization of NF10P7L from 3
C                                 to 2.
C                                 Removed initialization of NKPL since NKPL has
C                                 been removed from common block LOWER.
C
      INCLUDE 'lower.inc'
      DATA F10P7L,NF10P7L/70.,130.,210.,2/
      END
      BLOCK DATA LOWE
C
C  VERSION
C     1.2   27-April-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       27-Apr-1995  1.2
C                                 Corrected initialization of KPLE(1) from 2.
C                                 to 1.
C                                 Changed initialization of NF10P7LE from 3
C                                 to 2.
C                                 Changed initialization of NKPLE from 3 to 2.
C
      INCLUDE 'low_e.inc'
      DATA F10P7LE,KPLE,NF10P7LE,NKPLE/70.,130.,210.,1.,3.5,6.,2,2/
      END
      BLOCK DATA MIDF
C
C  VERSION
C     1.2   27-April-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       27-Apr-1995  1.2
C                                 Changed initialization of NF10P7M from 3
C                                 to 2.
C                                 Changed initialization of NKPM from 3 to 2.
C
      INCLUDE 'midlat.inc'
      DATA F10P7M,KPM,NF10P7M,NKPM/70.,130.,210.,1.,3.5,6.,2,2/
      END
      BLOCK DATA HIGH
C
C  VERSION
C     1.2   27-April-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       27-Apr-1995  1.2
C                                 Changed initialization of NF10P7H from 3
C                                 to 2.
C                                 Changed initialization of NKPH from 3 to 2.
C
      INCLUDE 'usuarr.inc'
      DATA F10P7H,KPH,NF10P7H,NKPH/70.,130.,210.,1.,3.5,6.,2,2/
      END
C
      BLOCK DATA REGS
      INCLUDE 'precip.inc'
      INCLUDE 'region_b.inc'
C
C     DATA for PRECIP.INC
C
       DATA LAT,MLT,ERG,LAT0,MLT0,RAD0,A0,A1,B1,MLATR,MLTR,VALTR,
     1 MLATR0,MLTR0,RADTR0/414*0.0/
C
C
C
C     DATA statments for REGION_B.INC
C
      DATA CTH,VTH/24.4,20.9,13.4,2.12,1.7,1.7/
      DATA CAB,VAB/-10.5,2.7,0.8,0.0,.267,-.267/
      DATA CB,PB/3.88,0.0,3.0,2.73,0.0,0.0/
      DATA CBB,VBB/11.5,2.633333333,0.0,-0.08333333333/
      DATA QBB,AB(0),BB(0)/0.0,0.00506666666,-10.5,11.5/
C
      END
      PROGRAM PIM
C
C  PURPOSE
C    PIM is the unadjusted parameterized model. Its purpose is to reconstruct
C    the profiles from the databases, merge the profiles according to F10P7,
C    Kp, and location, and return this as output.
C      (1) Get date and geophysical parameters from user
C      (2) Call parameterized models for output
C      (3) obtain EDP's from the parameterized models and return
C          same to calling program.
C
C  METHOD
C    The user will change the parameters as he/she wishes, and the
C    model uses the inputs to determine the ratio of profiles to merge
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY      description
C    A       REAL   (40,40,3,2)    The empirical orthogonal functions
C    CAPA    REAL  (0:8,17,40,3,2) The coefficients for the orthogonal functions
C    DAY     INTEGER               The day of the year.
C    GRDTYP  INTEGER               The switch for the type of output grid:
C                                  [0] Rectangular latitude/longitude grid
C                                  [1] Latitude/longitude pairs
C                                  [2] Azimuth/elevation (ground-based)
C    MLAT    REAL                  Corrected Geomagnetic latitude
C    MLON    REAL                  Corrected Geomagnetic longitude
C    MLT     REAL                  Corrected Geomagnetic local time
C    OUTFILE CHARACTER*32          The name of the output grid file.
C    OUTTYP  INTEGER               The switch for type of output:
C                                  [0] Critical frequencies and heights and
C                                      vertical TEC
C                                  [1] Vertical EDPs
C                                  [2] Vertical EDPs, critical frequencies and
C                                      heights, and vertical TEC
C                                  [3] No output
C    UT      REAL                  Universal time in seconds
C    UTHR    REAL                  Universal time in hours
C    YEAR    INTEGER               The calendar year
C    Z       REAL   (40,3,2)       The values of the altitudes at index
C
C  SUBROUTINES CALLED
C      GETDAT:  obtains all real time data, sorts it according to
C               its nature (direct or indirect) and writes two files
C               (DIRECT.DATA and INDIRECT.DATA);  returns number of
C               records in each file.
C      OUTPUT:  Puts the station and grid outputs to disk
C      READ_DBASES   Reads the parameterized model and URSI coefficients
C                    databases
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     Robert E. Daniell
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        30-Jan-1995   1.0.9 ==> 1.1
C                                   Local variable IGRIDSW has been renamed to
C                                   OUTTYP.
C                                   Local variable GRDTYP has been added.
C                                   The argument lists of routines GETDAT and
C                                   OUTPUT have changed.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   The description of local variable GRDTYP
C                                   has been modified to include the radar
C                                   output grid type.
C                                   The version number and date are displayed
C                                   to the default output device.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Updated the version number and version
C                                   date.
C                                   Removed argument BATCH from the call to
C                                   routine GETDAT.
C                                   Removed local variable BATCH since it is no
C                                   longer used.
C                                   Removed INCLUDE statement for "cinit.inc"
C                                   since it is no longer used.
C                                   Replaced the call to routine USER_INPUT
C                                   with a call to routine READ_DBASES.
C                                   Added INCLUDE statement for "dpath.inc".
C                                   Removed FORMAT statements 1000 and 1100
C                                   since they are not used.
C     L. Brown        23-Jan-1996   1.3 ==> 1.4
C                                   Updated the version number and version
C                                   date.
C                                   Changed name of output grid type
C                                   "satellite track" to
C                                   "latitude/longitude pairs".
C                                   Changed name of output grid type
C                                   "radar azimuth/elevation" to
C                                   "azimuth/elevation (ground-based)".
C     L. Brown        30-Sep-1996   1.4 ==> 1.5
C                                   Updated the version number and version
C                                   date.
C     L. Brown        14-Feb-1997   1.5 ==> 1.6
C                                   Updated the version number and version
C                                   date.
C     L. Brown        13-Jan-1998   1.6 ==> 1.7
C                                   Updated the version number and version
C                                   date.
C
C  REFERENCES
C
C  SPECIAL CONSTANTS
C
C  Parameters and common blocks:
C
      INCLUDE 'dpath.inc'
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'logicuni.inc'
C  Declarations:
      INTEGER YEAR,DAY,OUTTYP,GRDTYP,MONTH
      REAL UT,UTHR
      CHARACTER*32 OUTFILE
C
C
C  Display the version number and date
C
      PRINT *,' *****************************'
      PRINT *,' = PIM 1.7   13-January-1998 ='
      PRINT *,' *****************************'
C
C     Get the date, solar and geophysical parameters, and the altitude
C     and latitude longitude grid for the output.
C
      CALL GETDAT(YEAR,DAY,MONTH,UTHR,OUTFILE,OUTTYP,GRDTYP)
      UT = 3600.*UTHR
C
C     Initialize variables
C
      CALL INITPR
C
C  Read the parameterized model and URSI coefficients databases
C
      CALL READ_DBASES(DAY,MONTH,UTHR,PUSU,PMID,PLOW,PLME,PAWS,JOPEN)
C
C     Do the output grid.
C
      CALL OUTPUT(YEAR,DAY,UT,OUTTYP,GRDTYP,OUTFILE,LUTEXT,'N')
C
      STOP 'PIM successfully completed.'
      END
C
