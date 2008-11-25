C IRID12.FOR, Version 12.2.6 ------------------------- August 1994
C
C*****************************************************************
C*********************** UPDATE HISTORY **************************
C*****************************************************************
C***   IRIMP9 (1986) DIFFERS FROM IRIMP8 BY CONTAINING:        ***
C***    - CORRECTED B0 FOR SOUTHERN HEMISPHERE                 ***
C***    - RESTRICTION OF RZ12/COVM TO VALUES BELOW 145/185     ***
C***    - CORRECTED HARMONIZED BENT                            ***
C***    - NEW ELECTRON TEMPERATURE MODEL                       ***
C***   IRID10 (1988) DIFFERS FROM IRIMP9 BY CONTAINING:        ***
C***    - BETTER REPRESENTATION OF SOLAR ZENITH ANGLE          ***
C***    - USING ASYMPTOTE FOR LARGE ARGUMENTS OF EPSTEIN FUNC. ***
C***    - NEWER FOE NIGHT-TIME                                 ***
C***   IRID11 (1990) DIFFERS FROM IRID10 BY CONTAINING:        ***
C***    - CORRECT HEMISPHERE-SEASON IN TE MODEL                ***
C***    - OPTION TO USE URSI-FOF2                              ***
C***    - OPTION TO USE GULYAEVA'S BOTTOMSIDE THICKNESS PARA.  ***
C***    - DANILOV-YAICHNIKOV AS ALTERNATE ION COMPOS. MODEL    ***
C***   IRID12 (1990) DIFFERS FROM IRID11 BY CONTAINING:        ***
C***    - TE MAXIMUM AT NIGHT; HMAXN INSTEAD OF HMN            ***
C***    - 9/11/91  VNER FOR NIGHTTIME LAY-VERSION: ABS(..)     ***
C***    - 9/11/91  XEN(..) IN CASE OF LAY-VERSION              ***
C***    - 10/9/91  SUB ALL NAMES WITH 6 OR MORE CHARACTERS     ***
C***    - 10/28/91 CIRA-86 instead of CIRA-72                  ***
C***    - 10/29/91 TE > TI > TN enforced for final profile     ***
C***    - 10/31/91 CORRECTED HF1 IN HST SEARCH: N(HF1)>NME     ***
C***    - 11/14/91 C1=0 IF NO F1-REGION                        ***
C***    - 11/14/91 CORRECTED HZ FOR LIN. APP.                  ***
C***    - 12/10/91 CORRECTION FOR SVAR=0                       ***
C*** ------------------ included on diskette ----------------- ***
C***    -  1/29/92 NEQV INSTEAD OF NE BETWEEN URSIF2 AND URSIFO***
C***    -  1/29/92 REPLACE MONTH0 AND RG0 WITH MONTHO AND RGO  ***
C***    -  4/21/92 FREQUENCY OUTPUT AND H0.5                   ***
C***	-  6/ 8/93 DECIMAL MONTH FOR IONCOM                    ***
C***    -  7/ 6/94 CALCULATION OF DAYNR (MODA) FOR IVAR=4 NOT 3***
C***    -  8/30/94 REPLACE B0POL WITH B0_TAB; BETTER ANNUALLY  ***
C*****************************************************************
C
C*****************************************************************
C********* INTERNATIONAL REFERENCE IONOSPHERE (IRI). *************
C*****************************************************************
C****************   DRIVER PROGRAM    ****************************
C*****************************************************************
C*** THIS PROGRAM PRODUCES TABLES OF                           ***
C***    - ELECTRON DENSITY (OR PLASMA FREQUENCY                ***
C***    - NORMALISED ELECTRON DENSITY (TO F2-PEAK DENSITY)     ***
C***    - NEUTRAL TEMPERATURE (CIRA 86)                        ***
C***    - ELECTRON TEMPERATURE                                 ***
C***    - ION TEMPERATURE                                      ***
C***    - ELECTRON TO ION TEMPERATURE RATIO                    ***
C***    - RELATIVE PERCENTAGE DENSITIES OF ATOMIC OXYGEN IONS  ***
C***    - RELATIVE PERCENTAGE DENSITIES OF HYDROGEN IONS       ***
C***    - RELATIVE PERCENTAGE DENSITIES OF HELIUM IONS         ***
C***    - RELATIVE PERCENTAGE DENSITIES OF MOLECULAR OXYGEN ION***
C***    - RELATIVE PERCENTAGE DENSITIES OF NITROGEN OXYD IONS  ***
C*****************************************************************
C*** VARYING WITH                                              ***
C***    - GEOG. (OR GEOM.) LATITUDE, OR LONGITUDE, OR          ***
C***    - ALTITUDE, OR                                         ***
C***    - ZURICH SUNSPOT NUMBER (12-MONTH-RUNNING MEAN), OR    ***
C***    - MONTH, OR DAY OF YEAR, OR                            ***
C***    - LOCAL OR UNIVERSAL TIME                              ***
C*****************************************************************
C*** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        ***
C***     ELECTRON DENSITY         60/80 KM       1000 KM       ***
C***     TEMPERATURES              120 KM        3000 KM       ***
C***     ION DENSITIES             100 KM        1000 KM       ***
C*****************************************************************
C*****************************************************************
C*********          PROGRAM INTERNALLY              **************
C*********       ALL ANGLES ARE IN DEGREE           **************
C*********       ALL DENSITIES ARE IN M-3           **************
C*********       ALL ALTITUDES ARE IN KM            **************
C*********     ALL TEMPERATURES ARE IN KELVIN       **************
C*********     ALL TIMES ARE IN DECIMAL HOURS       **************
C*****************************************************************
C*****************************************************************
C* THE F2 PEAK VALUES CAN BE GIVEN AS INPUT OR CALCULATED WITH   *
C* THE CCIR OR URSI MODELS. THE CCIR COEFFICIENT SET FOR THE     *
C* MONTH "mm" IS EXPECTED IN THE BINARY FILE "CCIRmm.BIN" AND    *
C* THE URSI SET IN "URSImm.BIN". IF YOU USE THE ASCII CODED      *
C* FILES "CCIRmm.ASC", YOU HAVE TO INCORPORATE THE CHANGES       *
C* INDICTED IN PROGRAM SECTION ENTITLED "READ CCIR COEFFICIENT   *
C* SET FOR CHOSEN MONTH."                                        * 
C*****************************************************************
C*****************************************************************
      INTEGER 		EGNR,AGNR,DAYNR,DDO,DO2,SEASON,SEADAY
      INTEGER		PIKTAB
      REAL 		LATI,LONGI,MO2,MO,MODIP,NMF2,MAGBR
      REAL  		NMF1,NME,NMD,NEI,MM,MLAT,MLONG,NOBO2
      CHARACTER*2	MTENE(3)
      CHARACTER*3	MEF,MION
      CHARACTER*4	IMZ(11),MTNCOR
      CHARACTER*5       LTEX,ITEXT(11),MB0,MIRIM,MHMF2,MFOF2
      CHARACTER*12	FILENA
      DIMENSION  F(3),RIF(4),E(4),XDELS(4),DNDS(4)
      DIMENSION  FF0(988),XM0(441),F2(13,76,2),FM3(9,49,2),
     &		 AFOF2(10),AHMF2(10)
      DIMENSION  AMP(4),HXL(4),SCL(4),B0B1(5),OUTF(11)
      DIMENSION  CTN(3),CTNN(3),XSM(4),MM(5),DTI(4)
      DIMENSION  AHH(7),STTE(6),DTE(5),ATE(7),TEA(6),HOA(3),XNAR(3)
      DIMENSION  PG1O(80),PG2O(32),PG3O(80),PF1O(12),PF2O(4),PF3O(12)
      DIMENSION  HO(4),MO(5),DDO(4),HO2(2),MO2(3),DO2(2),DION(7)
      DIMENSION  XVAR(11),PARMIN(7),PARMAX(7),IPARN(7),IPART(7),
     &           IPARI(7)
      LOGICAL		EXT,SCHALT,NIGHT,UNTI,GEMA,TNOPIV,TCON(3)
      LOGICAL		F1REG,FOF2IN,HMF2IN,URSIF2,LAYVER,DY,GULB0
      LOGICAL		NOTBEG,STORE,NODEN,NOTEM,NOION,TENEOP
      LOGICAL           OLD79,TOPSI,BOTTO,BELOWE,HEIPRO,URSIFO
      COMMON	/BLOCK1/HMF2,NMF2,HMF1	         /CONST/UMR
     &		/BLOCK2/B0,B1,C1      /BLOCK3/HZ,T,HST,STR
     &  	/BLOCK4/HME,NME,HEF   /BLOCK5/NIGHT,E
     &		/BLOCK6/HMD,NMD,HDX   /BLOCK7/D1,XKK,FP30,FP3U,FP1,FP2
     &  	/BLOCK8/HS,TNHS,XSM,MM,DTI,MXSM    	
     &  	/BLOTN/ZX,TNX,ATN,CTN   /BLOTE/AHH,ATE1,STTE,DTE
     &		/BLO10/BETA,ETA,DELTA,ZETA	 /ARGEXP/ARGMAX
      EXTERNAL 		XE1,XE2,XE3,XE4,XE5,XE6,TEDER
      DATA  HOA  /300.,400.,600./,   XNAR       /3*0.0/,   
     &      XDELS   /3*5.,10./,      DNDS   /.016,.01,2*.016/,
     &      DDO	  /9,5,5,25/,        DO2        /5,5/,
     &      B0B1  /.755566,.778596,.797332,.812928,.826146/,
     &	    AFOF2 /10*3.E10/,	     AHMF2  /10*300./
      DATA  IMZ  /'GEOD','GEOD',2*'    ','L.T.','    ','YEAR',
     &        'LAT.',3*'    '/,       ITEXT/
     &      ' LATI',' LONG',' RZ12','MONTH',' HOUR',' H/KM','DAYOF',
     &        ' DIP ','MODIP','F10.7',' SZA '/
      DATA  JMAG,LATI,LONGI,MLAT,MLONG	/0,45.1,293.1,45.1,293.1/,
     &      XHIGHT,R,MONTH,MMDD,DHOUR	/100.,100.,10,1015,12.5/,
     &      IVAR,BVAR,EVAR,SVAR		/6,100.,1000.,100./,
     &      IRIPS,ISELEC,IJFOF2,IJHMF2    /1,2,2,1/,
     &      JAGNR,JAGNRM,MONTHO,RGO  	/0,1,0,0.0/
      DATA  PARMIN  /-90.,-360.,0.00,1.0,0.0,60.00,1./,
     &	    PARMAX  /+90.,+360.,250.,12.,49.,3000.,366./,
     &	    IPART   /1,0,1,0,1,0,1/,	IPARI	/1,0,0,1,0,1,1/,
     &	    IPARN   /1,1,0,0,1,1,0/
      DATA  MEF,MION/2*'---'/,   MHMF2,MFOF2,MB0,MIRIM/4*'-----'/,
     &      MTNCOR  /'----'/,    MTENE/3*'--'/
C
C PROGAM CONSTANTS
C
	ARGMAX=88.0
     	UMR=ATAN(1.0)*4./180.
      	ALOG2=ALOG(2.)
	ALG100=ALOG(100.)
      	MIN0=0
     	MAX1=1
	XNMAX=1.E15
	XMIN0=0.
	NOTBEG=.FALSE.
	UNTI=.FALSE.
	ISTART=1
C
C Code inserted to aleviate block data problem for PC version.
C Thus avoiding DATA statement with parameters from COMMON block.
C
        AHH(1)=120.
        AHH(2)=0.
        AHH(3)=300.
        AHH(4)=400.
        AHH(5)=600.
        AHH(6)=1400.
        AHH(7)=3000.
        DTE(1)=5.
        DTE(2)=5.
        DTE(3)=10.
        DTE(4)=20.
        DTE(5)=20.
        DTI(1)=10.
        DTI(2)=10.
        DTI(3)=20.
        DTI(4)=20.
C
C FIRST SPECIFY YOUR COMPUTERS CHANNEL NUMBERS ....................
C EGNR=INPUT, MONITO=MONITOR, KONSOL=MESSAGES......................
C AGNR=OUTPUT (OUTPUT IS DISPLAYED OR STORED IN FILE OUTPUT.IRI)...
C IUCCIR=UNIT NUMBER FOR CCIR COEFFICIENTS ........................
C IUOUT=UNIT NUMBER FOR OUTPUT FILE OUTPUT.IRI ....................
C IUMESS=UNIT NUMBER FOR MESSAGE OUTPUT FILE KONSOL.IRI ...........
C
      EGNR=5
      MONITO=6
      IUCCIR=10
      IUOUT=16
      IUMESS=15


c
c interactive parameter input ........................................
c
      WRITE(MONITO,5100)
5100  FORMAT(1X/////8X,'**** INTERNATIONAL REFERENCE ',
     &	'IONOSPHERE 1990 ****'/1X,63('*')/' THIS PROGRAM ALLOWS YOU',
     &  ' TO PRODUCE TABLES OF IRI PARAMETERS VARYING'/' WITH LATITUDE',
     &	', LONGITUDE,SOLAR ACTIVITY,MONTH,TIME OR ALTITUDE.'/1X,63('*')/
     &	'  In each of the following windows you will be asked to enter'/
     &	'  one or more values, defining the conditions for your IRI '/
     &	'  tables.'/'  In each window the current value(s) is (are) ',
     &  'shown in the right'/'  upper corner (# .. #). You can choose',
     &  ' the current value(s) by'/'  entering / at the prompt'/'  If ',
     &  'you enter a wrong character or a value outside the allowed'/
     &  '  parameter range, the program will ask you for a new entry.'/
     &	'  After your tables are displayed, you can change any ',
     &  'parameter'/'  you wish to change and create a new profile.'/
     &  '  You can leave the program at any point by entering Ctrl Z.'/
     &  1X,26('*'),' GOOD LUCK ',26('*'))
      GOTO 5508
c
c change parameters .................................................
c
3293  IF(GEMA) THEN
	DLATI=MLAT
	DLONG=MLONG
      ELSE
	DLATI=LATI
	DLONG=LONGI
      ENDIF
      IF(UNTI) THEN
	LTEX='UT+25'
      ELSE
      	LTEX='LT   '
      ENDIF
      WRITE(MONITO,5602) DLATI,JMAG,DLONG,ITEXT(IVAR),XHIGHT,BVAR,
     &  EVAR,SVAR,R,MONTH,IDAY,LTEX,DHOUR
      MAXI=13
      ISTART=ISTART+1
5602  FORMAT(1X//' **** WHICH PARAMETER DO YOU WANT TO ',
     & 'CHANGE?'/1X,60('-')/' 0  NO FURTHER CHANGES, CALCULATE',
     & ' PROFILE'/' 1  LATITUDE  #',F5.1,'#',8X,'7  GEOD.(0) OR',
     & ' GEOM.(1)    #',I1,'#'/' 2  LONGITUDE #',F5.1,'#',8X,
     & '8  SELECTION OF VARIABLE   #',A5,'#'/' 3  ALTITUDE  #',
     & F6.1,'#',7X,'9  VARIABLE RANGE  #',3F8.1,'#'/' 4  RZ12',
     & 6X,'#',F5.1,'#',7X,'10  PARAMETER CHOICES AND OPTIONS'/
     & ' 5  MONTH,DAY #',I2,I3,'#',7X,'11  F PEAK VALUES'/' 6  ',A5,
     & 5X,'#',F5.1,'#',7X,'12  TE(NE) MODEL'/28X,'13  DISPLAY',
     & ' OF PROFILE AND MESSAGES'/1X,70('-')/' ENTER NUMBER')   
8102  READ(EGNR,*,ERR=8100,END=3330) ISWIT
      IF((ISWIT.LE.MAXI).AND.(ISWIT.GE.MIN0)) GOTO 8104
8100  	WRITE(MONITO,8101) MIN0,MAXI
      	GOTO 8102
8104  GOTO (5300,3329,3339,3331,3500,5501,5502,3332,5503,5504,
     &    5505,5506,5507,5508) ISWIT+1
C
c store or display tables ...........................................
C
5508  WRITE(MONITO,5123) JAGNR
5123  FORMAT(/' DO YOU WANT YOUR IRI-PROFILES',27X,'#',I1,'#'/5X,
     & 'DISPLAYED ON YOUR MONITOR:  ENTER  0  AT PROMPT'/5X,
     & 'STORED IN FILE OUTPUT.IRI:  ENTER  1  AT PROMPT'/5X,
     & 'DISPLAYED AND STORED     :  ENTER  2  AT PROMPT')
      MAXI=2
      WRITE(MONITO,8640)
8105  READ(EGNR,*,ERR=8106,END=3330) JAGNR
      IF((JAGNR.LE.MAXI).AND.(JAGNR.GE.MIN0)) GOTO 8107
8106  	WRITE(MONITO,8101) MIN0,MAXI
      	GOTO 8105
8107  IVARNR=0
      AGNR=MONITO
      IF(JAGNR.GT.0) THEN
        OPEN(UNIT=IUOUT,FILE='OUTPUT.IRI',STATUS='NEW',FORM='FORMATTED')
        IF(JAGNR.EQ.1) AGNR=IUOUT
	STORE=.FALSE.
        IF(JAGNR.EQ.2) STORE=.TRUE.
      ENDIF
C
c store or display messages ...........................................
C
      WRITE(MONITO,5124) JAGNRM
5124  FORMAT(/' DO YOU WANT MESSAGES',37X,'#',I1,'#'/5X,
     & 'DISPLAYED ON YOUR MONITOR:  ENTER  0  AT PROMPT'/5X,
     & 'STORED IN FILE KONSOL.IRI:  ENTER  1  AT PROMPT'/5X,
     & 'STORED IN FILE OUTPUT.IRI:  ENTER  2  AT PROMPT')
      WRITE(MONITO,8640)
      MAXI=1
      IF(JAGNR.GT.0) MAXI=2
8111  READ(EGNR,*,ERR=8110,END=3330) JAGNRM
      IF((JAGNRM.GE.MIN0).AND.(JAGNRM.LE.MAXI)) GOTO 8109
8110  	WRITE(MONITO,8101) MIN0,MAXI
      	GOTO 8111
8109  KONSOL=MONITO
      IF(JAGNRM.EQ.1) THEN
        OPEN(UNIT=IUMESS,FILE='KONSOL.IRI',STATUS='NEW',
     &		FORM='FORMATTED')
        KONSOL=IUMESS
      	ENDIF
      IF(JAGNRM.EQ.2) KONSOL=IUOUT
      IF(NOTBEG) GOTO 3293
c
c select variable .................................................
c
5503  WRITE(MONITO,5040) IVAR
5040  FORMAT(1X//' SELECT YOUR VARIABLE:',35X,'#',I1,'#'//34X, 
     &  'Geodetic or geomagnetic coordinates'/34X,'are assumed ',
     &  'depending on your later'/' 1  LATITUDE    4  MONTH',10X,
     &  'selection of JMAG.'/' 2  LONGITUDE   5  HOUR',11X,
     &  'Variation in UNIVERSAL TIME is selected'/' 3  RZ12',8X,
     &  '6  ALTITUDE',7X,'by adding "25" to your BEGIN hour'/
     &  ' 7  DAYNR',25X,'in the next window.')
      WRITE(MONITO,8640)
      MAXI=7
      MINI=1
8113  READ(EGNR,*,ERR=8112,END=3330) IVAR
      IF((IVAR.LE.MAXI).AND.(IVAR.GE.MINI)) GOTO 5504
8112  	WRITE(MONITO,8101) MINI,MAXI
      	GOTO 8113
5504  if(ivar.eq.4) IDAY=15
      HEIPRO=(IVAR.EQ.6)
c
c select variable range ..............................................
c
      WRITE(MONITO,5044) BVAR,EVAR,SVAR
5044  FORMAT(1X//' CHOOSE YOUR VARIABLE RANGE:',3X,' BEGIN,  END ,',
     & ' STEPWIDTH ?'/30X,'#',F6.1,',',F6.1,',',F6.1,'#')
      IF(IVAR.EQ.5) WRITE(MONITO,6044)
6044  FORMAT(1X/' !! if you add 25 to your BEGIN hour, UNIVERSAL ',
     & 'TIME is assumed,'/' !! otherwise LOCAL TIME')
      WRITE(MONITO,8640)
      XMAX=PARMAX(IVAR)
      XMIN=PARMIN(IVAR)
8114  READ(EGNR,*,ERR=8115,END=3330) BVAR,EVAR,SVAR
      IF((EVAR.LE.XMAX).AND.(BVAR.GE.XMIN)) GOTO 8116
8115  	WRITE(MONITO,8117) XMIN,XMAX
     	GOTO 8114
8116  IF(IVAR.NE.5) GOTO 2929
      IF(BVAR.GT.24.1) THEN
	UNTI=.TRUE.
	IMZ(5)='U.T.'
	BVAR=BVAR-25.
      ELSE
	IMZ(5)='L.T.'
	UNTI=.FALSE.
      ENDIF
2929  LANZ=INT((EVAR-BVAR)/SVAR)+1
c
c output variable in table ...........................
c
      ITABLA=0
	IF(IVAR.GT.5) GOTO 4747
	IF((IVAR.EQ.2).OR.(IVAR.EQ.4)) GOTO 4747
      IF(IVAR.EQ.1) WRITE(MONITO,4646) ITABLA
      IF(IVAR.EQ.3) WRITE(MONITO,4546) ITABLA
      IF(IVAR.EQ.5) WRITE(MONITO,4446) ITABLA
4646  FORMAT(1X//' CHOOSE THE VARIABLE TO BE SHOWN IN TABLE:',16X,
     &  '#',I1,'#'/1X,60('-')/' enter    0     for GEODETIC or ',
     &  'GEOMAGNETIC LATITUDE'/10X,'1     for MAGNETIC DIP LATITUDE'/
     &  10X,'2     for MODIFIED DIP LATITUDE (MODIP)'/' !! This ',
     &  'allows you to display other latitude parameters'/' !! in ',
     &  'the final table.'/,' !! It does not change the selected ',
     &  'parameter or parameter range.') 
4546  FORMAT(1X//' CHOOSE THE VARIABLE TO BE SHOWN IN TABLE:',16X,
     &  '#',I1,'#'/1X,60('-')/' enter    0     for SOLAR SUNSPOT',
     &  ' NUMBER'/10X,'1     for 10.7 cm SOLAR RADIO FLUX'/' !! This',
     &  ' allows you to display F10.7 rather than R in the ',
     &  'final'/' !! table. It does not change the selected ',
     &  'R range.') 
4446  FORMAT(1X//' CHOOSE THE VARIABLE TO BE SHOWN IN TABLE:',16X,
     &  '#',I1,'#'/1X,60('-')/' enter    0     for LOCAL or UNIVER',
     &  'SAL TIME'/10X,'1     for SOLAR ZENITH ANGLE'/' !! This ',
     &  'allows you to display the solar zenith angle rather '/
     &  ' !! than local or universal time in the final table.'/
     &  ' !! It does not change the selected LT/UT range.')
      WRITE(MONITO,8640)
      MAXI=1
	IF(IVAR.LT.2) MAXI=2
      MINI=0
4645  READ(EGNR,*,ERR=4644,END=3330) ITABLA
      IF((ITABLA.LE.MAXI).AND.(ITABLA.GE.MINI)) GOTO 4747
4644  	WRITE(MONITO,8101) MINI,MAXI
      	GOTO 4645
4747  IF(NOTBEG) GOTO 3293
c
c geodetic or geomagnetic ..........................................
c
3332  WRITE(MONITO,6006) JMAG
6006  FORMAT(1X//1X,'JMAG ?',51X,'#',I1,'#'//' !! "0" geodetic',
     &  ' longitude and latitude'/' !! "1"    geomagnetic   ... ')
      WRITE(MONITO,8640)
8118  READ(EGNR,*,ERR=8119,END=3330) JMAG
      IF((JMAG.EQ.MAX1).OR.(JMAG.EQ.MIN0)) GOTO 8120
8119  	WRITE(MONITO,8101) MIN0,MAX1
      	GOTO 8118
8120  IF(JMAG.EQ.1) THEN
	GEMA=.TRUE.
	IMZ(1)='GEOM'
      ELSE
	GEMA=.FALSE.
	IMZ(1)='GEOD'
      ENDIF
      IMZ(2)=IMZ(1)
      IF(NOTBEG) GOTO 3293
      IVARNR=IVARNR+1
      IF(IVARNR.EQ.IVAR) GOTO 7339
c
c latitude ...........................................................
c
3329  IF(GEMA) THEN
	DLATI=MLAT
      ELSE
	DLATI=LATI
      ENDIF
      WRITE(MONITO,5000) IMZ(1),DLATI
5000  FORMAT(1X//1X,A4,'. LATITUDE ?   !NORTH!   [DEGREE,DECIMAL]',
     & 8X,'#',F5.1,'#')
      WRITE(MONITO,8640)
      XMAX=PARMAX(1)
      XMIN=PARMIN(1)
8121  READ(EGNR,*,ERR=8122,END=3330) DLATI
      IF((DLATI.LE.XMAX).AND.(DLATI.GE.XMIN)) GOTO 8123
8122  	WRITE(MONITO,8117) XMIN,XMAX
      	GOTO 8121
8123  IF(GEMA) THEN
	MLAT=DLATI
      ELSE
	LATI=DLATI
      ENDIF
      IF(NOTBEG) GOTO 3293
7339  IVARNR=IVARNR+1
      IF(IVARNR.EQ.IVAR) GOTO 7500
c
c longitude .....................................................
c
3339  IF(GEMA) THEN
	DLONG=MLONG
      ELSE
	DLONG=LONGI
      ENDIF
      WRITE(MONITO,6001) IMZ(2),DLONG
6001  FORMAT(1X//1X,A4,'. LONGITUDE ?   !EAST!   ',
     &   '[DEGREE,DECIMAL]',8X,'#',F5.1,'#')
      WRITE(MONITO,8640)
      XMAX=PARMAX(2)
      XMIN=PARMIN(2)
8124  READ(EGNR,*,ERR=8125,END=3330) DLONG
      IF((DLONG.LE.XMAX).AND.(DLONG.GE.XMIN)) GOTO 8126
8125  	WRITE(MONITO,8117) XMIN,XMAX
      	GOTO 8124
8126  IF(GEMA) THEN
	MLONG=DLONG
      ELSE
	LONGI=DLONG
      ENDIF
      IF(NOTBEG) GOTO 3293
7500  IVARNR=IVARNR+1
      IF(IVARNR.EQ.IVAR) GOTO 7701
c
c solar sunspot number .............................................
c
3500  WRITE(MONITO,6003) R
6003  FORMAT(1X//1X,'SOLAR SUNSPOT NUMBER ?   !12-MONTH-RUNNING '
     &  ,'MEAN!    #',F5.1,'#')
      WRITE(MONITO,8640)
      XMAX=PARMAX(3)
      XMIN=PARMIN(3)
8127  READ(EGNR,*,ERR=8128,END=3330) R
      IF((R.LE.XMAX).AND.(R.GE.XMIN)) GOTO 8129
8128  	WRITE(MONITO,8117) XMIN,XMAX
      	GOTO 8127
8129  IF(NOTBEG) GOTO 3293
7701  IVARNR=IVARNR+1
      IF((IVARNR.EQ.IVAR).OR.(IVAR.EQ.7)) GOTO 7502
c
c month .................................................................
c
5501  WRITE(MONITO,6004) MMDD
6004  FORMAT(1X//1X,'DATE ?   * MONTH&DAY (MMDD) or DAY OF YEAR ',
     &	'(-DDD) *   #',I4,'#')
      WRITE(MONITO,8640)
8130  READ(EGNR,*,ERR=8131,END=3330) MMDD
	if(MMDD.lt.0) then
		maxi=-1
		mini=-366
	else
		MAXI=1231
		MINI=101
		endif
      IF((MMDD.LE.MAXI).AND.(MMDD.GE.MINI)) GOTO 8132
8131  	WRITE(MONITO,8175) MINI,MAXI
      	GOTO 8130
8132  	if(MMDD.lt.0) then
		DAYNR=-MMDD
		call MODA(1,MONTH,IDAY,DAYNR)
	else
		MONTH=MMDD/100
		IDAY=MMDD-MONTH*100
		call MODA(0,MONTH,IDAY,DAYNR)
	endif
	zmonth = month + iday / 30.0
      IF(NOTBEG) GOTO 3293
7502  IVARNR=IVARNR+1
      IF(IVARNR.EQ.IVAR) GOTO 7331
c
c time of day .....................................................
c
5502  WRITE(MONITO,6005) DHOUR
6005  FORMAT(1X//1X,'HOUR ?',14X,'[HOURS, DECIMAL]',18X,'#',F4.1,
     &  '#'//' !! UNIVERSAL TIME (UT) can be chosen by entering ',
     &  'HOUR=UT+25.'/' !! If HOUR is less 25, than LOCAL TIME is',
     &  ' assumed')
      WRITE(MONITO,8640)
      XMAX=PARMAX(5)
      XMIN=PARMIN(5)
8133  READ(EGNR,*,ERR=8134,END=3330) DHOUR
      IF((DHOUR.LE.XMAX).AND.(DHOUR.GE.XMIN)) GOTO 8135
8134  	WRITE(MONITO,8117) XMIN,XMAX
      	GOTO 8133
8135  IF(DHOUR.GT.24.1) THEN
	UNTI=.TRUE.
	UT=DHOUR-25.
      ELSE
	UNTI=.FALSE.
	HOUR=DHOUR
      ENDIF
      IF(NOTBEG) GOTO 3293
7331  IVARNR=IVARNR+1
      IF(IVARNR.EQ.IVAR) GOTO 5505
c
c altitude .........................................................
c
3331  WRITE(MONITO,6002) XHIGHT
6002  FORMAT(1X//1X,'ALTITUDE ?    [KM]',34X,'#',F6.1,'#'//
     & ' !! ALTITUDE=0   tables of F2,F1,E, and D peak altitudes'/
     & ' !!',14X,'and densities are obtained'/
     & ' !! ALTITUDE=-1  tables of F2,F1,E, and D peak altitudes'/
     & ' !!',14X,'and plasma frequencies are obtained')
      WRITE(MONITO,8640)
      XMAX=PARMAX(6)
      XMIN=-1.0
8136  READ(EGNR,*,ERR=8137,END=3330) XHIGHT
      IF((XHIGHT.LE.XMAX).AND.(XHIGHT.GE.XMIN)) GOTO 8138
8137  	WRITE(MONITO,8117) XMIN0,XMAX
      	GOTO 8136
8138  IF(NOTBEG) GOTO 3293
c
c selection of output parameters ..............................
c
5505  	WRITE(MONITO,5534) IRIPS
5534  FORMAT(1X/' WHICH PARAMETERS SHOULD BE CALCULATED ?',18X,
     & '#',I1,'#'/1X,60('-')/5X,'1 - ALL PARAMETERS'/5X,
     & '2 - ONLY ELECTRON DENSITY'/5X,'3 - ONLY TEMPERATURES'/5X,
     & '4 - ONLY ION COMPOSITION'/5X,'5 - DENSITY AND TEMPERATURES'/
     & 5X,'6 - DENSITY AND ION COMPOSITION'/5X,
     & '7 - TEMPERATURES AND ION COMPOSITION')
      WRITE(MONITO,8640)
	MAXI=7
	MINI=1
8142  	READ(EGNR,*,ERR=8143,END=3330) IRIPS
      	IF((IRIPS.LE.MAXI).AND.(IRIPS.GE.MINI)) GOTO 8144
8143  		WRITE(MONITO,8101) MINI,MAXI
      		GOTO 8142
8144  NODEN=(IPARN(IRIPS).EQ.0)
      NOTEM=(IPART(IRIPS).EQ.0)
      NOION=(IPARI(IRIPS).EQ.0)
c
c selection of density and ion composition options ..................
c
	IF((NODEN).AND.(NOION)) goto 2810
      WRITE(MONITO,5030) ISELEC
5030  FORMAT(1X//' SELECTION OF DENSITY OPTIONS:',26X,'#',I3,'#'/1X,
     & 60('-')/5X,'1 - OLD-B0:   B0-table'/5X,'2 - NEW-B0:   Gulyae',
     & 'va-h0.5 (!recommended choice!) '/5X,'3 - OLD-TS:   Topside as',
     & ' in IRI 1979'/1X,60('-')/5X,'4 - LAY: ',
     & 5X,'analytical LAY-representation for the E-F'/19X,'region',
     & ' using HMF1(SZA) formula and Gul.-B0.'/1X,
     & 60('-')/' ION COMPOSITION: - Enter the chosen option as a ',
     & 'negative'/' number, if you would like to use the Danilov-',
     & 'Yaichnikov'/' ion composition model.')
      WRITE(MONITO,8640)
	mini=-4
	maxi=4
8139  READ(EGNR,*,ERR=8140,END=3330) ISELEC
      IF((ISELEC.LE.MAXI).AND.(ISELEC.GE.MINI)) GOTO 8141
8140  	WRITE(MONITO,8175) MINI,MAXI
      	GOTO 8139
8141  IF(ISELEC.LT.0) THEN
	DY=.TRUE.
	MION='DY-'
	IISELEC=-ISELEC
      ELSE
	DY=.FALSE.
	MION='----'
	IISELEC=ISELEC
      ENDIF
      IF(IISELEC.GT.3) THEN
	LAYVER=.TRUE.
	MEF='LAY'
      ELSE
	LAYVER=.FALSE.
	MEF='---'
      ENDIF
      IF(IISELEC.EQ.3) THEN
	OLD79=.TRUE.
	MIRIM='IRI79'
      ELSE
	OLD79=.FALSE.
	MIRIM='-----'
      ENDIF
      IF(IISELEC/2*2.NE.IISELEC) THEN
	GULB0=.FALSE.
	MB0='-----'
      ELSE
	GULB0=.TRUE.
	MB0='B0Gul'
      ENDIF
2810  IF(NOTBEG) GOTO 3293
c
c f peak density ....................................................
c
5506  IF(NODEN) GOTO 5507
      WRITE(MONITO,5050) IJFOF2 
5050  FORMAT(1X//' NMF2/FOF2 input ?    * F PEAK DENSITY / FREQUENCY',
     & ' *',6X,'#',I1,'#'/1X,60('-')/' !! OPTIONS:  enter  0    if ',
     & 'you want to enter your own'/' !!',23X,'NMF2/m-3 (or foF2/',
     & 'MHz) values.'/' !!',11x,'enter  1',
     & '    if you want to use the CCIR-67 model'/' !!',11X,'enter',
     & '  2    if you want to use the URSI-89 model'/' !!',23X,
     &        'URSI-89 was adopted as IRI standard.')
      WRITE(MONITO,8640)
	MAXI=2
	MINI=0
5701  READ(EGNR,*,ERR=5702,END=3330) IJFOF2
      IF((IJFOF2.LE.MAXI).AND.(IJFOF2.GE.MINI)) GOTO 5704
5702	WRITE(MONITO,8101) MINI,MAXI
	GOTO 5701
5704  IF(IJFOF2.EQ.0) THEN
	FOF2IN=.TRUE.
	MFOF2='INPUT'
      ELSE IF(IJFOF2.EQ.2) THEN
	URSIF2=.TRUE.
	FOF2IN=.FALSE.
	MFOF2='URSI '
      ELSE
	URSIF2=.FALSE.
	MFOF2='CCIR '
	FOF2IN=.FALSE.
      ENDIF
C
      IF(.NOT.FOF2IN) GOTO 5709
	IF((LANZ.GT.10).AND.(.NOT.HEIPRO)) THEN
	  WRITE(MONITO,5712)
5712  FORMAT(' ** MORE THAN 10 INPUT VALUES ARE NEEDED FOR F2/',
     &	'PEAK. IN SUCH **'/' ** CASES THE PROGRAM AUTOMATICALLY ',
     &	'USES THE CCIR-MODEL OPTION. **'/' ** TO AVOID THIS ',
     &	'DURING THE NEXT RUN, YOU SHOULD DECREASE YOUR **'/' ** ',
     &  'VARIABLE STEPSIZE OR RANGE, SO THAT NO MORE THAN 10 ',
     &  'INPUT  **'/' ** VALUES ARE NEEDED.')
	  URSIF2=.FALSE.
	  MFOF2='CCIR '
	  FOF2IN=.FALSE.
	  GOTO 5709
	  ENDIF
C
	X=BVAR-SVAR
        XMINI=0.
	IF(HEIPRO) THEN
	  LLANZ=1
	ELSE
	  LLANZ=LANZ
	ENDIF
	DO 5721 IJK=1,LLANZ
	  X=X+SVAR
	  IF(HEIPRO) THEN
	    WRITE(MONITO,5715) AFOF2(IJK)
	  ELSE
	    WRITE(MONITO,5711) ITEXT(IVAR),IMZ(IVAR),X,AFOF2(IJK) 
	  ENDIF
5715      FORMAT(1X//' NMF2/M-3 (OR FOF2/MHZ) FOR HEIGHT PROFILE',
     &      8X,'#',E9.3,'#')
5711      FORMAT(1X//' NMF2/M-3 (OR FOF2/MHZ) FOR ',A5,1x,A4,'=',
     &	    F7.1,4X,'#',E9.3,'#')
          WRITE(MONITO,8640)
	  ZFOF2=AFOF2(IJK)
8145      READ(EGNR,*,ERR=8146,END=3330) ZFOF2
          IF((ZFOF2.LE.XNMAX).AND.(ZFOF2.GE.XMINI)) GOTO 8147
8146  	  WRITE(MONITO,8148) XMINI,XNMAX
      	  GOTO 8145
8147      IF(ZFOF2.GT.100.0) THEN
	    AFOF2(IJK)=SQRT(ZFOF2/1.24E10)
          ELSE 
	    AFOF2(IJK)=ZFOF2
          ENDIF
5721	CONTINUE
c
c f peak altitude ..................................................
c
5709  WRITE(MONITO,5751) IJHMF2
5751  FORMAT(1X//' HMF2-input ?',12X,'* F PEAK ALTITUDE *',14X,'#'
     &  ,I1,'#'/1X,60('-')/' !! OPTIONS: enter  0   if you want to',
     &  ' enter HMF2/km values.'/' !!          enter  1   if you ',
     &  'want to use the CCIR-M3000 based model')
      WRITE(MONITO,8640)
	MAXI=1
	MINI=0
6701  READ(EGNR,*,ERR=6702,END=3330) IJHMF2
      IF((IJHMF2.LE.MAXI).AND.(IJHMF2.GE.MINI)) GOTO 6704
6702	WRITE(MONITO,8101) MINI,MAXI
	GOTO 6701
6704  IF(IJHMF2.EQ.0) THEN
	HMF2IN=.TRUE.
	MHMF2='INPUT'
      ELSE 
	MHMF2='CCIR '
	HMF2IN=.FALSE.
      ENDIF
C
      IF(.NOT.HMF2IN) GOTO 6709
	IF((LANZ.GT.10).AND.(.NOT.HEIPRO)) THEN
	  WRITE(MONITO,5712)
	  HMF2IN=.FALSE.
	  GOTO 6709
	  ENDIF
C
	X=BVAR-SVAR
	IF(HEIPRO) THEN
	  LLANZ=1
	ELSE
	  LLANZ=LANZ
	ENDIF
	DO 6721 IJK=1,LLANZ
	  X=X+SVAR
	  IF(HEIPRO) THEN
	    WRITE(MONITO,6715) AHMF2(IJK)
	  ELSE
	    WRITE(MONITO,6711) ITEXT(IVAR),IMZ(IVAR),X,AHMF2(IJK) 
	  ENDIF
6715      FORMAT(1X//' HMF2/KM FOR HEIGHT PROFILE',26X,'#',F6.1,'#')
6711      FORMAT(1X//' HMF2/KM FOR ',A5,1X,A4,'=',F7.1,22X,
     &	     '#',F6.1,'#')
          WRITE(MONITO,8640)
          XMAX=800.
	  XMIN=100.
8149      READ(EGNR,*,ERR=8150,END=3330) AHMF2(IJK)
	IF((AHMF2(IJK).LE.XMAX).AND.(AHMF2(IJK).GE.XMIN)) GOTO 6721
8150  	    WRITE(MONITO,8117) XMIN,XMAX
      	    GOTO 8149
6721	CONTINUE

6709  IF(NOTBEG) GOTO 3293
c
c Te = f ( Ne )  option ..........................................
c
5507  IF(NOTEM) GOTO 8155
      IF(IVAR.NE.6) GOTO 8155
      WRITE(MONITO,5060) XNAR
5060  FORMAT(1X//'  NE(300), NE(400), NE(600) ?    [m-3]'/33X,'#',
     & E8.2,',',E8.2,',',E8.2,'#'/1X,60('-')/' !! ELECTRON TEMPE',
     & 'RATURE:'/' !!    Option to use the electron density (NE)',
     & ' dependent'/' !!    models at 300, 400, 600 km altitude'/
     & ' !!    (NE(H)=0  model is not used at altitude h).'/1X,
     & 60('-')/' !!    This option is only allowed, when you gene',
     & 'rate'/' !!    altitude profiles (i.e. variable=height).')
      WRITE(MONITO,8640)
8152  READ(EGNR,*,ERR=8153,END=3330) XNAR
      DO 8154 JXNAR=1,3
  	DLA=XNAR(JXNAR)
	TCON(JXNAR)=.FALSE. 
 	IF((DLA.GT.XNMAX).OR.(DLA.LT.XMIN0)) GOTO 8153
8154	IF(DLA.GT.0.) TCON(JXNAR)=.TRUE. 
      TENEOP=(TCON(1).OR.TCON(2).OR.TCON(3))
      GOTO 8155
8153  WRITE(MONITO,8148) XMIN0,XNMAX
      GOTO 8152
8155  IF(NOTBEG) GOTO 3293
c
c interactive parameter input finished .............................
c
5300    TNOPIV=((TENEOP).AND.(IVAR.EQ.6))
	write(*,*) '*** IRI parameters are being calculated ***'
      if(NODEN) goto 2889
	if(LAYVER) write(*,*) 'Ne, E-F: The LAY-Version is ',
     &	  'prelimenary. Erroneous profile features can occur.'
	if(GULB0) write(*,*) 'Ne, B0: Bottomside thickness is ',
     &	  'obtained with Gulyaeva-1987 model.'
	if(OLD79) write(*,*) 'Ne: Using IRI-79. Correction',
     &	  ' of equatorial topside is not included.'
	if(HMF2IN) write(*,*) 'Ne, hmF2: Input values are used.'
	if(FOF2IN) then
	  write(*,*) 'Ne, foF2: Input values are used.'
	  goto 2889
	  endif
	if(URSIF2) then
	  write(*,*) 'Ne, foF2: URSI model is used.'
	else
	  write(*,*) 'Ne, foF2: CCIR model is used.'
	endif
2889  if((.not.NOION).and.(DY)) 
     &	   write(*,*) 'Ion Com.: Using Danilov-Yaichnikov-1985.'
      if((.not.NOTEM).and.(TNOPIV))
     &     write(*,*) 'Te: Temperature-density correlation is used.'
c
c table head .......................................................
c
      NOTBEG=.TRUE.
	IF(ITABLA.EQ.0) THEN
	   ITEVA=IVAR
	ELSE 
	   ITEVA=7+ITABLA*IVAR
	ENDIF
	IF(ITEVA.GT.11) ITEVA=11
      PIKTAB=0
	IF(IVAR.NE.6) THEN
		IF(XHIGHT.LT.1.0) PIKTAB=1
		IF(XHIGHT.LT.0.0) PIKTAB=2
		ENDIF
      IF(PIKTAB.GT.0) THEN
        WRITE(AGNR,8192) ITEXT(ITEVA),IMZ(ITEVA)
      ELSE
	HEIGHT=XHIGHT
        WRITE(AGNR,8193) ITEXT(ITEVA),IMZ(ITEVA)
      ENDIF
      IF(.NOT.STORE) GOTO 8444
   	IF(PIKTAB.GT.1) THEN
          WRITE(IUOUT,8192) ITEXT(ITEVA),IMZ(ITEVA)
      	ELSE IF(PIKTAB.GT.0) THEN
          WRITE(IUOUT,8194) ITEXT(ITEVA),IMZ(ITEVA)
	ELSE
          WRITE(IUOUT,8193) ITEXT(ITEVA),IMZ(ITEVA)
      	ENDIF
8192  FORMAT(//////////3X,A5,5X,'PEAK ALTITUDES IN KM',12X,'PEAK DEN',
     &  'SITIES IN CM-3'/4X,A4,'   HMF2 h0.5 B0 HMF1 HME HMD     ',
     &  'NMF2   NMF1   NVBot   NME     NMD')
8194  FORMAT(//////////3X,A5,5X,'PEAK ALTITUDES IN KM',11X,'PLASMA F',
     & 'REQUENCIES IN MHz'/4X,A4,'   HMF2 h0.5 B0 HMF1 HME HMD     ',
     &  'foF2   foF1   fVBot   foE     foD')
8193  FORMAT(//////////3X,A5,'  ELECTRON DENSITY      TEMPERATURES',
     &  7X,'ION PERCENTAGE DENSITIES'/4X,A4,'  NE/CM-3 NE/NMF2  ',
     &  'TN/K  TI/K  TE/K  TE/TI   O+   H+  He+  O2+  NO+')

c
c assign paramters to variable array ...............................
c
8444    IF(GEMA) THEN
	  XVAR(1)=MLAT
	  XVAR(2)=MLONG
	ELSE
  	  XVAR(1)=LATI
          XVAR(2)=LONGI
        ENDIF
        XVAR(3)=R
        XVAR(4)=MONTH
        IF(UNTI) THEN
	  XVAR(5)=UT
	ELSE
          XVAR(5)=HOUR
	ENDIF
        XVAR(6)=HEIGHT
	XVAR(7)=DAYNR
        LFD=0
        XVAR(IVAR)=BVAR-SVAR

c
c parameter loop begins here ......................................
c
2123    XVAR(IVAR)=XVAR(IVAR)+SVAR
        LFD=LFD+1
        IF(GEMA) THEN
           MLAT=XVAR(1)
           MLONG=XVAR(2)
        ELSE
           LATI=XVAR(1)
           LONGI=XVAR(2)
        ENDIF
        R=XVAR(3)
        IF(XVAR(4).LT.13.) then
		MONTH=XVAR(4)
		zmonth = month + iday / 30.0
		endif
        IF(UNTI) THEN
	  UT=XVAR(5)
	ELSE
          HOUR=XVAR(5)
        ENDIF
        HEIGHT=XVAR(6)
	DAYNR=XVAR(7)
C
C CALCULATION OF MEAN F10.7CM SOLAR RADIO FLUX (COV)................
C CALCULATION OF RESTRICTED SOLAR ACTIVITIES (RG,COVG)..............
C
      COV=63.75+R*(0.728+R*0.00089)
      RG=R
      COVG=COV
      IF(R.GT.150.) RG=150.
      IF(COV.GT.193.) COVG=193.
C
C CALCULATION OF GEOG. OR GEOM. COORDINATES IN DEG....................
C CALCULATION OF MAGNETIC INCLINATION (DIP), DECLINATION (DEC)........
C   DIP LATITUDE (MAGBR) AND MODIFIED DIP (MODIP). ALL IN DEGREE......
C
      IF((LFD.EQ.1).OR.(IVAR.LT.3)) THEN
        CALL GGM(JMAG,LONGI,LATI,MLONG,MLAT)
        ABSLAT=ABS(LATI)
        CALL FIELDG(LATI,LONGI,300.0,XMA,YMA,ZMA,BET,DIP,DEC,MODIP)
        MAGBR=ATAN(0.5*TAN(DIP*UMR))/UMR
	absmlt=abs(mlat)
	ABSMDP=abs(modip)
	ABSMBR=abs(magbr)
      ENDIF     
C
C CALCULATION OF SEASON (SUMMER=2, WINTER=4)..........................
C CALCULATION OF DAY OF YEAR AND SUN DECLINATION......................
C
	if(ivar.eq.7) call moda(1,month,iday,daynr)
        if(ivar.eq.4) call moda(0,month,iday,daynr)
	zmonth = month + iday / 30.0
      SEASON=INT((DAYNR+45.0)/92.0)
      IF(SEASON.LT.1) SEASON=4
      NSEASN=SEASON
      seaday=daynr
      IF(LATI.GT.0.0) GOTO 5592
   	SEASON=SEASON-2
    	IF(SEASON.LT.1) SEASON=SEASON+4
	seaday=daynr+183
	if(seaday.gt.366) seaday=seaday-366
C
C CALCULATION OF SOLAR ZENITH ANGLE (XHI/DEG).........................
C NOON VALUE (XHINON).................................................
C
5592  IF(UNTI) THEN
	HOUR=UT+LONGI/15.
	IF(HOUR.GT.24.) HOUR=HOUR-24.
      ELSE
        UT=HOUR-LONGI/15.
        IF(UT.LT.0.) UT=UT+24.
      ENDIF

	CALL SOCO(DAYNR,HOUR,LATI,LONGI,SUNDEC,XHI,SAX,SUX)
	CALL SOCO(DAYNR,12.0,LATI,LONGI,SUNDE1,XHINON,SAXNON,SUXNON)

	        NIGHT=.FALSE.
	if(abs(sax).gt.25.0) then
                if(sax.lt.0.0) NIGHT=.TRUE.
		goto 1334
		endif
      	if(SAX.le.SUX) goto 1386
	if((hour.gt.sux).and.(hour.lt.sax)) night=.true.
	goto 1334
1386  	IF((HOUR.GT.SUX).OR.(HOUR.LT.SAX)) NIGHT=.TRUE.
1334    XVAR(8)=MAGBR
        XVAR(9)=MODIP
        XVAR(10)=COV
        XVAR(11)=XHI
C
C CALCULATION OF ELECTRON DENSITY PARAMETERS................
C
      HNEA=65.
      IF(NIGHT) HNEA=80.
      HNEE=2000.
      IF(NODEN) GOTO 4933
      DELA=4.32
      IF(ABSMDP.GE.18.) DELA=1.0+EXP(-(ABSMDP-30.0)/10.0)
C      DELL=1+EXP(-(ABSLAT-20.)/10.)
C!!!!!!! F-REGION PARAMETERS AND E-PEAK !!!!!!!!!!!!!!!!!!!!!!!!!!
      FOE=FOEEDI(COV,XHI,XHINON,ABSLAT)
      NME=1.24E10*FOE*FOE
      HME=105.0
      IF((FOF2IN).AND.(HMF2IN)) GOTO 501
      IF((URSIF2).NEQV.(URSIFO)) GOTO 7739
      IF((MONTH.EQ.MONTHO).AND.(RG.EQ.RGO)) GOTO 4292
      IF(MONTH.EQ.MONTHO) GOTO 4291
C
C READ CCIR COEFFICIENT SET FOR CHOSEN MONTH....................
C
7739    WRITE(FILENA,104) MONTH+10
	URSIFO=URSIF2
1344	lread=1
        OPEN(IUCCIR,FILE=FILENA,STATUS='OLD',ERR=8448,
     &		FORM='FORMATTED')
        READ(IUCCIR,4689) F2,FM3
104     FORMAT('CCIR',I2,'.ASC')
4689    FORMAT(1X,4E15.8)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!! FOR BINARY CODED CCIR COEFFIECENTS FILES SUBSTITUTE !!
C!! THE LAST FOUR STATEMENTS WITH THE FOLLOWING THREE:  !!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c        OPEN(IUCCIR,FILE=FILENA,STATUS='OLD',ERR=8448,
c     &		FORM='UNFORMATTED')
c        READ(IUCCIR) F2,FM3
c104     FORMAT('CCIR',I2,'.BIN')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CLOSE(IUCCIR)
	MONTHO=MONTH
C
C READ URSI COEFFICIENT SET FOR CHOSEN MONTH....................
C
	if(URSIF2) then
	  WRITE(FILENA,1144) MONTH+10
1244      lread=2
        OPEN(IUCCIR,FILE=FILENA,STATUS='OLD',ERR=8448,
     &		FORM='FORMATTED')
        READ(IUCCIR,4689) F2
1144     FORMAT('URSI',I2,'.ASC')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!! FOR BINARY CODED CCIR COEFFIECENTS FILES SUBSTITUTE !!
C!! THE LAST THREE STATEMENTS WITH THE FOLLOWING THREE: !!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c	  OPEN(IUCCIR,FILE=FILENA,STATUS='OLD',ERR=8448,
c     &		FORM='UNFORMATTED')
c          READ(IUCCIR) F2
c1144    FORMAT('URSI',I2,'.BIN')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          CLOSE(IUCCIR)
	  endif
	GOTO 4291

8448	WRITE(MONITO,8449) FILENA
8449	FORMAT(1X////,
     &	  ' !!!! The file ',A10,' is not in your directory.'/
     &	  ' !!!!   Load a different diskette and enter  1,'/
     &    ' !!!!   or exit by entering  0)')
	read(*,*) idisk
	if(idisk.gt.0) goto (1344,1244) lread
	GOTO 3330
C
C LINEAR INTERPOLATION IN SOLAR ACTIVITY
C
4291    RR2=RG/100.
        RR1=1.-RR2
        DO 20 I=1,76
        DO 20 J=1,13
        K=J+13*(I-1)
20      FF0(K)=F2(J,I,1)*RR1+F2(J,I,2)*RR2
        DO 30 I=1,49
        DO 30 J=1,9
        K=J+9*(I-1)
30      XM0(K)=FM3(J,I,1)*RR1+FM3(J,I,2)*RR2
	RGO=RG

4292  CALL F2OUT(MODIP,LATI,LONGI,FF0,XM0,UT,YFOF2,XM3000)

501	IF(FOF2IN) THEN
	  FOF2=AFOF2(LFD)
	ELSE
          FOF2=YFOF2
	ENDIF
      	NMF2=1.24E10*FOF2*FOF2

	IF(HMF2IN) THEN
	  HMF2=AHMF2(LFD)
	ELSE
          HMF2=HMF2ED(MAGBR,RG,FOF2/FOE,XM3000)
	ENDIF
	IF((.not.HEIPRO).and.(PIKTAB.lt.1)) THEN
	   TOPSI=(HEIGHT.GE.HMF2)
	   BOTTO=((HEIGHT.GE.HME).AND.(HEIGHT.LT.HMF2))
	   BELOWE=(HEIGHT.LT.HME)
	ELSE
	   TOPSI=.TRUE.
	   BOTTO=.TRUE.
	   BELOWE=.TRUE.
	ENDIF
c
c topside profile parameters .............................
c
	IF(.NOT.TOPSI) GOTO 1501
      COS2=COS(MLAT*UMR)
      COS2=COS2*COS2
      FLU=(COVG-40.0)/30.0
      IF(OLD79) then
        ETA1=-0.0070305*COS2
      else
        EX=EXP(-MLAT/15.)
        EX1=EX+1
        EPIN=4.*EX/(EX1*EX1)
        ETA1=-0.02*EPIN
      endif
      ETA=0.058798+ETA1+FLU*(-0.014065+0.0069724*COS2)+
     &(0.0024287+0.0042810*COS2-0.00015280*FOF2)*FOF2
      ZETA=0.078922-0.0046702*COS2+FLU*(-0.019132+0.0076545*COS2)+
     &(0.0032513+0.0060290*COS2-0.00020872*FOF2)*FOF2
      BETA=-128.03+20.253*COS2+FLU*(-8.0755-0.65896*COS2)+(0.44041
     &+0.71458*COS2-0.042966*FOF2)*FOF2
      Z=EXP(94.45/BETA)
      Z1=Z+1
      Z2=Z/(BETA*Z1*Z1)
      DELTA=(ETA/Z1-ZETA/2.0)/(ETA*Z2+ZETA/400.0)
c
c bottomside profile parameters .............................
C
1501    HEF=HME
	HMF1=HMF2
        HZ=HMF2
        IF(.not.BOTTO) GOTO 2727  
	B1=3.0
C!!!!!!! INTERPOLATION FOR B0 OUT OF ARRAY B0F !!!!!!!!!!!!!!!!!!!!!
	if(GULB0) then
	  call ROGUL(SEADAY,XHI,SEAX,GRAT)
	  if(NIGHT) GRAT=0.91-HMF2/4000.
	  B0CNEW=HMF2*(1.-GRAT)
	  B0=B0CNEW/B0B1(1)
	else
          B0 = B0_TAB(HOUR,SAX,SUX,NSEASN,RG,MODIP)
	  GRAT = 1. - B0 * B0B1(1)/HMF2
	endif
C!!!!!!! F1-REGION PARAMETERS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      F1REG=.FALSE.
      HMF1=0.
      PNMF1=0.
      C1=0.  
      IF(NIGHT.OR.(SEASON.EQ.4)) GOTO 150
        FOF1=FOF1ED(ABSMBR,R,XHI)
        IF(FOF1.LT.1.E-3) GOTO 150
          F1REG=.TRUE.
          C1=.09+.11/DELA
          PNMF1=1.24E10*FOF1*FOF1
150   NMF1=PNMF1
C!!!!!!! PARAMETER FOR E AND VALLEY-REGION !!!!!!!!!!!!!!!!!!!!!
      XDEL=XDELS(SEASON)/DELA
      DNDHBR=DNDS(SEASON)/DELA
      HDEEP=HPOL(HOUR,10.5/DELA,28.,SAX,SUX,1.,1.)
      WIDTH=HPOL(HOUR,17.8/DELA,45.+22./DELA,SAX,SUX,1.,1.)
      DEPTH=HPOL(HOUR,XDEL,81.,SAX,SUX,1.,1.)
      DLNDH=HPOL(HOUR,DNDHBR,.06,SAX,SUX,1.,1.)
      IF(DEPTH.LT.1.0) GOTO 600
	IF(NIGHT) THEN
	  XDEPTH=-DEPTH
	ELSE
	  XDEPTH=DEPTH
	ENDIF  
      	CALL TAL(HDEEP,XDEPTH,WIDTH,DLNDH,EXT,E)
      	IF(.NOT.EXT) GOTO 667
      	  WRITE(KONSOL,650)
650   FORMAT(1X,'*NE* E-REGION VALLEY CAN NOT BE MODELLED')
600   	  WIDTH=.0
667   HEF=HME+WIDTH
      VNER = (1. - DEPTH / 100.) * NME
c
c Parameters below E  .............................
c
2727	IF(.not.BELOWE) GOTO 2726
C!!!!!!!D-REGION PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NMD=XMDED(XHI,R,4.0E8)
      HMD=HPOL(HOUR,81.0,88.0,SAX,SUX,1.,1.)
      F(1)=HPOL(HOUR,0.02+0.03/DELA,0.05,SAX,SUX,1.,1.)
      F(2)=HPOL(HOUR,4.6,4.5,SAX,SUX,1.,1.)
      F(3)=HPOL(HOUR,-11.5,-4.0,SAX,SUX,1.,1.)
      FP1=F(1)
      FP2=-FP1*FP1/2.0
      FP30=(-F(2)*FP2-FP1+1.0/F(2))/(F(2)*F(2))
      FP3U=(-F(3)*FP2-FP1-1.0/F(3))/(F(3)*F(3))
      HDX=HMD+F(2)
      X=HDX-HMD
      XDX=NMD*EXP(X*(FP1+X*(FP2+X*FP30)))
      DXDX=XDX*(FP1+X*(2.0*FP2+X*3.0*FP30))
      X=HME-HDX
      XKK=-DXDX*X/(XDX*ALOG(XDX/NME))
      D1=DXDX/(XDX*XKK*X**(XKK-1.0))
C
C SEARCH FOR HMF1 ..................................................
C
2726	IF(.not.BOTTO) GOTO 4933
	if(LAYVER) goto 6153
924	IF(.not.F1REG) GOTO 380
	XE2H=XE2(HEF)
      CALL REGFA1(HEF,HMF2,XE2H,NMF2,0.001,NMF1,XE2,SCHALT,HMF1)
	IF(.not.SCHALT) GOTO 380
	  WRITE(KONSOL,11)
11    FORMAT(1X,'*NE* HMF1 IS NOT EVALUATED BY THE FUNCTION XE2')
	IREGFA=1
c
c change B1 and try again ..........................................
c
9244 	IF(B1.GT.4.5) GOTO (7398,8922) IREGFA
	   	B1=B1+0.5
 		WRITE(KONSOL,902) B1-0.5,B1
902   FORMAT(6X,'CORR.: B1(OLD)=',F4.1,' B1(NEW)=',F4.1)
		IF(GULB0) then
			  ib1=int(b1*2.-5.)
			  B0=B0CNEW/b0b1(ib1)
			else
			  GRAT = 1. - B0 * B0B1(ib1)/HMF2			
			endif
   		GOTO 924
c
c omit F1 feature ....................................................
c
7398  WRITE(KONSOL,9269)
9269  FORMAT(1X,'CORR.: NO F1 REGION, B1=3, C1=0.0')
      	HMF1=0.
      	NMF1=0.
      	C1=0.0
      	B1=3.
      	F1REG=.FALSE.

C
C SEARCH FOR HST [NE3(HST)=NME] ..........................................
C
380	RRRR=0.5
	IF(F1REG) then
		hf1=hmf1
		xf1=nmf1
		GOTO 3972
		ENDIF
	RATHH=0.5
3973	hf1=hef+(hmf2-hef)*RATHH
	xf1=xe3(hf1)
	IF(XF1.LT.NME) THEN
		RATHH=RATHH+.1
		GOTO 3973
		ENDIF
3972	h=hf1
	deh=10.
	XXMIN=XF1
	HHMIN=HF1
3895    h=h-deh
	if(h.lt.HEF) then
	  h=h+2*deh
	  deh=deh/10.
	  if(deh.lt.1.) goto 3885
	  endif
   	XE3H=XE3(h)
	IF(XE3H.LT.XXMIN) THEN
	  XXMIN=XE3H
	  HHMIN=h
	  endif
	if(XE3H.gt.NME) goto 3895
      CALL REGFA1(h,HF1,XE3H,XF1,0.001,NME,XE3,SCHALT,HST)
	STR=HST
	IF(.not.SCHALT) GOTO 360
3885	WRITE(KONSOL,100)
100   FORMAT(1X,'*NE* HST IS NOT EVALUATED BY THE FUNCTION XE3')
	IREGFA=2
	IF(XXMIN/NME.LT.1.3) GOTO 9244
c
c assume linear interpolation between HZ and HEF ..................
c
8922	HZ=HHMIN+(HF1-HHMIN)*RRRR
	XNEHZ=XE3(HZ)
	if(xnehz-nme.lt.0.001) then
	  RRRR=RRRR+.1
	  GOTO 8922
  	  endif
	WRITE(KONSOL,901) HZ,HEF
901   FORMAT(6X,'CORR.: LIN. APP. BETWEEN HZ=',F5.1,
     & 		' AND HEF=',F5.1)
      	T=(XNEHZ-NME)/(HZ-HEF)
      	HST=-333.
	GOTO 4933
c
c calculate HZ, D and T ............................................
c
360	HZ=(HST+HF1)/2.0
    	D=HZ-HST
    	T=D*D/(HZ-HEF-D)
	GOTO 4933
C
C LAY-functions for middle ionosphere
C
6153	HMF1M=165.+0.6428*XHI
	HHALF = GRAT * HMF2
	HV1R = HME + WIDTH
	HV2R = HME + HDEEP
	HHMF2 = HMF2
	CALL INILAY(NIGHT,NMF2,NMF1,NME,VNER,HHMF2,HMF1M,HME,
     &			HV1R,HV2R,HHALF,HXL,SCL,AMP,IIQU)
	IF(IIQU.EQ.1) WRITE(KONSOL,7733)
7733	FORMAT('*NE* LAY amplitudes found with 2nd choice of HXL(1).')
	IF(IIQU.EQ.2) WRITE(KONSOL,7722)
7722	FORMAT('*NE* LAY amplitudes could not be found.')

C---------- CALCULATION OF NEUTRAL TEMPERATURE PARAMETER-------

4933  HTA=120.0
      HTE=3000.0
	IF(NOTEM) GOTO 240
      SEC=UT*3600.
      CALL CIRA86(DAYNR,SEC,LATI,LONGI,HOUR,COV,TEXOS,TN120,SIGMA)
	IF(HOUR.NE.0.0) THEN
      SECNI=(24.-LONGI/15.)*3600.
      CALL CIRA86(DAYNR,SECNI,LATI,LONGI,0.,COV,TEXNI,TN1NI,SIGNI)
	ELSE
      TEXNI=TEXOS
      TN1NI=TN120
      SIGNI=SIGMA
        ENDIF
      TLBDH=TEXOS-TN120
      TLBDN=TEXNI-TN1NI
C
C--------- CALCULATION OF ELECTRON TEMPERATURE PARAMETER--------
C
881   CONTINUE

C !!!!!!!!!! TE(120KM)=TN(120KM) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ATE(1)=TN(AHH(1),TEXOS,TLBDH,SIGMA)

C !!!!!!!!!! TE-MAXIMUM (JICAMARCA,ARECIBO) !!!!!!!!!!!!!!!!!!!!
      HMAXD=60.*EXP(-(MLAT/22.41)**2)+210.
      HMAXN=150.
      AHH(2)=HPOL(HOUR,HMAXD,HMAXN,SAX,SUX,1.,1.)
      TMAXD=800.*EXP(-(MLAT/33.)**2)+1500.
      TMAXN=TN(HMAXN,TEXNI,TLBDN,SIGNI)+20
      ATE(2)=HPOL(HOUR,TMAXD,TMAXN,SAX,SUX,1.,1.)

C !!!!!!!!!! TE(300,400KM)=TE-AE-C !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! TE(1400,3000KM)=TE-ISIS !!!!!!!!!!!!!!!!!!!!!!!!!!!
	DIPLAT=MAGBR
      CALL TEBA(DIPLAT,HOUR,NSEASN,TEA)
      ATE(3)=TEA(1)
      ATE(4)=TEA(2)
      ATE(6)=TEA(3)
      ATE(7)=TEA(4)

C !!!!!!!!!! TE(600KM)=TE-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ETT=EXP(-MLAT/11.35)
      TET=2900.-5600.*ETT/((ETT+1)**2.)
      TEN=839.+1161./(1.+EXP(-(ABSMLT-45.)/5.))
      ATE(5)=HPOL(HOUR,TET,TEN,SAX,SUX,1.5,1.5)

C !!!!!!!!!! OPTION TO USE TE-NE-RELATION !!!!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! AT 300, 400 OR 600 KM  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(TNOPIV) THEN
        DO 3395 I=1,3
3395	  IF(TCON(I)) ATE(I+2)=TEDE(HOA(I),XNAR(I),-COV)
	ENDIF
C !!!!!!!!!! TE'S ARE CORRECTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! ALSO TE > TN ENFORCED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      TNAHH2=TN(AHH(2),TEXOS,TLBDH,SIGMA)
      IF(ATE(2).LT.TNAHH2) ATE(2)=TNAHH2
      STTE1=(ATE(2)-ATE(1))/(AHH(2)-AHH(1))
      DO 1901 I=2,6
       TNAHHI=TN(AHH(I+1),TEXOS,TLBDH,SIGMA)
       IF(ATE(I+1).LT.TNAHHI) ATE(I+1)=TNAHHI
       STTE2=(ATE(I+1)-ATE(I))/(AHH(I+1)-AHH(I))
       ATE(I)=ATE(I)-(STTE2-STTE1)*DTE(I-1)*ALOG2
1901  STTE1=STTE2
C !!!!!!!!!! GRADIENTS ARE CALCULATED WITH !!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! CORRECTED REGION BOUNDARIES !!!!!!!!!!!!!!!!!!!!!!
      DO 1902 I=1,6
1902  STTE(I)=(ATE(I+1)-ATE(I))/(AHH(I+1)-AHH(I))
      ATE1=ATE(1)
887   CONTINUE
C
C------------ CALCULATION OF ION TEMPERATURE PARAMETERS--------
C
C !!!!!!!!!! TI(430KM,DAY)=TI-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!!!
      XSM(1)=430.0
      Z1=EXP(-0.09*MLAT)
      Z2=Z1+1.
      TID1 = 1240.0 - 1400.0 * Z1 / ( Z2 * Z2 )
      MM(2)=HPOL(HOUR,3.0,0.0,SAX,SUX,1.,1.)
C !!!!!!!!!! TI < TE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        TED1=TEA(6)+30.
        IF(TID1.GT.TED1) TID1=TED1

C !!!!!!!!!! TI(430KM,NIGHT)=TI-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!
      Z1=ABSMLT
      Z2=Z1*(0.47+Z1*0.024)*UMR
      Z3=COS(Z2)
      TIN1=1200.0-300.0*SIGN(1.0,Z3)*SQRT(ABS(Z3))
C !!!!!!!!!! TN < TI < TE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        TEN1=TEA(5)
	TNN1=TN(XSM(1),TEXNI,TLBDN,SIGNI)
        IF(TEN1.LT.TNN1) TEN1=TNN1
        IF(TIN1.GT.TEN1) TIN1=TEN1
        IF(TIN1.LT.TNN1) TIN1=TNN1

C !!!!!!!!!! TI(430KM,LT) FROM STEP FUNCTION !!!!!!!!!!!!!!!!!!
	TI1=TIN1  
 	IF(TID1.GT.TIN1) TI1=HPOL(HOUR,TID1,TIN1,SAX,SUX,1.,1.)

C !!!!!!!!!! TANGENT ON TN DETERMINES HS !!!!!!!!!!!!!!!!!!!!!!
	TI13=TEDER(130.)
	TI50=TEDER(500.)
      CALL REGFA1(130.0,500.0,TI13,TI50,0.01,TI1,TEDER,SCHALT,HS)
      IF(SCHALT) HS=200.
      TNHS=TN(HS,TEXOS,TLBDH,SIGMA)
      MM(1)=DTNDH(HS,TEXOS,TLBDH,SIGMA)
      IF(SCHALT) MM(1)=(TI1-TNHS)/(XSM(1)-HS)
      MXSM=2

C !!!!!!!!!! XTETI ALTITTUDE WHERE TE=TI !!!!!!!!!!!!!!!!!!!!!!
2391    XTTS=500.
        X=500.
2390    X=X+XTTS
        IF(X.GE.AHH(7)) GOTO 240
        TEX=ELTE(X)
        TIX=TI(X)
        IF(TIX.LT.TEX) GOTO 2390
        X=X-XTTS
        XTTS=XTTS/10.
        IF(XTTS.GT.0.1) GOTO 2390
        XTETI=X+XTTS*5.

C !!!!!!!!!! TI=TE ABOVE XTETI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        MXSM=3
        MM(3)=STTE(6)
        XSM(2)=XTETI
        IF(XTETI.GT.AHH(6)) GOTO 240
        MXSM=4
        MM(3)=STTE(5)
        MM(4)=STTE(6)
        XSM(3)=AHH(6)
        IF(XTETI.GT.AHH(5)) GOTO 240
        MXSM=5
        DTI(1)=5.
        DTI(2)=5.
        MM(3)=STTE(4)
        MM(4)=STTE(5)
        MM(5)=STTE(6)
        XSM(3)=AHH(5)
        XSM(4)=AHH(6)
C
C CALCULATION OF ION DENSITY PARAMETER..................
C
240   IF(NOION) GOTO 141
      HNIA=100.
      HNIE=2000.
	if(DY) goto 141
C
C INPUT OF THE ION DENSITY PARAMETER ARRAYS PF1O,PF2O AND PF3O......
C
      RIF(1)=2.
      IF(ABSLAT.LT.30.0) RIF(1)=1.
      RIF(2)=2.
      IF(COV.LT.100.0) RIF(2)=1.
      RIF(3)=SEASON
      IF(SEASON.EQ.1) RIF(3)=3.
      RIF(4)=1.
      IF(NIGHT) RIF(4)=2.
      CALL KOEFP1(PG1O)
      CALL KOEFP2(PG2O)
      CALL KOEFP3(PG3O)
      CALL SUFE(PG1O,RIF,12,PF1O)
      CALL SUFE(PG2O,RIF, 4,PF2O)
      CALL SUFE(PG3O,RIF,12,PF3O)
c
c calculate O+ profile parameters
c
      IF(ABS(XHI).LE.90.0) THEN
	ZZZ1=COS(XHI*UMR)
      ELSE
	ZZZ1=0.0
      ENDIF
      msumo=4
      RDOMAX=100.0
      MO(1)=EPSTEP(PF1O(1),PF1O(2),PF1O(3),PF1O(4),ZZZ1)
      MO(2)=EPSTEP(PF1O(5),PF1O(6),PF1O(7),PF1O(8),ZZZ1)
      MO(3)=0.0
      HO(1)=EPSTEP(PF1O(9),PF1O(10),PF1O(11),PF1O(12),ZZZ1)
      HO(2)=290.0
      IF((RIF(2).EQ.2.).AND.(RIF(3).EQ.2.)) HO(2)=237.0
      HO(4)=PF2O(1)
	ho05=pf2o(4)
      MO(4)=PF2O(2)
      MO(5)=PF2O(3)
c
c adjust gradient MO(4) of O+ profile segment above F peak
c
7100   	HO(3)=(ALG100-MO(5)*(HO(4)-ho05))/MO(4)+HO(4)
      	IF(HO(3).LE.HO(2)+20.) THEN
		MO(4)=MO(4)-0.001
 		GOTO 7100
		endif
	hfixo=(ho(2)+ho(3))/2.
c
c find height H0O of maximum O+ relative density
c
      DELX=5.0
      X=HO(2)
      YMAXX=0.0
7102  X=X+DELX
      Y=RPID(X,HFIXO,RDOMAX,msumo,MO,DDO,HO)
      IF(Y.LE.YMAXX) then
	if(delx.le.0.1) GOTO 7104
	x=x-delx
	delx=delx/5.
      ELSE
      	YMAXX=Y
      ENDIF
      GOTO 7102 
7104  H0O=X-DELX/2.
7101	if(y.lt.100.0) goto 7103
          rdomax=rdomax-0.01
	y=rpid(h0o,hfixo,rdomax,msumo,mo,ddo,ho)
	goto 7101
7103	yo2h0o=100.-y
	yoh0o=y
c
c calculate parameters for O2+ profile
c
	hfixo2  = pf3o(1)
	rdo2mx = pf3o(2) 
      DO 7105 L=1,2
		I = L * 2
      		HO2(L)=PF3O(1+I)+PF3O(2+I)*ZZZ1
7105  		MO2(L+1)=PF3O(7+I)+PF3O(8+I)*ZZZ1
      MO2(1)=PF3O(7)+PF3O(8)*ZZZ1
	if(hfixo2.gt.ho2(1)) then
	   ymo2z=mo2(2)
	else
	   ymo2z=mo2(1)
	endif
	aldo21=alog(rdo2mx)+ymo2z*(ho2(1)-hfixo2)
	hfixo2=(ho2(2)+ho2(1))/2.
	rdo2mx=exp(aldo21+mo2(2)*(hfixo2-ho2(1)))
c
c make sure that rd(O2+) is less or equal 100-rd(O+) at O+ maximum
c
7106  Y=RPID(H0O,hfixo2,rdo2mx,2,MO2,DO2,HO2)
      IF(Y.GT.yo2h0o) then
      	MO2(3)=MO2(3)-0.02
      	GOTO 7106
	endif
c
C use ratio of NO+ to O2+ density at O+ maximum to calculate
c NO+ density above the O+ maximum (H0O)
c
      IF(y.LT.1.) then
	NOBO2=0.0
      ELSE
	NOBO2= (yo2h0o-y)/y
      ENDIF
C
C CALCULATION FOR THE REQUIRED HEIGHT RANGE.......................
C
141   IF(.NOT.F1REG) HMF1=HZ
      VARIAB=XVAR(IVAR)
      XCOR=XVAR(ITEVA)
c
c special output: peak densities and altitudes
c
      IF(PIKTAB.eq.1) THEN
	IHMF1=0
	IF(F1REG) IHMF1=INT(HMF1+.5)
	IHMF2=INT(HMF2+.5)
	IHHALF=INT(HMF2*GRAT+.5)
	IHME=INT(HME+.5)
	IHMD=INT(HMD+.5)
	IB0=INT(B0+.5)
	iyp1=int(nmf2/1.e6+.5)
	iyp2=int(pnmf1/1.e6+.5)
	iyp3=int(vner/1.e6+.5)
	iyp4=int(nme/1.e6+.5)
	iyp5=int(nmd/1.e6+.5)
        WRITE(AGNR,3910) XCOR,IHMF2,IHHALF,IB0,IHMF1,IHME,IHMD,
     &    iyp1,iyp2,iyp3,iyp4,iyp5
        IF(STORE) WRITE(IUOUT,3910) XCOR,IHMF2,IHHALF,IB0,IHMF1,
     &    IHME,IHMD,iyp1,iyp2,iyp3,iyp4,iyp5
3910    FORMAT(1X,F7.1,2X,2I5,4I4,3X,5I7)
        GOTO 2135
      ENDIF
      IF(PIKTAB.eq.2) THEN
	IHMF1=0
	IF(F1REG) IHMF1=INT(HMF1+.5)
	yp1=sqrt(nmf2/1.24e10)
	yp2=sqrt(pnmf1/1.24e10)
	yp3=sqrt(vner/1.24e10)
	yp4=sqrt(nme/1.24e10)
	yp5=sqrt(nmd/1.24e10)
        WRITE(AGNR,3919) XCOR,INT(HMF2+.5),INT(HMF2*GRAT+.5),
     &    int(B0+.5),IHMF1,INT(HME+.5),INT(HMD+.5),yp1,yp2,yp3,yp4,yp5
        IF(STORE) WRITE(IUOUT,3919) XCOR,INT(HMF2+.5),INT(HMF2*GRAT+.5),
     &    int(B0+.5),IHMF1,INT(HME+.5),INT(HMD+.5),yp1,yp2,yp3,yp4,yp5
3919    FORMAT(1X,F7.1,2X,2I5,4I4,3X,5F7.2)
        GOTO 2135
      ENDIF
c
c normal output
c
300   DO 7397 KI=1,11
7397  OUTF(KI)=-1.6
      OUTF(1)=-1.
      OUTF(2)=-1.
      OUTF(6)=-1.
      IF(NODEN) GOTO 330
      IF((HEIGHT.GT.HNEE).OR.(HEIGHT.LT.HNEA)) GOTO 330
        IF(LAYVER) THEN
	  NEI=-9.
	  IF(IIQU.LT.2)	NEI=XEN(HEIGHT,HMF2,NMF2,HME,4,HXL,SCL,AMP)
	ELSE
	  NEI=XE(HEIGHT)
	ENDIF
      	OUTF(1)=NEI/1.E6
      	OUTF(2)=NEI/NMF2
330   IF(NOTEM) GOTO 7108
      IF((HEIGHT.GT.HTE).OR.(HEIGHT.LT.HTA)) GOTO 7108
      	TNH=TN(HEIGHT,TEXOS,TLBDH,SIGMA)
      	TIH=TNH
      	IF(HEIGHT.GE.HS) TIH=TI(HEIGHT)
      	TEH=ELTE(HEIGHT)
	IF(TIH.LT.TNH) TIH=TNH
	IF(TEH.LT.TIH) TEH=TIH
      	OUTF(3)=TNH
      	OUTF(4)=TIH
      	OUTF(5)=TEH
      	OUTF(6)=TEH/TIH
7108  IF(NOION) GOTO 7118
      IF((HEIGHT.GT.HNIE).OR.(HEIGHT.LT.HNIA)) GOTO 7118
	if(DY) then
      call IONCOM(HEIGHT,XHI*UMR,LATI*UMR,COV,ZMONTH,DION)
      ROX=DION(1)
      RHX=DION(2)
      RNX=DION(3)
      RHEX=DION(4)
      RNOX=DION(5)
      RO2X=DION(6)
      RCLUST=DION(7)
	else
      ROX=RPID(HEIGHT,HFIXO,RDOMAX,msumo,MO,DDO,HO)
      RO2X=RPID(HEIGHT,HFIXO2,rdo2mx,2,MO2,DO2,HO2)
      CALL RDHHE(HEIGHT,H0O,ROX,RO2X,NOBO2,10.,RHX,RHEX)
      RNOX=RDNO(HEIGHT,H0O,RO2X,ROX,NOBO2)
	endif
      OUTF(7)=ROX
      OUTF(8)=RHX
      OUTF(9)=RHEX
      OUTF(10)=RO2X
      OUTF(11)=RNOX

7118  IF(IVAR.EQ.6) XCOR=HEIGHT
      WRITE(AGNR,7117) XCOR,INT(OUTF(1)+.5),OUTF(2),INT(OUTF(3)+.5),
     &  INT(OUTF(4)+.5),INT(OUTF(5)+.5),OUTF(6),INT(OUTF(7)+.5),
     &  INT(OUTF(8)+.5),INT(OUTF(9)+.5),INT(OUTF(10)+.5),
     &  INT(OUTF(11)+.5)
      IF(STORE) WRITE(IUOUT,7117) XCOR,INT(OUTF(1)+.5),OUTF(2),
     &  INT(OUTF(3)+.5),INT(OUTF(4)+.5),INT(OUTF(5)+.5),OUTF(6),
     &  INT(OUTF(7)+.5),INT(OUTF(8)+.5),INT(OUTF(9)+.5),
     &  INT(OUTF(10)+.5),INT(OUTF(11)+.5)
7117  FORMAT(1X,F7.1,I9,F8.4,3I6,F7.2,5I5)
      IF(SVAR.EQ.0.0) GOTO 2289
      IF(IVAR.EQ.6) THEN
        HEIGHT=HEIGHT+SVAR
        IF(HEIGHT.LE.EVAR) GOTO 300
	HEIGHT=HEIGHT-SVAR
        GOTO 2289
      ENDIF
2135  MONTHO=MONTH
      RGO=RG
      IF(VARIAB+SVAR.LE.EVAR) GOTO 2123
c
c table bottom .................................
c
2289  IF(PIKTAB.gt.0) THEN 
        WRITE(AGNR,6196) MHMF2,MB0,MFOF2
      ELSE
        MTNCOR='----'
	MTENE(1)='--'
	MTENE(2)='--'
	MTENE(3)='--'
	IF((NOTEM).OR.(.NOT.TNOPIV)) GOTO 8158
	    MTNCOR='T(N)'
	    IF(TCON(1)) MTENE(1)='/3'
	    IF(TCON(2)) MTENE(2)='/4'
	    IF(TCON(3)) MTENE(3)='/6'
8158    WRITE(AGNR,6193) MFOF2,MB0,MEF,MIRIM,MTNCOR,MTENE,MION
      ENDIF
      WRITE(AGNR,2193) LATI,LONGI,HEIGHT,R,MONTH,IDAY,HOUR,
     &	     XHI,MLAT,MLONG,DIP,COV,DAYNR,UT,SUNDEC
      IF(.NOT.STORE) GOTO 6199
	IF(PIKTAB.gt.0) THEN 
          WRITE(IUOUT,6196) MHMF2,MB0,MFOF2
        ELSE
          WRITE(IUOUT,6193) MFOF2,MB0,MEF,MIRIM,MTNCOR,
     &		MTENE,MION
        ENDIF
        WRITE(IUOUT,2193) LATI,LONGI,HEIGHT,R,MONTH,IDAY,HOUR,
     &	   XHI,MLAT,MLONG,DIP,COV,DAYNR,UT,SUNDEC
6193  FORMAT(1X,'I-',A5,'--',A5,'-',A3,'-',A5,'-I',11('-'),A4,
     &  3A2,'--I',11('-'),A3,9('-'),'I')
6196  FORMAT(1X,10('-'),A5,'--'A5,19('-'),A5,30('-'))
2193  FORMAT(' LAT/LON=',F5.1,'/',F5.1,'    H=',F6.1,
     &  '  RZ12=',F5.1,' MMDD:',I2,I2,'  LT:',F4.1,'  SZA=',F5.1/
     &  ' MLA/MLO=',F5.1,'/',F5.1,'  DIP=',F5.1,'  F10.7=',F5.1,
     &  '  DDD:',I4,'  UT:',F4.1,'  SDE=',F5.1/1X,74('-'))
6199    WRITE(MONITO,5600)
5600  FORMAT(1X/' **** DO YOU WANT TO CONTINUE?'/1X,60('-')/
     &  ' "0"  QUIT AND EXIT        "1"  NEW PARAMETERS')
	MAXI=1
      IF(IVAR.EQ.6) THEN
	WRITE(MONITO,5607)
5607    FORMAT(1X,'"2"  DIFFERENT ALTITUDE RANGE')
	MAXI=2
	ENDIF
      WRITE(MONITO,5666)
5666  FORMAT(1X,60('-'))
8672  READ(EGNR,*,ERR=8670,END=3330) IALL
      IF((IALL.GE.MIN0).AND.(IALL.LE.MAXI)) GOTO 8671
8670  	WRITE(MONITO,8101) MIN0,MAXI
      	GOTO 8672
8671  IF(IALL.EQ.0) GOTO 3330
c
c different altitude range ............................
c
      IF((IALL.EQ.2).AND.(IVAR.EQ.6)) THEN
        WRITE(MONITO,4618) BVAR,EVAR,SVAR
4618    FORMAT(1X,'BEGIN, END, STEPWIDTH   [KM]',10X,'#',F6.1,','
     &   ,F6.1,',',F6.1,'#')
      	WRITE(MONITO,8640)
	XMAX=PARMAX(6)
	XMIN=PARMIN(6)
8691 	READ(EGNR,*,ERR=8692,END=3330) BVAR,EVAR,SVAR
    	IF((EVAR.LE.XMAX).AND.(BVAR.GE.XMIN)) GOTO 8693
8692  	WRITE(MONITO,8117) XMIN,XMAX
     	GOTO 8691
8693    HEIGHT=BVAR
        WRITE(AGNR,1793) ITEXT(IVAR),IMZ(IVAR)
        IF(STORE) WRITE(IUOUT,1793) ITEXT(IVAR),IMZ(IVAR)
1793    FORMAT(//////////3X,A5,'  ELECTRON DENSITY      TEMPERATU',
     &    'RES',7X,'ION PERCENTAGE DENSITIES'/4X,A4,'  NE/CM-3 NE/N',
     &    'MF2  TN/K  TI/K  TE/K  TE/TI   O+   H+  He+  O2+  NO+')
        GOTO 300
      ENDIF
      GOTO 3293
3330  CONTINUE
8148  	FORMAT(1X,'Your input value is outside the value range:',
     &		F3.1,' to ',E9.1,', try again')
8101  	FORMAT(1X,'Your input value is outside the value range:',
     &		I2,' to ',I2,', try again')
8175  	FORMAT(1X,'Your input value is outside the value range:',
     &		I4,' to ',I4,', try again')
8117  	FORMAT(1X,'Your input value is outside the value range:',
     &		F7.1,' to ',F7.1,', try again')
8640  	FORMAT(1X,60('-')/' Enter /, to continue with current value(s);',
     & 		' Ctrl Z, to exit')
      STOP
      END
