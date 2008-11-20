


C******************************************************************************
C*                                                                            *
C*                 The  Marshall  Space  Flight  Center                       *
C*               Marshall Engineering  Thermosphere Model                     *
C*       --------------------------------------------------------             *
C*                                                                            *
C*                              written by                                    *
C*                                                                            *
C*                              Mike Hickey                                   *
C*                Universities Space Research Association                     *
C*                           NASA / MSFC , ED44                               *
C*                           Tel. (205) 544-5692                              *
C*                                                                            *
C*    This program is a driving program for the following subroutines :-      *
C*                                                                            *
C*                            ATMOSPHERES                                     *
C*                            SOLSET                                          *
C*                            TIME                                            *
C*                            J70                                             *
C*                                                                            *
C*   The atmospheric model is a modified Jacchia 1970 model and is given in   *
C*   the subroutine J70.  All of the other subroutines were designed to       *
C*   allow flexible use of this model so that various input parameters could  *
C*   be varied within a driving program with very little software development.*
C*   Thus, for example, driving routines can be written quite easily to       *
C*   facilitate the plotting of output as line or contour plots.  Control is  *
C*   achieved by setting the values of four switches in the driving program,  *
C*   as described in subroutine ATMOSPHERES.                                  *
C*                                                                            *
C******************************************************************************

	REAL*4  INDATA (12) , OUTDATA (12) , AUXDATA (5)

	CHARACTER*1  SWITCH (4)

	CALL LIB$INIT_TIMER

C  Set all switches to 'Y' so that only one particular calculation is performed

		SWITCH (1) = 'Y'
		SWITCH (2) = 'Y'
		SWITCH (3) = 'Y'
		SWITCH (4) = 'Y'

	CALL ATMOSPHERES ( INDATA, OUTDATA, AUXDATA, SWITCH )

C  Now type output data

	WRITE(1,*)' All output in MKS units'
	WRITE(1,*)' '
	WRITE(1,*)' Exospheric temperature = ', OUTDATA (1),' K'
	WRITE(1,*)' Temperature            = ', OUTDATA (2),' K'
	WRITE(1,*)' N2 number density      = ', OUTDATA (3),' /m3'
	WRITE(1,*)' O2 number density      = ', OUTDATA (4),' /m3'
	WRITE(1,*)' O number density       = ', OUTDATA (5),' /m3'
	WRITE(1,*)' A number density       = ', OUTDATA (6),' /m3'
	WRITE(1,*)' He number density      = ', OUTDATA (7),' /m3'
	WRITE(1,*)' H number density       = ', OUTDATA (8),' /m3'
	WRITE(1,*)' Average molecular wt.  = ', OUTDATA (9)
	WRITE(1,*)' Total mass density     = ', OUTDATA (10),' kg/m3'
	WRITE(1,*)' Log10 mass density     = ', OUTDATA (11)
	WRITE(1,*)' Total pressure         = ', OUTDATA (12),' Pa'
	WRITE(1,*)' Local grav. acceln.    = ', AUXDATA (1),' m.sec-2'
	WRITE(1,*)' Ratio specific heats   = ', AUXDATA (2)
	WRITE(1,*)' Pressure scale-height  = ', AUXDATA (3),' m'
	WRITE(1,*)' Specific heat cons. p  = ', AUXDATA (4),'m2.sec-2.K-1'
	WRITE(1,*)' Specific heat cons. v  = ', AUXDATA (5),'m2.sec-2.K-1'
	WRITE(1,*)' '

	CALL LIB$SHOW_TIMER

	STOP
	END






	SUBROUTINE ATMOSPHERES ( INDATA, OUTDATA, AUXDATA, SWITCH )
	
C******************************************************************************
C*			DESCRIPTION:-                                         *
C*			-----------                                           *
C*                                                                            *
C*  Calculate atmospheric data in single precision using subroutine J70       *
C*  and J70SUP.                                                               *
C*                                                                            *
C*			SUBROUTINES:-                                         *
C*			-----------                                           *
C*                                                                            *
C*       TIME, SOLSET, GMC, J70 and J70SUP                                    *
C*                                                                            *
C*			  INPUT:-                                             *
C*			  ------                                              *
C*  ------------ all single precision, either through     ----------------    *
C*  ------------ subroutines or from main driver prog.    ----------------    *
C*                                                                            *
C*  INDATA (1) -- altitude		= Z                                   *
C*    ..   (2) -- latitude		= XLAT				      *
C*    ..   (3) -- longitude		= XLNG				      *
C*    ..   (4) -- year (yy)		= IYR				      *
C*    ..   (5) -- month (mm)		= MN				      *
C*    ..   (6) -- day (dd)		= IDA				      *
C*    ..   (7) -- hour (hh)		= IHR				      *
C*    ..   (8) -- mins (mm)		= MIN				      *
C*    ..   (9) -- geomagnetic index     = IGEO_IND			      *
C*    ..   (10)-- solar radio noise flux= F10				      *
C*    ..   (11)-- 162-day average F10    = F10B				      *
C*    ..   (12)-- geomagnetic activity index = GI=AP			      *
C*									      *
C*									      *
C*			  OUTPUT:-                                            *
C*			  -------                                             *
C*                                                                            *
C*      NOTE :  All output in MKS units                                       *
C*      --------------------------------                                      *
C*                                                                            *
C*  ---------------------  all single precision    -----------------------    *
C* 									      *
C*                                                                            *
C*  OUTDATA (1) -- exospheric temperature (K)                                 *
C*    ..    (2) -- temperature at altitude Z				      *
C*    ..    (3) -- N2 number density  (per meter-cubed)			      *
C*    ..    (4) -- O2 number density  (      ..       )			      *
C*    ..    (5) -- O  number density  (      ..       )			      *
C*    ..    (6) -- A  number density  (      ..       )			      *
C*    ..    (7) -- He number density  (      ..       )                       *
C*    ..    (8) -- H  number density  (      ..       )			      *
C*    ..    (9) -- average molecular weight                                   *
C*    ..    (10)-- total density                                              *
C*    ..    (11)-- log10 ( total density )                                    *
C*    ..    (12)-- total pressure ( Pa )                                      *
C*									      *
C*  AUXDATA (1) -- gravitational acceleration ( m/s-s )                       *
C*    ..    (2) -- ratio of specific heats                                    *
C*    ..    (3) -- pressure scale-height ( m )                                *
C*    ..    (4) -- specific heat at constant pressure                         *
C*    ..    (5) -- specific heat at constant volume                           *
C*                                                                            *
C*                                                                            *
C*			   COMMENTS:-					      *
C*			   ---------					      *
C*                                                                            *
C*  SWITCH(1) -- if Y(es), date and time are input from terminal through      *
C*		  	   subroutine TIME once only			      *
C*  SWITCH(2) -- if Y(es), solar/magnetic activity are input from terminal    *
C*			   through subroutine SOLSET once only		      *
C*  SWITCH(3) -- if Y(es), only ONE altitude value is input from terminal     *
C*			   through main calling program			      *
C*  SWITCH(4) -- if Y(es), only ONE latitude AND longitude are input from     *
C*  			   terminal through main calling program              *
C*									      *
C*   ATMOSPHERES written by Mike Hickey ( USRA, NASA/ED44 )                   *
C*                          Tel: (205) 544-5692                               *
C*      --------  January-April 1987  ---------                               *
C******************************************************************************

	
	EXTERNAL TIME
	
	DIMENSION  AUXDATA (5)
	
	INTEGER  HR

	REAL*4  LAT, LON, INDATA (12), OUTDATA (12)
	
	CHARACTER*1  SWITCH (4)
	
	PARAMETER PI = 3.14159265
	
C -----------------------------------------------------------------------------
C	This next section is only executed on the first call to ATMOSPHERES
	
		DO WHILE ( CALL. EQ. 0.0 )
	
C  SECTION A:-
C  ----------

	IF ( SWITCH(1). EQ. 'Y' ) THEN
	
	CALL TIME ( IYR, MON, IDA, HR, MIN, SWITCH(1) )
	INDATA (4) = FLOATJ (IYR)
	INDATA (5) = FLOATJ (MON)
	INDATA (6) = FLOATJ (IDA)
	INDATA (7) = FLOATJ (HR)
	INDATA (8) = FLOATJ (MIN)
	
	END IF
	
C  SECTION B:-
C  ----------
	
	IF ( SWITCH(2). EQ. 'Y' ) THEN
	
	CALL SOLSET ( IGEO_IND, F10, F10B, GI, SWITCH(2) )
	INDATA (9) = FLOATJ (IGEO_IND)
	INDATA (10) = F10
	INDATA (11) = F10B
	INDATA (12) = GI
	
	END IF
	
C  SECTION C:-
C  ----------
	
	IF ( SWITCH(3). EQ. 'Y' ) THEN
	
	TYPE *,' Input altitude, km'
	ACCEPT *, INDATA (1)
	Z = INDATA (1)
	
	END IF
	
C  SECTION D:-
C  ----------
	
	IF ( SWITCH(4). EQ. 'Y' ) THEN
	
	TYPE *,' Input latitude and longitude, degrees'
	ACCEPT *, ( INDATA(I), I= 2,3 )
	LAT = INDATA (2)
	LON = INDATA (3)
	RLT = INDATA (2) * PI / 180.	!  geographic latitude, radians
	
	END IF
	
	
	CALL = 1.0
		END DO
	
C	End of first executable section
C -----------------------------------------------------------------------------
	
C  The following depend on the values of the switches

C****
C* SECTION 1:-

		IF ( SWITCH(1). NE. 'Y' ) THEN
	
	IYR = JINT ( INDATA (4) )
	MON = JINT ( INDATA (5) )
	IDA = JINT ( INDATA (6) )
	HR  = JINT ( INDATA (7) )
	MIN = JINT ( INDATA (8) )
	CALL TIME ( IYR, MON, IDA, HR, MIN, SWITCH(1) )
	
		END IF
	

C*****
C* SECTION 2:-	

		IF ( SWITCH(2). NE. 'Y' ) THEN
	
	IGEO_IND = JINT ( INDATA (9) )
	F10      = INDATA (10)
	F10B     = INDATA (11)
	GI       = INDATA (12)
	CALL SOLSET ( IGEO_IND, F10, F10B, GI, SWITCH(2) )
	
		END IF
	
C*****
C* SECTION 3:-	


		IF ( SWITCH(3). NE. 'Y' ) THEN
	
	Z = INDATA (1)
	
		END IF


C*****
C* SECTION 4:-	
	
		IF ( SWITCH(4). NE. 'Y' ) THEN
	
	LAT = INDATA (2)
	LON = INDATA (3)
	RLT = INDATA (2) * PI / 180.	!  geographic latitude, radians
	
		END IF
	
	
C  All setting-up complete.
	
		CALL J70 ( INDATA, OUTDATA )
		CALL J70SUP ( Z, OUTDATA, AUXDATA )

	
	RETURN

	ENTRY ATMOS_ENT ( DUMMY )
	CALL = DUMMY
	RETURN

	END	






	SUBROUTINE TIME ( IYR, MON, IDA, HR, MIN, SWITCH )
	
C******************************************************************************
C*                                                                            *
C*          This subroutine sets up time of year and day                      *
C*                                                                            *
C*                    INPUTS/OUTPUTS:                                         *
C*                   ----------------                                         *
C*  IYR  =  year ( 2 digits )                                                 *
C*  MON  =  month                                                             *
C*  IDA  =  day of month                                                      *
C*  HR   =  hour of day                                                       *
C*  MIN  =  minutes                                                           *
C*                                                                            *
C*        Written by Mike Hickey, USRA                                        *
C******************************************************************************
	
	DIMENSION  IDAY ( 12 )

	INTEGER  HR

	CHARACTER*1  SWITCH

	DATA IDAY / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
	
	PARAMETER PI = 3.14159265

C------------------------------------------------------------------------------
C ------     If  SWITCH = Y(es)  then input data and time from terminal -------
C______________________________________________________________________________

		IF ( SWITCH.EQ.'Y'. OR. SWITCH.EQ.'y' )  THEN
	
	TYPE *, ' Input date and time of date? ( yy,mm,dd,hh,mm ) '
	ACCEPT *, IYR, MON, IDA, HR, MIN

		END IF

C _____________________________________________________________________________
	
	
	IF (  JMOD (IYR,4) .EQ. 0 )  THEN
		IF ( JMOD (IYR,100) .NE. 0 ) IDAY ( 2 ) = 29
	ELSE
		IDAY ( 2 ) = 28
	END IF

	DAYTOT = 0.0
	
		DO  1  I = 1, 12
	DAYTOT = DAYTOT + FLOATJ ( IDAY ( I )  )
1		CONTINUE
	
		IF ( MON. GT. 1 )		THEN
	
	KE = MON - 1
	ID = 0
		DO 2  I = 1, KE
	ID = ID + IDAY (I)
2		CONTINUE
	
	ID = ID + IDA
						ELSE
	DD = IDA
		END IF
	
	
	RETURN
	END





	
	SUBROUTINE SOLSET ( IGEO_IND, F10, F10B, GI, SWITCH )
	
C******************************************************************************
C*                                                                            *
C* This subroutine simply calls for a setup of the solar-activty and auroral  *
C* activity indices.                                                          *
C*                                                                            *
C*                     INPUTS/OUTPUTS:                                        *
C*                    ----------------                                        *
C*  IGEO_IND = geomagnetic index                                              *
C*  F10      = solar radio noise flux                                         *
C*  F10B     = 162-day average F10                                            *
C*  GI       = geomagnetic activity index                                     *
C*                                                                            *
C*      Written by Mike Hickey, USRA                                          *
C******************************************************************************

	CHARACTER*1  SWITCH

	IGEO_IND = 2

C -----------------------------------------------------------------------------
C -----  If SWITCH = Y(es) then input geomagnetic indices from terminal -------
C______________________________________________________________________________

		IF ( SWITCH.EQ.'Y'. OR. SWITCH.EQ.'y' ) THEN
	
C	TYPE *, ' Input geomagnetic index ( 1-KP, 2-AP ) '
C	ACCEPT *, IGEO_IND
	
	TYPE *, ' Input solar radio noise flux ( F10 = 0-400 ) '
	ACCEPT *, F10
	
	TYPE *, ' Input 162-day average F10 ( F10B = 0-250 ) '
	ACCEPT *, F10B
	
C		IF ( IGEO_IND . EQ. 2 )		THEN
C	
C	TYPE *, ' Input geomagnetic activity index ( GI = 0-400 ) '
C	
C						ELSE
C	
C	TYPE *, ' Input geomagnetic activity index ( GI = 0-9 ) '
C	
C		END IF

	TYPE *,' Input AP index ( AP = 0 - 400 ) '
	
	ACCEPT *, GI

		END IF

C _____________________________________________________________________________

	
	RETURN
	END






	SUBROUTINE J70SUP ( Z, OUTDATA, AUXDATA )
	
C******************************************************************************
C*                                                                            *
C*                          DESCRIPTION:-                                     *
C*                          -----------                                       *
C*                                                                            *
C*  J70SUP calculates auxilliary variables which are output in array          *
C*  AUXDATA, given data input from J70 which are contained in array OUTDATA.  *
C*                                                                            *
C*                         INPUT DATA:-                                       *
C*                         ----------                                         *
C*                                                                            *
C*   Z  --  altitude (km)                                                     *
C*  TZZ --  temperature at altitude z   =  OUTDATA (2)                        *
C*      --  N2 number density           =     ..   (3)                        *
C*      --  O2  ..      ..              =     ..   (4)                        *
C*      --  O   ..      ..              =     ..   (5)                        *
C*      --  A   ..      ..              =     ..   (6)                        *
C*      --  He  ..      ..              =     ..   (7)                        *
C*      --  H   ..      ..              =     ..   (8)                        *
C*  EM  --  average molecular weight    =     ..   (9)                        *
C* DENS --  total density               =     ..   (10)                       *
C*  P   --  total pressure              =     ..   (12)                       *
C*                                                                            *
C*                          OUTPUT DATA:-                                     *
C*                          -----------                                       *
C*                                                                            *
C*  G  --  gravitational acceleration          =  AUXDATA (1)                 *
C* GAM --  ratio of specific heats             =  AUXDATA (2)                 *
C*  H  --  pressure scale-height               =  AUXDATA (3)                 *
C*  CP --  specific heat at constant pressure  =  AUXDATA (4)                 *
C*  CV --  specific heat at constant volume    =  AUXDATA (5)                 *
C*                                                                            *
C*  Written by Mike Hickey, USRA                                              *
C******************************************************************************
	
	REAL*4  OUTDATA (12), AUXDATA (5), H
	
	G = 9.80665 / ( ( 1. + Z / 6.356766E3 )**2 )
	
	H = OUTDATA (12) / ( G * OUTDATA (10) )
	
	SUM1 = OUTDATA (3) + OUTDATA (4)
	SUM2 = 0.0
		DO 1  I = 5, 8
	SUM2 = SUM2 + OUTDATA (I)
1		CONTINUE
	
	GAM = ( 1.4 * SUM1 + 1.67 * SUM2 ) / ( SUM1 + SUM2 )
	
	CV = G * H / (   ( GAM - 1.0 ) * OUTDATA (2)   )
	
	CP = GAM * CV
	
	AUXDATA (1) = G
	AUXDATA (2) = GAM
	AUXDATA (3) = H
	AUXDATA (4) = CP
	AUXDATA (5) = CV
	
	RETURN
	END





                    SUBROUTINE J70 ( INDATA, OUTDATA )

C******************************************************************************
C**                                                                          **
C**                    J70 developed from J70MM by                           **
C**                         Mike P. Hickey                                   **
C**              Universities Space Research Association                     **
C**                                at                                        **
C**              NASA / Marshall Space Flight Center, ED44,                  **
C**                   Huntsville, Alabama, 35812, USA.                       **
C**                       Tel. (205) 544-5692                                **
C**                                                                          **
C** INPUTS:        through the subroutine calling list                       **
C**                                                                          **
C** OUTPUTS:       through the subroutine calling list                       **
C**                                                                          **
C**                                                                          **
C**                        INPUT DATA:                                       **
C**                       ------------                                       **
C**          Z    --  altitude                 = INDATA (1)                  **
C**          XLAT -- latitude                  = INDATA (2)                  **
C**          XLNG -- longitude                 = INDATA (3)                  **
C**          IYR -- year (yy)                  = INDATA (4)                  **
C**          MN -- month (mm)                  = INDATA (5)                  **
C**          IDA -- day  (dd)                  = INDATA (6)                  **
C**          IHR -- hour (hh)                  = INDATA (7)                  **
C**          MIN -- mins (mm)                  = INDATA (8)                  **
C**          I1 -- geomagnetic index           = INDATA (9)                  **
C**          F10  --  solar radio noise flux   = INDATA (10)                 **
C**          F10B --  162-day average F10       = INDATA (11)                **
C**          GI --  geomagnetic activity index = INDATA (12)                 **
C**                                                                          **
C**                                                                          **
C**                        OUTPUT DATA:                                      **
C**                       -------------                                      **
C**          T-- exospheric temperature      = OUTDATA (1)                   **
C**          TZZ-- temperature at altitude Z = OUTDATA (2)                   **
C**          A(1)-- N2 number density        = OUTDATA (3)                   **
C**          A(2)-- O2 number density        = OUTDATA (4)                   **
C**          A(3)-- O  number density        = OUTDATA (5)                   **
C**          A(4)-- A  number density        = OUTDATA (6)                   **
C**          A(5)-- He number density        = OUTDATA (7)                   **
C**          A(6)-- H  number density        = OUTDATA (8)                   **
C**          EM-- average molecular weight   = OUTDATA (9)                   **
C**          DENS-- total density            = OUTDATA (10)                  **
C**          DL-- log10 ( total density )    = OUTDATA (11)                  **
C**          P-- total pressure              = OUTDATA (12)                  **
C**                                                                          **
C**    NB.  Input through array 'INDATA'                                     **
C**         Output through array 'OUTDATA'                                   **
C******************************************************************************

        DIMENSION  A ( 6 )

	REAL*4  INDATA ( 12 ), OUTDATA ( 12 )
	PARAMETER RGAS = 8.31432E3 		!  J/kmol-K
	PARAMETER BFH = 440.0

C Calcultions performed for only one latitude , one longitude
C and one altitude

C
C** Set parameters to INDATA values
C
	Z    = INDATA (1)
	XLAT = INDATA (2)
	XLNG = INDATA (3)
	IYR  = JINT ( INDATA (4) )  + 1900
	MN   = JINT ( INDATA (5) )
	IDA  = JINT ( INDATA (6) )
	IHR  = JINT ( INDATA (7) )
	MIN  = JINT ( INDATA (8) )
	I1   = JINT ( INDATA (9) )
	F10  = INDATA (10)
	F10B = INDATA (11)
	GI   = INDATA (12)


	CALL TME ( MN , IDA , IYR , IHR , MIN , XLAT , XLNG , SDA ,
     .		SHA , DD , DY )

	CALL TINF ( F10 , F10B , GI , XLAT , SDA , SHA , DY , I1 , TE )

	CALL JAC ( Z , TE , TZ , A(1) , A(2) , A(3) , A(4) , A(5) , A(6) ,
     .		EM , DENS , DL )


		DENLG = 0.
		DUMMY = DL
		DEN   = DL

	IF ( Z .LE. 170. )  THEN
		CALL SLV ( DUMMY , Z , XLAT , DD )
		DENLG = DUMMY
	END IF

C
C** 'Fair' helium number density between base fairing height ( BFH ) and 500 km
C

	IF ( Z. GE. 500. )  THEN
			CALL SLVH ( DEN , A(5) , XLAT , SDA )
			DL = DEN
	ELSE IF ( Z .GT. BFH )  THEN
			DHEL1 = A ( 5 )
			DHEL2 = A ( 5 )
			DLG1 = DL
			DLG2 = DL
		CALL SLVH ( DLG2 , DHEL2 , XLAT , SDA )
			IH = Z
		CALL FAIR5 ( DHEL1 , DHEL2 , DLG1 , DLG2 , IH , FDHEL , FDLG )
			DL = FDLG
			A ( 5 ) = FDHEL
	END IF

			DL = DL + DENLG
			DENS = 10.**DL
			XLAT = XLAT * 57.29577951


C  Fill OUTDATA array
	OUTDATA (1) = TE
	OUTDATA (2) = TZ

		DO 80 I = 1, 6
	OUTDATA (I+2) = 1.E6 * ( 10. ** A(I) )
80		CONTINUE

	OUTDATA (9) = EM
	OUTDATA (10) = DENS * 1000.
	OUTDATA (11) = DL
	P = OUTDATA (10) * RGAS * TZ / EM
	OUTDATA (12) = P

	RETURN
	END






      SUBROUTINE TME ( MN , IDA , IYR , IHR , MIN , XLAT , XLNG ,
     .			SDA , SHA , DD , DY )

C******************************************************************************
C** Subroutine 'TME' performs the calculations of the solar declination      **
C** angle and solar hour angle.                                              **
C**                                                                          **
C** INPUTS:  MN  = month                                                     **
C**          IDA = day                                                       **
C**          IYR = year                                                      **
C**          IHR = hour                                                      **
C**          MIN = minute                                                    **
C**          XMJD= mean Julian date                                          **
C**          XLAT= latitude ( input-geocentric latitude )                    **
C**          XLNG= longitude ( input-geocentric longitude, -180,+180 )       **
C**                                                                          **
C** OUTPUTS: SDA = solar declination angle (rad)                             **
C**          SHA = solar hour angle (rad)                                    **
C**          DD  = day number from 1 JAN.                                    **
C**          DY  = DD / tropical year                                        **
C**      Modified by Mike Hickey, USRA                                       **
C******************************************************************************

	DIMENSION IDAY(12)

	DATA IDAY / 31,28,31 ,30,31,30 ,31,31,30 ,31,30,31 /
	PARAMETER YEAR = 365.2422 
	PARAMETER A1 = 99.6909833 , A2 = 36000.76892
	PARAMETER A3 = 0.00038708 , A4 = 0.250684477 
	PARAMETER B1 = 0.0172028  , B2 = 0.0335     , B3 = 1.407
	PARAMETER PI = 3.14159265 , TPI = 6.28318531 
	PARAMETER PI2 = 1.57079633 , PI32 = 4.71238898 
	PARAMETER RAD_DEG = 0.017453293

	XLAT = XLAT / 57.29577951
	YR = IYR

	IF ( JMOD(IYR,4) .EQ. 0 ) THEN
	   IF ( JMOD(IYR,100) .NE. 0 ) IDAY(2) = 29  ! Century not a leap year
	ELSE
	   IDAY(2) = 28
	END IF
		ID = 0
	IF ( MN. GT. 1 ) THEN
	   DO  20  I = 1 , MN-1
		ID = ID + IDAY(I)
20	   CONTINUE
	END IF
		ID = ID + IDA
		DD = ID
		DY = DD/YEAR
C
C** Compute mean Julian date 
C
	XMJD = 2415020. + 365. * ( YR - 1900. ) + DD
     .	                + FLOATJ (  ( IYR - 1901 ) / 4 )
C
C** Compute Greenwich mean time in minutes GMT
C
	XHR = IHR
	XMIN = MIN
	GMT = 60 * XHR + XMIN
	FMJD = XMJD - 2435839. + GMT / 1440.
C
C** Compute Greenwich mean position - GP ( in rad )
C
	XJ = ( XMJD - 2415020.5 ) / ( 36525.0 )
	GP = AMOD ( A1 + A2 * XJ + A3 * XJ * XJ + A4 * GMT , 360. )

C
C** Compute right ascension point - RAP ( in rad )
C
C** 1st convert geocentric longitude to deg longitude -  west neg , + east
C
	IF ( XLNG .GT. 180. ) XLNG = XLNG - 360.

	RAP = AMOD ( GP + XLNG , 360. )

C
C** Compute celestial longitude - XLS ( in rad ) - - zero to 2PI
C
	Y1 = B1 * FMJD
	Y2 = 0.017202 * ( FMJD - 3. )
	XLS = AMOD ( Y1 + B2 * SIN(Y2) - B3 , TPI )
C
C** Compute solar declination angle - SDA ( in rad )
C
	B4 = RAD_DEG * ( 23.4523 - 0.013 * XJ )
	SDA = ASIN ( SIN ( XLS ) * SIN ( B4 ) )
C
C** Compute right ascension of Sun - RAS ( in rad ) - - zero to 2PI
C
C** These next few lines do not appear in NASA CR-179359 or NASA CR-??????
C** They are added here to ensure that that argument of ASIN stays bounded
C** between -1 and +1 , which could otherwise be effected by roundoff error.
	ARG = TAN ( SDA ) / TAN ( B4 )
		IF ( ARG .GT. 1.0 ) ARG = 1.0
		IF ( ARG .LT. -1. ) ARG = -1.0
	RAS = ASIN ( ARG )
C
C** Put RAS in same quadrant as XLS
C
	RAS = ABS ( RAS )
	TEMP = ABS ( XLS )

	IF ( TEMP.LE.PI .AND. TEMP.GT.PI2 )  THEN
		RAS = PI - RAS
	ELSE IF ( TEMP.LE.PI32 .AND. TEMP.GT.PI )  THEN
		RAS = PI + RAS
	ELSE IF ( TEMP.GT.PI32 )  THEN
		RAS = TPI - RAS
	END IF
		IF ( XLS. LT. 0. ) RAS = -RAS

C
C** Compute solar hour angle - SHA ( in deg ) - -
C
	SHA = RAP * RAD_DEG - RAS
	IF ( SHA.GT.PI ) SHA = SHA - TPI
	IF ( SHA.LT.-PI ) SHA = SHA + TPI


	RETURN
	END






	SUBROUTINE TINF ( F10 , F10B , GI, XLAT, SDA , SHA , DY , I1 , TE )

C******************************************************************************
C** Subroutine 'TINF' calculates the exospheric temperature according to     **
C** L. Jacchia SAO 313, 1970                                                 **
C**                                                                          **
C**   F10 = solar radio noise flux ( x E-22 Watts / m2 )                     **
C**   F10B= 162-day average F10                                              **
C**   GI  = geomagnetic activity index                                       **
C**   LAT = geographic latitude at perigee ( in rad )                        **
C**   SDA = solar declination angle ( in rad )                               **
C**   SHA = solar hour angle                                                 **
C**   DY  = D / Y  ( day number / tropical year ) ; 1                        **
C**   I1  = geomagnetic equation index  ( 1--GI=KP , 2--GI=AP )              **
C**   RE  = diurnal factor  KP, F10B, AVG                                    **
C**                                                                          **
C**      CONSTANTS -- C = solar activity variation                           **
C**                -- BETA , etc = diurnal variation                         **
C**                -- D = geomagnetic variation                              **
C**                -- E = semiannual variation                               **
C**                                                                          **
C**  Modified by Mike Hickey, USRA                                           **
C******************************************************************************

	PARAMETER PI = 3.14159265 , TPI = 6.28318531
        PARAMETER XM = 2.5 , XNN = 3.0
C
C**  Ci are solar activity variation variables
C
        PARAMETER C1 = 383.0 , C2 = 3.32 , C3 = 1.80
C
C**  Di are geomagnetic variation variables
C
	PARAMETER D1 = 28.0 , D2 = 0.03 , D3 = 1.0 , D4 = 100.0 , D5 = -0.08
C
C**  Ei are semiannual variation variables
C
        PARAMETER E1 = 2.41 , E2 = 0.349 , E3 = 0.206 , E4 = 6.2831853 
	PARAMETER E5 = 3.9531708 , E6 = 12.5663706 , E7 = 4.3214352 
        PARAMETER E8 = 0.1145 , E9 = 0.5 , E10 = 6.2831853
	PARAMETER E11 = 5.9742620 , E12 = 2.16

	PARAMETER BETA = -0.6457718 , GAMMA = 0.7504916 , P = 0.1047198
	PARAMETER RE = 0.31
C
C** solar activity variation
C
	TC = C1 + C2 * F10B + C3 * ( F10 - F10B )
C
C** diurnal variation
C
	ETA    = 0.5 * ABS ( XLAT - SDA )
	THETA  = 0.5 * ABS ( XLAT + SDA )
	TAU    = SHA + BETA + P * SIN ( SHA + GAMMA )

	IF ( TAU. GT. PI ) TAU = TAU - TPI
	IF ( TAU. LT.-PI ) TAU = TAU + TPI

	A1 = ( SIN ( THETA ) )**XM
	A2 = ( COS ( ETA ) )**XM
	A3 = ( COS ( TAU / 2. ) )**XNN
	B1 = 1.0 + RE * A1
	B2 = ( A2 - A1 ) / B1
	TV = B1 * ( 1. + RE * B2 * A3 )
	TL = TC * TV
C
C** geomagnetic variation
C
      	IF ( I1.EQ.1 ) THEN
   		TG = D1 * GI + D2 * EXP(GI)
	ELSE
   		TG = D3 * GI + D4 * ( 1 - EXP ( D5 * GI ) )
	END IF
C
C** semiannual variation
C
	G3 = 0.5 * ( 1.0 + SIN ( E10 * DY + E11 ) )
	G3 = G3 ** E12
	TAU1 = DY + E8 * ( G3 - E9 )
	G1 = E2 + E3 * ( SIN ( E4 * TAU1 + E5 ) )
	G2 = SIN ( E6 * TAU1+ E7 )
	TS = E1 + F10B * G1 * G2
C
C** exospheric temperature
C
	TE = TL + TG + TS

	RETURN
	END







	SUBROUTINE JAC ( Z , T , TZ , AN , AO2 , AO , AA , AHE , AH , EM ,
     .			 DENS , DL )

C******************************************************************************
C**                                                                          **
C**  Subroutine 'JAC' calculates the temperature TZ , the total density DENS **
C**  and its logarithm DL, the mean molecular weight EM, the individual      **
C**  specie number densities for N, O2, O, A, HE and H ( each preceded with  **
C**  an 'A' ) at altitude Z given the exospheric temperature T.              **
C**  This subroutine uses the subroutine 'GAUSS' and the function            **
C**  subprograms 'TEMP' and 'MOL_WT'.                                        **
C**                                                                          **
C**   Rewritten by Mike Hickey, USRA                                         **
C******************************************************************************

	DIMENSION  ALPHA(6) , EI(6) , DI(6) , DIT(6)
	REAL*4  MOL_WT

	PARAMETER AV = 6.02257E23
	PARAMETER QN   = .78110
	PARAMETER QO2  = .20955
	PARAMETER QA   = .009343
	PARAMETER QHE  = 1.289E-05
	PARAMETER RGAS = 8.31432
	PARAMETER PI   = 3.14159265
	PARAMETER T0   = 183.

	GRAVITY ( ALTITUDE ) = 9.80665 / ( ( 1. + ALTITUDE / 6.356766E3 )**2 )



	DATA ALPHA / 0.0 , 0.0 , 0.0 , 0.0 , -.380 , 0.0 /
	DATA EI / 28.0134 , 31.9988 , 15.9994 , 39.948 , 4.0026 , 1.00797 /



	TX = 444.3807 + .02385 * T - 392.8292 * EXP ( -.0021357 * T )
	A2 = 2. * (T-TX) / PI
	TX_T0 = TX - T0
	T1 =  1.9 * TX_T0 /   35.
	T3 = -1.7 * TX_T0 / ( 35.**3 )
	T4 = -0.8 * TX_T0 / ( 35.**4 )
	TZ = TEMP ( Z , TX , T1 , T3 , T4 , A2 )


C**	SECTION 1
C**     ---------

	A = 90.
	D = AMIN1 ( Z , 105. )

C  Integrate  gM/T  from  90  to  minimum of Z or 105 km :-

	CALL GAUSS ( A, D, 1, R, TX , T1 , T3 , T4 , A2 )

C  The number 2.1926E-8 = density x temperature/mean molecular weight at 90 km.

	EM = MOL_WT ( D )
	TD = TEMP ( D , TX , T1 , T3 , T4 , A2 )

	DENS = 2.1926E-8 * EM * EXP( -R / RGAS ) / TD

	FACTOR = AV * DENS
	PAR = FACTOR / EM
	FACTOR = FACTOR / 28.96


C  For altitudes below and at 105 km calculate the individual specie number
C  densities from the mean molecular weight and total density.

	IF ( Z. LE. 105 )  THEN

	DL = ALOG10 ( DENS )
	AN  = ALOG10 ( QN * FACTOR )
	AA  = ALOG10 ( QA * FACTOR )
	AHE = ALOG10 ( QHE * FACTOR )
	AO  = ALOG10 ( 2. * PAR * ( 1.-EM / 28.96 ) )
	AO2 = ALOG10 ( PAR * ( EM * ( 1.+QO2 ) / 28.96-1. ) )
	AH  = 0.
C
C** Return to calling program
C
	RETURN


	END IF



C**	SECTION 2  :  This section is only performed for altitudes above 105 km
C**     ---------

C  Note that having reached this section means that D in section 1 is 105 km.
C  ----

C  Calculate individual specie number densities from the total density and mean
C  molecular weight at 105 km altitude.

		DI(1) = QN * FACTOR
		DI(2) = PAR * (EM * (1.+QO2) / 28.96-1.)
		DI(3) = 2. * PAR * (1.- EM / 28.96)
		DI(4) = QA * FACTOR
		DI(5) = QHE * FACTOR

C  Integrate  g/T  from  105 km  to  Z km :-

	CALL GAUSS ( D, Z, 2, R, TX , T1 , T3 , T4 , A2 )

		DO  41  I = 1 , 5
	DIT(I) = DI(I) * ( TD / TZ ) **(1.+ALPHA(I)) * EXP( -EI(I) * R / RGAS)
		IF ( DIT(I). LE. 0. ) DIT(I) = 1.E-6
41 		CONTINUE




C**  This section calculates atomic hydrogen densities above 500 km altitude.
C**  Below this altitude , H densities are set to 10**-6.

C**	SECTION 3
C**     ---------

	IF ( Z .GT. 500. )  THEN

		A1 = 500.
		S = TEMP ( A1 , TX , T1 , T3 , T4 , A2 )

	DI(6) = 10.** ( 73.13 - 39.4 * ALOG10 (S) + 5.5 * ALOG10(S) *ALOG10(S))
  
	CALL GAUSS ( A1, Z, 7, R, TX , T1 , T3 , T4 , A2 )

		DIT(6) = DI(6) * (S/TZ) * EXP ( -EI(6) * R / RGAS )

	ELSE

		DIT (6) = 1.0

	END IF



C   For altitudes greater than 105 km , calculate total density and mean
C   molecular weight from individual specie number densities.

			DENS=0
		DO  42  I = 1 , 6
			DENS = DENS + EI(I) * DIT(I) / AV
42    		CONTINUE

		EM = DENS * AV / ( DIT(1)+DIT(2)+DIT(3)+DIT(4)+DIT(5)+DIT(6) )
		DL = ALOG10 (DENS)

		AN  = ALOG10(DIT(1))
		AO2 = ALOG10(DIT(2))
		AO  = ALOG10(DIT(3))
		AA  = ALOG10(DIT(4))
		AHE = ALOG10(DIT(5))
		AH  = ALOG10(DIT(6))


		RETURN
		END






	FUNCTION  TEMP ( ALT , TX , T1 , T3 , T4 , A2 )

C******************************************************************************
C**                                                                          **
C**  Function subprogram 'TEMP' calculates the temperature at altitude ALT   **
C**  using equation (10) for altitudes between 90 and 125 km and equation    **
C**  (13) for altitudes greater than 125 km , from SAO Report 313.           **
C**                                                                          **
C**  Written by Mike Hickey, USRA                                            **
C******************************************************************************

	PARAMETER BB = 4.5E-6

		U = ALT - 125.
	IF ( U .GT . 0. )  THEN
		TEMP = TX  +  A2 * ATAN ( T1 * U * ( 1. + BB * (U**2.5)) / A2 )
	ELSE
		TEMP = TX  +  T1 * U  +  T3 * (U**3)  +  T4 * (U**4)
	END IF

	END





	REAL FUNCTION MOL_WT*4 ( A )

C******************************************************************************
C**                                                                          **
C**  Subroutine 'MOL_WT' calculates the molecular weight for altitudes       **
C**  between 90 and 105 km according to equation (1) of SAO report 313.      **
C**  Otherwise, MOL_WT is set to unity.                                      **
C**                                                                          **
C**  Written by Mike Hickey, USRA                                            **
C******************************************************************************

	DIMENSION  B (7)

	DATA B / 28.15204 , -0.085586, 1.284E-4, -1.0056E-5, -1.021E-5,
     .		1.5044E-6, 9.9826E-8 /

	IF ( A. GT. 105. ) THEN

		MOL_WT = 1.

	ELSE

		U = A - 100.
		MOL_WT = B (1)
	DO  1  I = 2 , 7

	MOL_WT = MOL_WT  +  B (I) * U ** ( I-1 )

1	CONTINUE

	END IF

	END






	SUBROUTINE GAUSS ( Z1 , Z2 , NMIN , R , TX , T1 , T3 , T4 , A2 )

C******************************************************************************
C**  Subdivide total integration-altitude range into intervals suitable for  **
C**  applying Gaussian Quadrature , set the number of points for integration **
C**  for each sub-interval , and then perform Gaussian Quadrature.           **
C**       Written by Mike Hickey, USRA, NASA/MSFC, ED44, July 1988.          **
C******************************************************************************

	REAL*4   ALTMIN (9) , C(8,6), X(8,6), MOL_WT
	INTEGER  NG (8) , NGAUSS , NMIN , J

	GRAVITY ( ALTITUDE ) = 9.80665 / ( ( 1. + ALTITUDE / 6.356766E3 )**2 )

	DATA  ALTMIN / 90., 105., 125., 160., 200., 300., 500., 1500., 2500. /
	DATA    NG   /  4 , 5  ,  6  ,  6  ,  6  ,  6  ,  6  , 6 /

C  Coefficients for Gaussian Quadrature ...

	DATA  C / .5555556 , .8888889 , .5555556 , .0000000 , ! n=3
     .            .0000000 , .0000000 , .0000000 , .0000000 , ! n=3
     .            .3478548 , .6521452 , .6521452 , .3478548 , ! n=4
     .            .0000000 , .0000000 , .0000000 , .0000000 , ! n=4
     .            .2369269 , .4786287 , .5688889 , .4786287 , ! n=5
     .            .2369269 , .0000000 , .0000000 , .0000000 , ! n=5
     .            .1713245 , .3607616 , .4679139 , .4679139 , ! n=6
     .            .3607616 , .1713245 , .0000000 , .0000000 , ! n=6
     .            .1294850 , .2797054 , .3818301 , .4179592 , ! n=7
     .            .3818301 , .2797054 , .1294850 , .0000000 , ! n=7
     .            .1012285 , .2223810 , .3137067 , .3626838 , ! n=8
     .            .3626838 , .3137067 , .2223810 , .1012285 / ! n=8

C  Abscissas for Gaussian Quadrature ...

	DATA  X / -.7745967 ,  .0000000 ,  .7745967 ,  .0000000 , ! n=3
     .             .0000000 ,  .0000000 ,  .0000000 ,  .0000000 , ! n=3
     .            -.8611363 , -.3399810 ,  .3399810 ,  .8611363 , ! n=4
     .             .0000000 ,  .0000000 ,  .0000000 ,  .0000000 , ! n=4
     .            -.9061798 , -.5384693 ,  .0000000 ,  .5384693 , ! n=5
     .             .9061798 ,  .0000000 ,  .0000000 ,  .0000000 , ! n=5
     .            -.9324695 , -.6612094 , -.2386192 ,  .2386192 , ! n=6
     .             .6612094 ,  .9324695 ,  .0000000 ,  .0000000 , ! n=6
     .            -.9491079 , -.7415312 , -.4058452 ,  .0000000 , ! n=7
     .             .4058452 ,  .7415312 ,  .9491079 ,  .0000000 , ! n=7
     .            -.9602899 , -.7966665 , -.5255324 , -.1834346 , ! n=8
     .             .1834346 ,  .5255324 ,  .7966665 ,  .9602899 / ! n=8

		R  =  0.0

	DO   2   K = NMIN , 8

		NGAUSS = NG (K)
                A      = ALTMIN (K)
                D      = AMIN1 ( Z2 , ALTMIN (K+1) )
                RR   = 0.0
                DEL = 0.5 * ( D - A )
                J   = NGAUSS - 2

	DO   1   I = 1 , NGAUSS

		Z = DEL * ( X(I,J) + 1. ) + A
	RR = RR + C(I,J) * MOL_WT(Z) * GRAVITY(Z) / TEMP ( Z,TX,T1,T3,T4,A2 )

1	CONTINUE

                RR = DEL * RR
                R  = R + RR
                IF ( D .EQ. Z2 )  RETURN

2	CONTINUE

	RETURN
	END





      SUBROUTINE SLV ( DEN , ALT , XLAT , DAY )

C******************************************************************************
C** Subroutine 'SLV' computes the seasonal-latitudinal variation of density  **
C** in the lower thermosphere in accordance with L. Jacchia, SAO 332, 1971.  **
C** This affects the densities between 90 and 170 km.  This subroutine need  **
C** not be called for densities above 170 km, because no effect is observed. **
C**                                                                          **
C** The variation should be computed after the calculation of density due to **
C** temperature variations and the density ( DEN ) must be in the form of a  **
C** base 10 log.  No adjustments are made to the temperature or constituent  **
C** number densities in the region affected by this variation.               **
C**                                                                          **
C**                      DEN    = density (log10)                            **
C**                      ALT    = altitude (km)                              **
C**                      XLAT   = latitude (rad)                             **
C**                      DAY    = day number                                 **
C**                                                                          **
C******************************************************************************

C** initialize density (DEN) = 0.0
C
	DEN = 0.0
C
C** check if altitude exceeds 170 km
C
	IF ( ALT. GT. 170. ) RETURN
C
C** compute density change in lower thermosphere
C
	Z = ALT - 90.
	X = -0.0013 * Z * Z
	Y = 0.0172 * DAY + 1.72
	P = SIN (Y)
	SP = ( SIN (XLAT) ) **2
	S = 0.014 * Z * EXP (X)
	D = S * P * SP
C
C** check to compute absolute value of 'XLAT'
C
	IF ( XLAT. LT. 0. ) D = -D
	DEN = D

	RETURN
	END





      SUBROUTINE SLVH ( DEN , DENHE , XLAT , SDA )

C******************************************************************************
C** Subroutine 'SLVH' computes the seasonal-latitudinal varaition of the     **
C** helium number density according to L. Jacchia, SAO 332, 1971.  This      **
C** correction is not important below about 500 km.                          **
C**                                                                          **
C**                     DEN   = density (log10)                              **
C**                     DENHE = helium number density (log10)                **
C**                     XLAT  = latitude (rad)                               **
C**                     SDA   = solar declination angle (rad)                **
C******************************************************************************

	D0 = 10. ** DENHE
	A = ABS ( 0.65 * ( SDA / 0.40909079 ) )

	B  = 0.5 * XLAT
C
C** Check to compute absolute value of 'B'
C
	IF ( SDA. LT. 0. ) B = -B
C
C** compute X, Y, DHE and DENHE
C
	X  = 0.7854 - B
	Y  = ( SIN (X) ) ** 3
	DHE= A * ( Y - 0.35356 )
	DENHE = DENHE + DHE
C
C** compute helium number density change
C
	D1 = 10. ** DENHE
	DEL= D1 - D0
	RHO= 10. ** DEN
	DRHO = ( 6.646E-24 ) * DEL
	RHO  = RHO + DRHO
	DEN  = ALOG10 (RHO)

	RETURN
	END




	SUBROUTINE FAIR5 ( DHEL1 ,DHEL2 ,DLG1 ,DLG2 ,IH ,FDHEL ,FDLG )

C******************************************************************************
C** This subroutine fairs between the region above 500 km, which invokes the **
C** seasonal-latitudinal variation of the helium number density ( subroutine **
C** SLVH ), and the region below, which does not invoke any seasonal-        **
C** latitudinal variation at all.                                            **
C**                                                                          **
C** INPUTS:  DHEL1 = helium number density before invoking SLVH              **
C**          DHEL2 = helium number density after invoking SLVH               **
C**          DLG1  = total density before invoking SLVH                      **
C**          DLG2  = total density after invoking SLVH                       **
C**          IH    = height  ( km ) -- INTEGER                               **
C**          IBFH  = base fairing height ( km ) -- INTEGER                   **
C** OUTPUTS: FDHEL = faired helium number density                            **
C**          FDLG  = faired total density                                    **
C**                                                                          **
C** Written by Bill Jeffries, CSC, Huntsville, AL.                           **
C**            ph. (205) 830-1000, x311                                      **
C******************************************************************************

	DIMENSION CZ ( 6 )
	DATA CZ / 1.0, 0.9045085, 0.6545085, 0.3454915, 0.0954915, 0.0 /
	PARAMETER IBFH = 440

C  Height index
	I = ( IH - IBFH ) /10 + 1
C  Non-SLVH fairing coefficient
	CZI = CZ ( I )
C  SLVH fairing coefficient
	SZI = 1.0 - CZI
C  Faired density
	FDLG = ( DLG1 * CZI ) + ( DLG2 * SZI )
C  Faired helium number density
	FDHEL = ( DHEL1 * CZI ) + ( DHEL2 * SZI )

	RETURN
	END

