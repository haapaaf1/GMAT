;Main program RECCCRIT
;
;  PURPOSE
;     To plot contours of PIM critical frequencies and heights and TEC vs.
;     latitude and longitude from rectangular latitude/longitude grid output.
;
;  METHOD
;     This program is menu-driven.
;
;  LOCAL VARIABLES
;     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
;        -----------------------------Description------------------------------
;     ALT    Float      Vector        Km              > 0.
;                       (NALT)
;        The altitude grid
;     CRDTYP Integer    Scalar        n/a             0 or 1
;        A flag identifying the primary coordinate system used, 0 for
;        geographic or 1 for geomagnetic
;     C_LABS Integer    Vector        n/a             0 or 1
;                       (?)
;        Integer flags designating which contour lines to label, 1 for yes or
;        0 for no
;     DATTYP Integer    Scalar        n/a             0 <= DATTYP <= 2
;        A flag identifying the type of data, 0 for critical frequencies and
;        heights and TEC, 1 for electron density profiles, or 2 for electron
;        density profiles, critical frequencies and heights, and TEC
;     DAY    Integer    Scalar        n/a             1 <= DAY <= 366
;        The day of the year
;     DLAT   Float      Scalar        Degrees north   <> 0.
;        The spacing of the primary latitude grid
;     DLON   Float      Scalar        Degrees east    <> 0.
;        The spacing of the primary longitude grid
;     DUM1   Float      Vector        n/a             n/a
;                       (5)
;        A dummy array used for reading
;     DUM2   Float      Vector        n/a             n/a
;                       (7)
;        A dummy array used for reading
;     EDP    Float      Vector        cm-3            > 0.
;                       (NALT)
;        An electron density altitude profile
;     ELAT   Float      Scalar        Degrees north   -90. <= ELAT <= 90.
;        The ending latitude of the primary latitude grid
;     ELON   Float      Scalar        Degrees east    n/a
;        The ending longitude of the primary latitude grid
;     F10P7  Float      Scalar        Solar Flux      >= 0.
;                                     Units
;        The 10.7 cm solar flux
;     FILE   String     Scalar        n/a             Valid file name
;        The name of the PIM output file
;     FOE    Float      Matrix        MHz             >= 0.
;                       (NLAT,NLON)
;        The E-layer critical frequency
;     FOF1   Float      Matrix        MHz             >= 0.
;                       (NLAT,NLON)
;        The F1-layer critical frequency
;     FOF2   Float      Matrix        MHz             >= 0.
;                       (NLAT,NLON)
;        The F2-layer critical frequency
;     GLAT   Float      Matrix        Degrees north   -90. <= GLAT <= 90.
;                       (NLAT,NLON)
;        Geographic latitudes on the primary latitude and longitude grids
;     GLON   Float      Matrix        Degrees east    n/a
;                       (NLAT,NLON)
;        Geographic longitudes on the primary latitude and longitude grids
;     GRDTYP Integer    Scalar        n/a             0 <= GRDTYP <= 2
;        A flag identifying the type of output grid, 0 for a rectangular
;        latitude/longitude output grid, 1 for a latitude/longitude pairs
;        output grid, or 2 for an azimuth/elevation (ground-based) output grid
;     HME    Float      Matrix        Km              >= 0.
;                       (NLAT,NLON)
;        The E-layer critical height
;     HMF1   Float      Matrix        Km              >= 0.
;                       (NLAT,NLON)
;        The F1-layer critical height
;     HMF2   Float      Matrix        Km              >= 0.
;                       (NLAT,NLON)
;        The F2-layer critical height
;     IDUM   Integer    Scalar        n/a             n/a
;        A dummy variable used for reading
;     ILAT   Integer    Scalar        n/a             0 <= ILAT <= NLAT-1
;        The loop counter for the primary latitude grid
;     ILON   Integer    Scalar        n/a             0 <= ILON <= NLON-1
;        The loop counter for the primary longitude grid
;     IMENU  Integer    Scalar        n/a             1 <= IMENU <= 8
;        The menu choice
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
;     LEVELS Float      Vector        Varies          Varies
;                       (?)
;        The contour levels to plot
;     LUN    Long       Scalar        n/a             n/a
;        The logical unit number used to access the PIM output file
;     MLAT   Float      Matrix        Degrees north   -90. <= MLAT <= 90.
;                       (NLAT,NLON)
;        Geomagnetic latitudes on the primary latitude and longitude grids
;     MLON   Float      Matrix        Degrees east    n/a
;                       (NLAT,NLON)
;        Geomagnetic longitudes on the primary latitude and longitude grids
;     MLT    Float      Matrix        Decimal hours   0. <= MLT < 24.
;                       (NLAT,NLON)
;        Geomagnetic local times on the primary latitude and longitude grids
;     NALT   Integer    Scalar        n/a             > 0
;        The number of altitudes on the altitude grid
;     NLAT   Integer    Scalar        n/a             > 0
;        The number of latitudes on the primary latitude grid
;     NLON   Integer    Scalar        n/a             > 0
;        The number of longitudes on the primary longitude grid
;     PLASPH Integer    Scalar        n/a             0 or 1
;        The plasmasphere flag, 0 for no plasmasphere or 1 for a Gallagher
;        plasmasphere
;     PRMTYP Integer    Scalar        n/a             1 <= PRMTYP <= 10
;        A flag identifying the quantity to plot, 1 for foF2, 2 for nmF2,
;        3 for hmF2, 4 for foF1, 5 for nmF1, 6 for hmF1, 7 for foE, 8 for nmE,
;        9 for hmE, or 10 for TEC
;     SCLEV1 String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the contour
;        levels
;     SCLEV2 String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the range of the
;        contour levels
;     SCRD1  String     Scalar        n/a             n/a
;        A string identifying the primary coordinate system
;     SCRD2  String     Scalar        n/a             n/a
;        A string identifying the primary coordinate system
;     SDUM   String     Scalar        n/a             n/a
;        A dummy variable used for reading
;     SLAT   Float      Scalar        Degrees north   -90. <= SLAT <= 90.
;        The starting latitude of the primary latitude grid
;     SLON   Float      Scalar        Degrees east    n/a
;        The starting longitude of the primary longitude grid
;     SPARAM String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the parameter
;        being plotted
;     SSN    Float      Scalar        n/a             >= 0.
;        The sunspot number
;     SX1    Float      Scalar        Normalized      0. <= SX1 <= 1.
;                                     device
;                                     coordinates
;        A variable used for annotating plots
;     SY1    Float      Scalar        Normalized      0. <= SY1 <= 1.
;                                     device
;                                     coordinates
;        A variable used for annotating plots
;     TEC    Float      Matrix        TEC Units       >= 0.
;                       (NLAT,NLON)
;        Total electron content
;     UT     Float      Scalar        Decimal hours   0. <= UT < 24.
;        The universal time
;     X      Float      Vector        Degrees east    n/a
;                       (NLON)
;        The longitude grid for a plot
;     XRANGE Float      Vector        Degrees east    n/a
;                       (2)
;        The longitude range for a plot
;     XTITLE String     Scalar        n/a             n/a
;        The x-axis title
;     Y      Float      Vector        Degrees north   -90. <= Y <= 90.
;                       (NLAT)
;        The latitude grid for a plot
;     YEAR   Integer    Scalar        n/a             n/a
;        The year
;     YRANGE Float      Vector        Degrees north   -90. <= YRANGE <= 90.
;                       (2)
;        The latitude range for a plot
;     YTITLE String     Scalar        n/a             n/a
;        The y-axis title
;     Z      Float      Matrix        Varies          Varies
;                       (NLON,NLAT)
;        The z-variable for a plot
;
;  PROCEDURES REQUIRED
;     -Name- ----------------------------Description---------------------------
;     None
;
;  FUNCTIONS REQUIRED
;     -Name- ---Type--- ----------------------Description----------------------
;     None
;
;  FILES ACCESSED
;     ----Name---- ---Type--- -Unit- ---------------Description----------------
;     Varies       Formatted  Varies Rectangular-grid output file from PIM
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Waltham, MA 02154  USA
;     (617)-487-2250
;
;  VERSION
;     1.6   14-February-1997
;
;  MODIFICATIONS
;     ----Person---- ----Date---- -----------------Description-----------------
;     L. Brown        8-Mar-1993  1.0 ==> Created
;     L. Brown       10-May-1994  1.0 ==> 1.1
;                                 Keyword POSITION removed from AXIS calls
;                                 because POSITION is no longer supported by
;                                 AXIS.
;                                 Keyword SPLINE changed to FOLLOW in CONTOUR
;                                 call because splines are no longer supported
;                                 by CONTOUR.
;     L. Brown       13-Oct-1994  1.1 ==> 1.2
;                                 The data types of the output file have been
;                                 redefined.
;                                 The format of the PIM output file has
;                                 changed.
;     L. Brown       28-Apr-1995  1.2 ==> 1.3
;                                 Renamed from PIMCCRIT.PRO to RECCCRIT.PRO.
;                                 Now accepts only rectangular-grid output.
;     L. Brown       25-Sep-1995  1.3 ==> 1.4
;                                 A header line now precedes the critical
;                                 parameters line in the output file for
;                                 DATTYP=0.
;     L. Brown       24-Jan-1996  1.4 ==> 1.5
;                                 Renamed output grid type "satellite" to
;                                 "latitude/longitude pairs".
;                                 Renamed output grid type
;                                 "radar azimuth/elevation" to
;                                 "azimuth/elevation (ground-based)".
;     L. Brown       14-Feb-1997  1.5 ==> 1.6
;                                 The plasmasphere flag is now encoded in the
;                                 coordinate system type / output grid type
;                                 flag.
;                                 The plasmasphere option is now included in
;                                 the plot annotation.
;                                 The plasmasphere option is now included in
;                                 the case parameters display.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     -Name- ---Type--- -----Units----- --------------Description--------------
;     None
;
;  Get the name of the PIM output file
;
GET_FILE:
FILE=''
READ,'What is the name of the PIM rectangular-grid output file? ',FILE
FILE=STRCOMPRESS(FILE,/REMOVE_ALL)
;
;  Open the PIM output file
;
GET_LUN,LUN
OPENR,LUN,FILE
PRINT,'Reading data...Please wait...'
;
;  Read environment information from the PIM output file
;
YEAR=0
DAY=0
UT=0.
F10P7=0.
KP=0.
SSN=0.
SDUM=''
READF,LUN,SDUM
READF,LUN,SDUM
READF,LUN,YEAR,DAY,UT,F10P7,KP,SSN
UT=UT/3600.
;
;  Read the primary coordinate system type, output grid type, and plasmasphere
;  flag from the PIM output file
;
IDUM=0
READF,LUN,SDUM
READF,LUN,SDUM
READF,LUN,IDUM
CRDTYP=IDUM MOD 10
GRDTYP=(IDUM MOD 100)/10
PLASPH=IDUM/100
IF GRDTYP NE 0 THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for RECCCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECCCRIT
   ENDELSE
ENDIF
;
;  Read grid information from the PIM output file
;
SLAT=0.
SLON=0.
ELAT=0.
ELON=0.
NLAT=0
NLON=0
DLAT=0.
DLON=0.
READF,LUN,SDUM
READF,LUN,SDUM
READF,LUN,SLAT,ELAT,SLON,ELON,NLAT,NLON,DLAT,DLON
;
;  Read the output data type from the PIM output file
;
DATTYP=0
READF,LUN,DATTYP
IF (DATTYP NE 0) AND (DATTYP NE 2) THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of output data for RECCCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECCCRIT
   ENDELSE
ENDIF
;
;  Read critical frequencies and heights and TEC from the PIM output file
;
IF DATTYP EQ 2 THEN BEGIN
   NALT=0
   READF,LUN,NALT,FORMAT='(29X,I6)'
   READF,LUN,SDUM
   ALT=FLTARR(NALT)
   READF,LUN,ALT
   EDP=FLTARR(NALT)
ENDIF
GLAT=FLTARR(NLAT,NLON)
GLON=FLTARR(NLAT,NLON)
MLAT=FLTARR(NLAT,NLON)
MLON=FLTARR(NLAT,NLON)
MLT=FLTARR(NLAT,NLON)
FOF2=FLTARR(NLAT,NLON)
HMF2=FLTARR(NLAT,NLON)
FOF1=FLTARR(NLAT,NLON)
HMF1=FLTARR(NLAT,NLON)
FOE=FLTARR(NLAT,NLON)
HME=FLTARR(NLAT,NLON)
TEC=FLTARR(NLAT,NLON)
DUM1=FLTARR(5)
DUM2=FLTARR(7)
FOR ILAT=0,NLAT-1 DO BEGIN
   FOR ILON=0,NLON-1 DO BEGIN
      READF,LUN,SDUM
      READF,LUN,DUM1
      GLAT(ILAT,ILON)=DUM1(0)
      GLON(ILAT,ILON)=DUM1(1)
      MLAT(ILAT,ILON)=DUM1(2)
      MLON(ILAT,ILON)=DUM1(3)
      MLT(ILAT,ILON)=DUM1(4)
      IF DATTYP EQ 2 THEN BEGIN
         READF,LUN,SDUM
         READF,LUN,EDP
      ENDIF
      READF,LUN,SDUM
      READF,LUN,DUM2
      FOF2(ILAT,ILON)=DUM2(0)
      HMF2(ILAT,ILON)=DUM2(1)
      FOF1(ILAT,ILON)=DUM2(2)
      HMF1(ILAT,ILON)=DUM2(3)
      FOE(ILAT,ILON)=DUM2(4)
      HME(ILAT,ILON)=DUM2(5)
      TEC(ILAT,ILON)=DUM2(6)
   ENDFOR
ENDFOR
;
;  Close the PIM output file
;
CLOSE,LUN
FREE_LUN,LUN
;
;  Set the latitude and longitude grids according to the primary coordinate
;  system type
;
IF CRDTYP EQ 0 THEN BEGIN
   X=REFORM(GLON(0,*))
   Y=GLAT(*,0)
   SCRD1='geographic'
   SCRD2='Geographic'
ENDIF ELSE BEGIN
   X=REFORM(MLON(0,*))
   Y=MLAT(*,0)
   SCRD1='geomagnetic'
   SCRD2='Geomagnetic'
ENDELSE
XTITLE=SCRD2+' Longitude (!uo!nE)'
YTITLE=SCRD2+' Latitude (!uo!nN)'
;
;  Attempt to insure that the primary longitude grid is monotonic for the
;  CONTOUR routine
;  Note: This is for cases where the user's desired longitude range for PIM is
;        other than [0,360], for example [-180,180].
;
X=(360.+(X MOD 360.)) MOD 360.
DUM=WHERE(INDGEN(NLON) EQ SORT(X),IDUM)
IF IDUM LT NLON THEN BEGIN
   IF (IDUM EQ 1) AND (X(NLON-1) LE X(NLON-2)) THEN BEGIN
      X(NLON-1)=X(NLON-1)+360.
   ENDIF ELSE BEGIN
      ILON=0
      WHILE (X(ILON) LE X(ILON+1)) AND (ILON LT NLON-2) DO BEGIN
         X(ILON)=X(ILON)-360.
         ILON=ILON+1
      ENDWHILE
      IF ILON EQ NLON-2 THEN BEGIN
         IF X(ILON) LE X(ILON+1) THEN X(ILON)=X(ILON)-360.
      ENDIF ELSE BEGIN
         X(ILON)=X(ILON)-360.
      ENDELSE
   ENDELSE
   DUM=WHERE(INDGEN(NLON) EQ SORT(X),IDUM)
   IF IDUM LT NLON THEN BEGIN
      PRINT,'RECCCRIT:Primary longitude grid is not monotonically '+$
            'increasing in one period of'
      PRINT,'         longitude (360 degree range).'
      STOP,'RECCCRIT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plotting parameters
;
PRMTYP=1
Z=TRANSPOSE(FOF2)
LEVELS=INDGEN(30)
C_LABS=INDGEN(30) MOD 2
SPARAM='f!do!nF!d2!n (MHz)
SCLEV1='Contours every 1 MHz'
SCLEV2='from 0 to 29 MHz'
XRANGE=[MIN(X),MAX(X)]
YRANGE=[MIN(Y),MAX(Y)]
;
;  Display the menu
;
DISPLAY_MENU:
PRINT,'1) File: '+FILE
CASE PRMTYP OF
   1   :PRINT,'2) Parameter: foF2'
   2   :PRINT,'2) Parameter: nmF2'
   3   :PRINT,'2) Parameter: hmF2'
   4   :PRINT,'2) Parameter: foF1'
   5   :PRINT,'2) Parameter: nmF1'
   6   :PRINT,'2) Parameter: hmF1'
   7   :PRINT,'2) Parameter: foE'
   8   :PRINT,'2) Parameter: nmE'
   9   :PRINT,'2) Parameter: hmE'
   ELSE:PRINT,'2) Parameter: TEC'
ENDCASE
PRINT,'$(A,F6.2,A,F6.2,A)','3) Latitude range: ',YRANGE(0),' to ',YRANGE(1), $
      ' degrees north '+SCRD1
PRINT,'$(A,F7.2,A,F7.2,A)','4) Longitude range: ',XRANGE(0),' to ',XRANGE(1), $
      ' degrees east '+SCRD1
PRINT,'5) Make plot'
PRINT,'6) Display case parameters
PRINT,'7) Redisplay the menu'
PRINT,'8) Quit
;
; Get the menu choice
;
IMENU=0
WHILE (IMENU LT 1) OR (IMENU GT 8) DO BEGIN
   READ,'What is your choice (1-8)? ',IMENU
ENDWHILE
;
;  Process the menu choice
;
CASE IMENU OF
   1   :GOTO,GET_FILE
   2   :BEGIN
           PRMTYP=0
           WHILE (PRMTYP LT 1) OR (PRMTYP GT 10) DO BEGIN
              PRINT,'Do you want to plot'
              PRINT,'   (1) foF2,'
              PRINT,'   (2) nmF2,'
              PRINT,'   (3) hmF2,'
              PRINT,'   (4) foF1,'
              PRINT,'   (5) nmF1,'
              PRINT,'   (6) hmF1,'
              PRINT,'   (7) foE,'
              PRINT,'   (8) nmE,'
              PRINT,'   (9) hmE, or'
              PRINT,'  (10) TEC'
              READ,'(1-10)? ',PRMTYP
           ENDWHILE
           CASE PRMTYP OF
           1   :BEGIN
                   Z=TRANSPOSE(FOF2)
                   LEVELS=INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='f!do!nF!d2!n (MHz)
                   SCLEV1='Contours every 1 MHz'
                   SCLEV2='from 0 to 29 MHz'
                END
           2   :BEGIN
                   Z=TRANSPOSE(1.24E4*FOF2^2)
                   DUM=FINDGEN(9)+1.
                   LEVELS=[DUM*1.E4,DUM*1.E5,DUM*1.E6,1.E7]
                   IDUM=(INDGEN(9)+1) MOD 2
                   C_LABS=[IDUM,IDUM,IDUM,1]
                   SPARAM='n!dm!nF!d2!n (cm!u-3!n)
                   SCLEV1='Contours at [1,2,...,8,9]*10!un!n cm!u-3!n'
                   SCLEV2='from 10!u4!n to 10!u7!n cm!u-3!n'
                END
           3   :BEGIN
                   Z=TRANSPOSE(HMF2)
                   LEVELS=250+10*INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='h!dm!nF!d2!n (km)
                   SCLEV1='Contours every 10 km'
                   SCLEV2='from 250 to 540 km'
                END
           4   :BEGIN
                   Z=TRANSPOSE(FOF1)
                   LEVELS=INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='f!do!nF!d1!n (MHz)
                   SCLEV1='Contours every 1 MHz'
                   SCLEV2='from 0 to 29 MHz'
                END
           5   :BEGIN
                   Z=TRANSPOSE(1.24E4*FOF1^2)
                   DUM=FINDGEN(9)+1.
                   LEVELS=[DUM*1.E4,DUM*1.E5,DUM*1.E6,1.E7]
                   IDUM=(INDGEN(9)+1) MOD 2
                   C_LABS=[IDUM,IDUM,IDUM,1]
                   SPARAM='n!dm!nF!d1!n (cm!u-3!n)
                   SCLEV1='Contours at [1,2,...,8,9]*10!un!n cm!u-3!n'
                   SCLEV2='from 10!u4!n to 10!u7!n cm!u-3!n'
                END
           6   :BEGIN
                   Z=TRANSPOSE(HMF1)
                   LEVELS=250+10*INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='h!dm!nF!d1!n (km)
                   SCLEV1='Contours every 10 km'
                   SCLEV2='from 250 to 540 km'
                END
           7   :BEGIN
                   Z=TRANSPOSE(FOE)
                   LEVELS=FINDGEN(30)/2.
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='f!do!nE (MHz)
                   SCLEV1='Contours every 0.5 MHz'
                   SCLEV2='from 0 to 14.5 MHz'
                END
           8   :BEGIN
                   Z=TRANSPOSE(1.24E4*FOE^2)
                   DUM=FINDGEN(9)+1.
                   LEVELS=[DUM*1.E3,DUM*1.E4,DUM*1.E5,1.E6]
                   IDUM=(INDGEN(9)+1) MOD 2
                   C_LABS=[IDUM,IDUM,IDUM,1]
                   SPARAM='n!dm!nE (cm!u-3!n)
                   SCLEV1='Contours at [1,2,...,8,9]*10!un!n cm!u-3!n'
                   SCLEV2='from 10!u3!n to 10!u6!n cm!u-3!n'
                END
           9   :BEGIN
                   Z=TRANSPOSE(HME)
                   LEVELS=80+5*INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='h!dm!nE (km)'
                   SCLEV1='Contours every 5 km'
                   SCLEV2='from 80 to 225 km'
                END
           ELSE:BEGIN
                   Z=TRANSPOSE(TEC)
                   LEVELS=5*INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='TEC (TEC Units)'
                   SCLEV1='Contours every 5 TEC Units'
                   SCLEV2='from 0 to 145 TEC Units'
                END
           ENDCASE
        END
   3   :BEGIN
           PRINT,'Available '+SCRD1+' latitudes are'
           PRINT,Y,FORMAT='(10F7.1)'
           READ,'What range of '+SCRD1+' latitude do you want? ',YRANGE
        END
   4   :BEGIN
           PRINT,'Available '+SCRD1+' longitudes are'
           PRINT,X,FORMAT='(10F7.1)'
           READ,'What range of '+SCRD1+' longitude do you want? ',XRANGE
        END
   5   :BEGIN
           CONTOUR,Z,X,Y,/FOLLOW,/NORMAL,POSITION=[.125,.125,.675,.925], $
                   LEVELS=LEVELS,C_LABELS=C_LABS, $
                   XSTYLE=5,XRANGE=XRANGE, $
                   YSTYLE=5,YRANGE=YRANGE
           AXIS,.125,.100,XAXIS=0,/NORMAL, $
                XSTYLE=1,XRANGE=XRANGE,XTICKS=1,XTITLE=XTITLE
           AXIS,.125,.950,XAXIS=1,/NORMAL, $
                XSTYLE=1,XRANGE=XRANGE,XTICKS=1,XTICKNAME=REPLICATE(' ',2)
           AXIS,.100,.125,YAXIS=0,/NORMAL, $
                YSTYLE=1,YRANGE=YRANGE,YTICKS=1,YTICKLEN=.01,YTITLE=YTITLE
           AXIS,.700,.125,YAXIS=1,/NORMAL, $
                YSTYLE=1,YRANGE=YRANGE,YTICKS=1,YTICKLEN=.01, $
                YTICKNAME=REPLICATE(' ',2)
           SX1=.725
           SY1=.925
           XYOUTS,/NORMAL,SX1,SY1,FILE,SIZE=1.5
           SY1=SY1-.080
           XYOUTS,/NORMAL,SX1,SY1,'Year: '+STRING(YEAR,'(I4)'),SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'Day: '+STRCOMPRESS(STRING(DAY,'(I3)'), $
                  /REMOVE_ALL),SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'UT: '+STRCOMPRESS(STRING(UT,'(F5.2)'), $
                  /REMOVE_ALL)+' hr',SIZE=1.25
           SY1=SY1-.080
           XYOUTS,/NORMAL,SX1,SY1,'F!d10.7!n: '+STRCOMPRESS(STRING(F10P7, $
                  '(F5.1)'),/REMOVE_ALL),SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'SSN: '+STRCOMPRESS(STRING(SSN,'(F5.1)'), $
                  /REMOVE_ALL),SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'K!dp!n: '+STRCOMPRESS(STRING(KP,'(F3.1)'), $
                  /REMOVE_ALL),SIZE=1.25
           SY1=SY1-.080
           IF PLASPH EQ 0 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: None',SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: Gallagher',SIZE=1.25
           ENDELSE
           SY1=SY1-.160
           XYOUTS,/NORMAL,SX1,SY1,SPARAM,SIZE=1.5
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,SCLEV1,SIZE=1
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,SCLEV2,SIZE=1
        END
   6   :BEGIN
           PRINT,'--- Case parameters for file '+FILE+' ---'
           PRINT,'$(A,I4)','Year: ',YEAR
           PRINT,'$(A,I3)','Day: ',DAY
           PRINT,'$(A,F5.2,A)','UT: ',UT,' hr'
           PRINT,'$(A,F5.1)','F10.7: ',F10P7
           PRINT,'$(A,F5.1)','SSN: ',SSN
           PRINT,'$(A,F3.1)','Kp: ',KP
           PRINT,'Primary coordinate system: '+SCRD2
           PRINT,'$(A,F5.1,A,F5.1,A,F5.1,A)','Dataset latitude range: ', $
                 MIN(Y),' to ',MAX(Y),' in steps of ',DLAT, $
                 ' degrees north '+SCRD1
           PRINT,'$(A,F6.1,A,F6.1,A,F5.1,A)','Dataset longitude range: ', $
                 MIN(X),' to ',MAX(X),' in steps of ',DLON, $
                 ' degrees east '+SCRD1
           IF DATTYP EQ 0 THEN BEGIN
              PRINT,'Type of data: Critical frequencies and heights and TEC'
           ENDIF ELSE BEGIN
              PRINT,'Type of data: EDPs, critical frequencies and heights, ', $
                    'and TEC'
           ENDELSE
           PRINT,'Type of grid: Rectangular latitude/longitude'
           IF PLASPH GT 0 THEN BEGIN
              PRINT,'Plasmasphere: Gallagher'
           ENDIF ELSE BEGIN
              PRINT,'Plasmasphere: None'
           ENDELSE
           PRINT,'--- Press <ENTER> to continue ---'
           SDUM=GET_KBRD(1)
        END
   7   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_RECCCRIT
ENDCASE
GOTO,DISPLAY_MENU
;
END_RECCCRIT:
PRINT,'RECCCRIT terminated normally.'
;
END
