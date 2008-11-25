;Main program RECPCRIT
;
;  PURPOSE
;     To plot profiles of PIM critical frequencies and heights and TEC vs.
;     latitude or longitude from rectangular latitude/longitude grid output.
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
;     FILE   String     Scalar        n/a             File must exist
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
;     IMENU  Integer    Scalar        n/a             0 <= IMENU <= 9
;        The menu choice
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
;     LAT    Float      Vector        Degrees north   -90. <= LAT <= 90.
;                       (NLAT)
;        The primary latitude grid
;     LON    Float      Vector        Degrees east    n/a
;                       (NLON)
;        The primary longitude grid
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
;     PRFTYP Integer    Scalar        n/a             1 <= PRFTYP <= 2
;        A flag identifying the type of plot to make, 1 for a latitude profile
;        (constant longitude) or 2 for a longitude profile (constant latitude)
;     PRMTYP Integer    Scalar        n/a             1 <= PRMTYP <= 10
;        A flag identifying the quantity to plot, 1 for foF2, 2 for nmF2,
;        3 for hmF2, 4 for foF1, 5 for nmF1, 6 for hmF1, 7 for foE, 8 for nmE,
;        9 for hmE, or 10 for TEC
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
;     X      Float      Vector        Varies          Varies
;                       (?)
;        The x-coordinates for a plot
;     XRANGE Float      Vector        Varies          Varies
;                       (2)
;        The x-range for a plot
;     XTITLE String     Scalar        n/a             n/a
;        The x-axis title
;     XTYPE  Integer    Scalar        n/a             XTYPE = 0 or 1
;        The x-axis type, 0 for linear or 1 for logarithmic
;     Y      Float      Vector        Varies          Varies
;                       (?)
;        The y-coordinates for a plot
;     YEAR   Integer    Scalar        n/a             n/a
;        The year
;     YRANGE Float      Vector        Varies          Varies
;                       (2)
;        The y-range for a plot
;     YTITLE String     Scalar        n/a             n/a
;        The y-axis title
;     YTYPE  Integer    Scalar        n/a             YTYPE = 0 or 1
;        The y-axis type, 0 for linear or 1 for logarithmic
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
;                                 because it is no longer allowed.
;     L. Brown       13-Oct-1994  1.1 ==> 1.2
;                                 The data types of the output file have been
;                                 redefined.
;                                 The format of the PIM output file has
;                                 changed.
;     L. Brown       28-Apr-1995  1.2 ==> 1.3
;                                 Renamed from PIMPCRIT.PRO to RECPCRIT.PRO.
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
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for RECPCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECPCRIT
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
   PRINT,'File "'+FILE+'" contains the wrong type of output data for RECPCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECPCRIT
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
   LON=REFORM(GLON(0,*))
   LAT=GLAT(*,0)
   SCRD1='geographic'
   SCRD2='Geographic'
ENDIF ELSE BEGIN
   LON=REFORM(MLON(0,*))
   LAT=MLAT(*,0)
   SCRD1='geomagnetic'
   SCRD2='Geomagnetic'
ENDELSE
;
;  Attempt to insure that the primary longitude grid is monotonic
;  Note: This is for cases where the user's desired longitude range for PIM is
;        other than [0,360], for example [-180,180].
;
LON=(360.+(LON MOD 360.)) MOD 360.
DUM=WHERE(INDGEN(NLON) EQ SORT(LON),IDUM)
IF IDUM LT NLON THEN BEGIN
   IF (IDUM EQ 1) AND (LON(NLON-1) LE LON(NLON-2)) THEN BEGIN
      LON(NLON-1)=LON(NLON-1)+360.
   ENDIF ELSE BEGIN
      ILON=0
      WHILE (LON(ILON) LE LON(ILON+1)) AND (ILON LT NLON-2) DO BEGIN
         LON(ILON)=LON(ILON)-360.
         ILON=ILON+1
      ENDWHILE
      IF ILON EQ NLON-2 THEN BEGIN
         IF LON(ILON) LE LON(ILON+1) THEN LON(ILON)=LON(ILON)-360.
      ENDIF ELSE BEGIN
         LON(ILON)=LON(ILON)-360.
      ENDELSE
   ENDELSE
   DUM=WHERE(INDGEN(NLON) EQ SORT(LON),IDUM)
   IF IDUM LT NLON THEN BEGIN
      PRINT,'RECPCRIT:Primary longitude grid is not monotonically '+$
            'increasing in one period of'
      PRINT,'         longitude (360 degree range).'
      STOP,'RECPCRIT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plotting parameters
;
PRFTYP=1
PRMTYP=1
ILAT=0
ILON=0
X=FOF2(*,ILON)
XRANGE=[MIN(X),MAX(X)]
Y=LAT
YRANGE=[MIN(Y),MAX(Y)]
;
;  Display the menu
;
DISPLAY_MENU:
PRINT,'1) File: '+FILE
IF PRFTYP EQ 1 THEN BEGIN
   PRINT,'2) Type of profile: Latitude (constant longitude)'
   PRINT,'$(A,F6.1,A)','3) Constant longitude: ',LON(ILON), $
         ' degrees east '+SCRD1
   PRINT,'$(A,F5.1,A,F5.1,A)','4) Latitude range: ',YRANGE(0),' to ', $
         YRANGE(1),' degrees north '+SCRD1
   CASE PRMTYP OF
      1   :BEGIN
              PRINT,'5) Parameter: foF2'
              PRINT,'$(A,F5.1,A,F5.1)','6) foF2 range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      2   :BEGIN
              PRINT,'5) Parameter: nmF2'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmF2 range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      3   :BEGIN
              PRINT,'5) Parameter: hmF2'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmF2 range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      4   :BEGIN
              PRINT,'5) Parameter: foF1'
              PRINT,'$(A,F5.1,A,F5.1)','6) foF1 range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      5   :BEGIN
              PRINT,'5) Parameter: nmF1'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmF1 range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      6   :BEGIN
              PRINT,'5) Parameter: hmF1'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmF1 range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      7   :BEGIN
              PRINT,'5) Parameter: foE'
              PRINT,'$(A,F5.1,A,F5.1)','6) foE range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      8   :BEGIN
              PRINT,'5) Parameter: nmE'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmE range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      9   :BEGIN
              PRINT,'5) Parameter: hmE'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmE range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      ELSE:BEGIN
              PRINT,'5) Parameter: TEC'
              PRINT,'$(A,F5.1,A,F5.1)','6) TEC range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
   ENDCASE
ENDIF ELSE BEGIN
   PRINT,'2) Type of profile: Longitude (constant latitude)'
   PRINT,'$(A,F6.1,A)','3) Constant latitude: ',LAT(ILAT), $
         ' degrees north '+SCRD1
   PRINT,'$(A,F6.1,A,F6.1,A)','4) Longitude range: ',XRANGE(0),' to ', $
         XRANGE(1),' degrees east '+SCRD1
   CASE PRMTYP OF
      1   :BEGIN
              PRINT,'5) Parameter: foF2'
              PRINT,'$(A,F5.1,A,F5.1)','6) foF2 range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      2   :BEGIN
              PRINT,'5) Parameter: nmF2'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmF2 range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      3   :BEGIN
              PRINT,'5) Parameter: hmF2'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmF2 range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      4   :BEGIN
              PRINT,'5) Parameter: foF1'
              PRINT,'$(A,F5.1,A,F5.1)','6) foF1 range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      5   :BEGIN
              PRINT,'5) Parameter: nmF1'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmF1 range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      6   :BEGIN
              PRINT,'5) Parameter: hmF1'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmF1 range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      7   :BEGIN
              PRINT,'5) Parameter: foE'
              PRINT,'$(A,F5.1,A,F5.1)','6) foE range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      8   :BEGIN
              PRINT,'5) Parameter: nmE'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmE range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      9   :BEGIN
              PRINT,'5) Parameter: hmE'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmE range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      ELSE:BEGIN
              PRINT,'5) Parameter: TEC'
              PRINT,'$(A,F5.1,A,F5.1)','6) TEC range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
   ENDCASE
ENDELSE
PRINT,'7) Make plot'
PRINT,'8) Display case parameters
PRINT,'9) Redisplay the menu'
PRINT,'0) Quit
;
; Get the menu choice
;
IMENU=-1
WHILE (IMENU LT 0) OR (IMENU GT 9) DO BEGIN
   READ,'What is your choice (1-9,0)? ',IMENU
ENDWHILE
;
;  Process the menu choice
;
CASE IMENU OF
   1   :GOTO,GET_FILE
   2   :BEGIN
           PRFTYP=0
           WHILE (PRFTYP NE 1) AND (PRFTYP NE 2) DO BEGIN
              PRINT,'What kind of profile do you want -'
              PRINT,'   (1) Latitude (constant longitude), or'
              PRINT,'   (2) Longitude (constant latitude)'
              READ,'(1,2)? ',PRFTYP
           ENDWHILE
           IF PRFTYP EQ 1 THEN BEGIN
              CASE PRMTYP OF
                 1   :X=FOF2(*,ILON)
                 2   :X=1.24E4*(FOF2(*,ILON)^2)
                 3   :X=HMF2(*,ILON)
                 4   :X=FOF1(*,ILON)
                 5   :X=1.24E4*(FOF1(*,ILON)^2)
                 6   :X=HMF1(*,ILON)
                 7   :X=FOE(*,ILON)
                 8   :X=1.24E4*(FOE(*,ILON)^2)
                 9   :X=HME(*,ILON)
                 ELSE:X=TEC(*,ILON)
              ENDCASE
              Y=LAT
           ENDIF ELSE BEGIN
              X=LON
              CASE PRMTYP OF
                 1   :Y=REFORM(FOF2(ILAT,*))
                 2   :Y=REFORM(1.24E4*(FOF2(ILAT,*)^2))
                 3   :Y=REFORM(HMF2(ILAT,*))
                 4   :Y=REFORM(FOF1(ILAT,*))
                 5   :Y=REFORM(1.24E4*(FOF1(ILAT,*)^2))
                 6   :Y=REFORM(HMF1(ILAT,*))
                 7   :Y=REFORM(FOE(ILAT,*))
                 8   :Y=REFORM(1.24E4*(FOE(ILAT,*)^2))
                 9   :Y=REFORM(HME(ILAT,*))
                 ELSE:Y=REFORM(TEC(ILAT,*))
              ENDCASE
           ENDELSE
           XRANGE=[MIN(X),MAX(X)]
           YRANGE=[MIN(Y),MAX(Y)]
        END
   3   :BEGIN
           DUM=0.
           IDUM=0
           IF PRFTYP EQ 1 THEN BEGIN
              PRINT,'Available '+SCRD1+' longitudes are'
              PRINT,LON,FORMAT='(10F7.1)'
              READ,'What constant '+SCRD1+' longitude do you want? ',DUM
              IDUM=MIN(ABS(LON-DUM))
              ILON=!C
              CASE PRMTYP OF
                 1   :X=FOF2(*,ILON)
                 2   :X=1.24E4*(FOF2(*,ILON)^2)
                 3   :X=HMF2(*,ILON)
                 4   :X=FOF1(*,ILON)
                 5   :X=1.24E4*(FOF1(*,ILON)^2)
                 6   :X=HMF1(*,ILON)
                 7   :X=FOE(*,ILON)
                 8   :X=1.24E4*(FOE(*,ILON)^2)
                 9   :X=HME(*,ILON)
                 ELSE:X=TEC(*,ILON)
              ENDCASE
              XRANGE=[MIN(X),MAX(X)]
           ENDIF ELSE BEGIN
              PRINT,'Available '+SCRD1+' latitudes are'
              PRINT,LAT,FORMAT='(10F7.1)'
              READ,'What constant '+SCRD1+' latitude do you want? ',DUM
              IDUM=MIN(ABS(LAT-DUM))
              ILAT=!C
              CASE PRMTYP OF
                 1   :Y=REFORM(FOF2(ILAT,*))
                 2   :Y=REFORM(1.24E4*(FOF2(ILAT,*)^2))
                 3   :Y=REFORM(HMF2(ILAT,*))
                 4   :Y=REFORM(FOF1(ILAT,*))
                 5   :Y=REFORM(1.24E4*(FOF1(ILAT,*)^2))
                 6   :Y=REFORM(HMF1(ILAT,*))
                 7   :Y=REFORM(FOE(ILAT,*))
                 8   :Y=REFORM(1.24E4*(FOE(ILAT,*)^2))
                 9   :Y=REFORM(HME(ILAT,*))
                 ELSE:Y=REFORM(TEC(ILAT,*))
              ENDCASE
              YRANGE=[MIN(Y),MAX(Y)]
           ENDELSE
        END
   4   :IF PRFTYP EQ 1 THEN BEGIN
           PRINT,'Available '+SCRD1+' latitudes are'
           PRINT,LAT,FORMAT='(10F7.1)'
           READ,'What range of '+SCRD1+' latitude do you want? ',YRANGE
        ENDIF ELSE BEGIN
           PRINT,'Available '+SCRD1+' longitudes are'
           PRINT,LON,FORMAT='(10F7.1)'
           READ,'What range of '+SCRD1+' longitude do you want? ',XRANGE
        ENDELSE
   5   :BEGIN
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
           IF PRFTYP EQ 1 THEN BEGIN
              CASE PRMTYP OF
                 1   :X=FOF2(*,ILON)
                 2   :X=1.24E4*(FOF2(*,ILON)^2)
                 3   :X=HMF2(*,ILON)
                 4   :X=FOF1(*,ILON)
                 5   :X=1.24E4*(FOF1(*,ILON)^2)
                 6   :X=HMF1(*,ILON)
                 7   :X=FOE(*,ILON)
                 8   :X=1.24E4*(FOE(*,ILON)^2)
                 9   :X=HME(*,ILON)
                 ELSE:X=TEC(*,ILON)
              ENDCASE
              XRANGE=[MIN(X),MAX(X)]
           ENDIF ELSE BEGIN
              CASE PRMTYP OF
                 1   :Y=REFORM(FOF2(ILAT,*))
                 2   :Y=REFORM(1.24E4*(FOF2(ILAT,*)^2))
                 3   :Y=REFORM(HMF2(ILAT,*))
                 4   :Y=REFORM(FOF1(ILAT,*))
                 5   :Y=REFORM(1.24E4*(FOF1(ILAT,*)^2))
                 6   :Y=REFORM(HMF1(ILAT,*))
                 7   :Y=REFORM(FOE(ILAT,*))
                 8   :Y=REFORM(1.24E4*(FOE(ILAT,*)^2))
                 9   :Y=REFORM(HME(ILAT,*))
                 ELSE:Y=REFORM(TEC(ILAT,*))
              ENDCASE
              YRANGE=[MIN(Y),MAX(Y)]
           ENDELSE
        END
   6   :IF PRFTYP EQ 1 THEN BEGIN
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','foF2 ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of foF2 do you want? ',XRANGE
                   END
              2   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmF2 ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of nmF2 do you want? ',XRANGE
                   END
              3   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmF2 ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of hmF2 do you want? ',XRANGE
                   END
              4   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','foF1 ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of foF1 do you want? ',XRANGE
                   END
              5   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmF1 ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of nmF1 do you want? ',XRANGE
                   END
              6   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmF1 ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of hmF1 do you want? ',XRANGE
                   END
              7   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','foE ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of foE do you want? ',XRANGE
                   END
              8   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmE ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of nmE do you want? ',XRANGE
                   END
              9   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmE ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of hmE do you want? ',XRANGE
                   END
              ELSE:BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','TEC ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of TEC do you want? ',XRANGE
                   END
           ENDCASE
        ENDIF ELSE BEGIN
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','foF2 ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of foF2 do you want? ',YRANGE
                   END
              2   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmF2 ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of nmF2 do you want? ',YRANGE
                   END
              3   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmF2 ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of hmF2 do you want? ',YRANGE
                   END
              4   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','foF1 ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of foF1 do you want? ',YRANGE
                   END
              5   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmF1 ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of nmF1 do you want? ',YRANGE
                   END
              6   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmF1 ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of hmF1 do you want? ',YRANGE
                   END
              7   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','foE ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of foE do you want? ',YRANGE
                   END
              8   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmE ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of nmE do you want? ',YRANGE
                   END
              9   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmE ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of hmE do you want? ',YRANGE
                   END
              ELSE:BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','TEC ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of TEC do you want? ',YRANGE
                   END
           ENDCASE
        ENDELSE
   7   :BEGIN
           IF PRFTYP EQ 1 THEN BEGIN
              CASE PRMTYP OF
                 1   :BEGIN
                         XTITLE='f!do!nF!d2!n (MHz)'
                         XTYPE=0
                      END
                 2   :BEGIN
                         XTITLE='n!dm!nF!d2!n (cm!u-3!n)'
                         XTYPE=1
                      END
                 3   :BEGIN
                         XTITLE='h!dm!nF!d2!n (km)'
                         XTYPE=0
                      END
                 4   :BEGIN
                         XTITLE='f!do!nF!d1!n (MHz)'
                         XTYPE=0
                      END
                 5   :BEGIN
                         XTITLE='n!dm!nF!d1!n (cm!u-3!n)'
                         XTYPE=1
                      END
                 6   :BEGIN
                         XTITLE='h!dm!nF!d1!n (km)'
                         XTYPE=0
                      END
                 7   :BEGIN
                         XTITLE='f!do!nE (MHz)'
                         XTYPE=0
                      END
                 8   :BEGIN
                         XTITLE='n!dm!nE (cm!u-3!n)'
                         XTYPE=1
                      END
                 9   :BEGIN
                         XTITLE='h!dm!nE (km)'
                         XTYPE=0
                      END
                 ELSE:BEGIN
                         XTITLE='TEC (TEC Units)'
                         XTYPE=0
                      END
              ENDCASE
              YTITLE=SCRD2+' Latitude (!uo!nN)'
              YTYPE=0
           ENDIF ELSE BEGIN
              XTITLE=SCRD2+' Longitude (!uo!nE)'
              XTYPE=0
              CASE PRMTYP OF
                 1   :BEGIN
                         YTITLE='f!do!nF!d2!n (MHz)'
                         YTYPE=0
                      END
                 2   :BEGIN
                         YTITLE='n!dm!nF!d2!n (cm!u-3!n)'
                         YTYPE=1
                      END
                 3   :BEGIN
                         YTITLE='h!dm!nF!d2!n (km)'
                         YTYPE=0
                      END
                 4   :BEGIN
                         YTITLE='f!do!nF!d1!n (MHz)'
                         YTYPE=0
                      END
                 5   :BEGIN
                         YTITLE='n!dm!nF!d1!n (cm!u-3!n)'
                         YTYPE=1
                      END
                 6   :BEGIN
                         YTITLE='h!dm!nF!d1!n (km)'
                         YTYPE=0
                      END
                 7   :BEGIN
                         YTITLE='f!do!nE (MHz)'
                         YTYPE=0
                      END
                 8   :BEGIN
                         YTITLE='n!dm!nE (cm!u-3!n)'
                         YTYPE=1
                      END
                 9   :BEGIN
                         YTITLE='h!dm!nE (km)'
                         YTYPE=0
                      END
                 ELSE:BEGIN
                         YTITLE='TEC (TEC Units)'
                         YTYPE=0
                      END
              ENDCASE
           ENDELSE
           PLOT,X,Y,/NORMAL,POSITION=[.125,.125,.675,.925], $
                XSTYLE=5,XTYPE=XTYPE,XRANGE=XRANGE, $
                YSTYLE=5,YTYPE=YTYPE,YRANGE=YRANGE
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
           IF PRFTYP EQ 1 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Lon: '+ $
                     STRCOMPRESS(STRING(LON(ILON),'(F6.1)'), $
                     /REMOVE_ALL)+' !uo!nE '+SCRD1,SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Lat: '+ $
                     STRCOMPRESS(STRING(LAT(ILAT),'(F5.1)'), $
                     /REMOVE_ALL)+' !uo!nN '+SCRD1,SIZE=1.25
           ENDELSE
           SY1=SY1-.080
           IF PLASPH EQ 0 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: None',SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: Gallagher',SIZE=1.25
           ENDELSE
        END
   8   :BEGIN
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
   9   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_RECPCRIT
ENDCASE
GOTO,DISPLAY_MENU
;
END_RECPCRIT:
PRINT,'RECPCRIT terminated normally.'
;
END
