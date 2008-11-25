;Main program RECPCUT
;
;  PURPOSE
;     To plot profiles of PIM electron density and plasma frequency for cuts in
;     latitude, longitude, or altitude from rectangular latitude/longitude grid
;     output.
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
;     DUM    Float      Scalar        n/a             n/a
;        An intermediate dummy variable
;     DUM1   Float      Vector        n/a             n/a
;                       (5)
;        A dummy array used for reading
;     DUM2   Float      Vector        n/a             n/a
;                       (NALT)
;        A dummy array used for reading
;     EDP    Float      Matrix        cm-3            > 0.
;                       (NALT,NLAT,
;                        NLON)
;        Electron density altitude profiles on the primary latitude and
;        longitude grids
;     ELAT   Float      Scalar        Degrees north   -90. <= ELAT <= 90.
;        The ending latitude of the primary latitude grid
;     ELON   Float      Scalar        Degrees east    n/a
;        The ending longitude of the primary latitude grid
;     F10P7  Float      Scalar        Solar Flux      >= 0.
;                                     Units
;        The 10.7 cm solar flux
;     FILE   String     Scalar        n/a             File must exist
;        The name of the PIM output file
;     FP     Float      Matrix        MHz             >= 0.
;                       (NALT,NLAT,
;                        NLON)
;        Plasma frequencies on the primary latitude and longitude grids
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
;     IALT   Integer    Scalar        n/a             0 <= IALT <= NALT-1
;        The altitude index
;     IDUM   Integer    Scalar        n/a             n/a
;        A dummy variable used for reading
;     ILAT   Integer    Scalar        n/a             0 <= ILAT <= NLAT-1
;        The primary latitude index
;     ILON   Integer    Scalar        n/a             0 <= ILON <= NLON-1
;        The primary longitude index
;     IMENU  Integer    Scalar        n/a             0 <= IMENU <= 10
;        The menu choice
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
;     LAT    Float      Vector        Degrees north   -90. <= LAT <= 90.
;                       (NLAT)
;        The primary latitude grid
;     LON    Float      Vector        Degrees east    n/a
;                       (NLON)
;        The primary longitude grid
;     LEDP   Float      Matrix        n/a             n/a
;                       (NALT,NLAT,
;                        NLON)
;        The logarithm of the electron density altitude profiles on the
;        primary latitude and longitude grids
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
;     PRFTYP Integer    Scalar        n/a             1 <= PRFTYP <= 3
;        A flag identifying the type of plot to make, 1 for a latitude profile
;        (constant longitude and altitude), 2 for a longitude profile (constant
;        latitude and altitude), or 3 for an altitude profile (constant
;        latitude and longitude)
;     PRMTYP Integer    Scalar        n/a             1= < PRMTYP <= 3
;        A flag identifying the quantity to plot, 1 for electron density,
;        2 for the logarithm of the electron density, or 3 for plasma frequency
;     SCRD1  String     Scalar        n/a             n/a
;        A string identifying the primary coordinate system
;     SCRD2  String     Scalar        n/a             n/a
;        A string identifying the primary coordinate system
;     SDUM   Float      Scalar        n/a             n/a
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
;     UT     Float      Scalar        Decimal hours   0. <= UT < 24.
;        The universal time
;     X      Float      Vector        Varies          Varies
;                       (?)
;        The x-coordinates for a plot
;     XRANGE Float      Vector        Varies          Varies
;                       (2)
;        The x-range for a plot
;     XTITLE String     Scalar        n/a             n/a
;        The x-title for the plot
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
;        The y-title for the plot
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
;     1.5   14-February-1997
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
;                                 Renamed from PIMPCUT.PRO to RECPCUT.PRO.
;                                 Now accepts only rectangular-grid output.
;     L. Brown       24-Jan-1996  1.3 ==> 1.4
;                                 Renamed output grid type "satellite" to
;                                 "latitude/longitude pairs".
;                                 Renamed output grid type
;                                 "radar azimuth/elevation" to
;                                 "azimuth/elevation (ground-based)".
;     L. Brown       14-Feb-1997  1.4 ==> 1.5
;                                 The plasmasphere flag is now encoded in the
;                                 coordinate system type / output grid type
;                                 flag.
;                                 Changed altitude format F6.1 to F7.1 to
;                                 allow for plasmaspheric altitudes.
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
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for RECPCUT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECPCUT
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
IF (DATTYP NE 1) AND (DATTYP NE 2) THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of data for RECPCUT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECPCUT
   ENDELSE
ENDIF
;
;  Read the altitude grid from the PIM output file
;
NALT=0
READF,LUN,NALT,FORMAT='(29X,I6)'
READF,LUN,SDUM
ALT=FLTARR(NALT)
READF,LUN,ALT
;
;  Read electron density profiles from the PIM output file
;
GLAT=FLTARR(NLAT,NLON)
GLON=FLTARR(NLAT,NLON)
MLAT=FLTARR(NLAT,NLON)
MLON=FLTARR(NLAT,NLON)
MLT=FLTARR(NLAT,NLON)
EDP=FLTARR(NALT,NLAT,NLON)
DUM1=FLTARR(5)
DUM2=FLTARR(NALT)
FOR ILAT=0,NLAT-1 DO BEGIN
   FOR ILON=0,NLON-1 DO BEGIN
      READF,LUN,SDUM
      READF,LUN,DUM1
      GLAT(ILAT,ILON)=DUM1(0)
      GLON(ILAT,ILON)=DUM1(1)
      MLAT(ILAT,ILON)=DUM1(2)
      MLON(ILAT,ILON)=DUM1(3)
      MLT(ILAT,ILON)=DUM1(4)
      READF,LUN,SDUM
      READF,LUN,DUM2
      EDP(*,ILAT,ILON)=DUM2
      IF DATTYP EQ 2 THEN BEGIN
         READF,LUN,SDUM
         READF,LUN,SDUM
      ENDIF
   ENDFOR
ENDFOR
;
;  Close the PIM output file
;
CLOSE,LUN
FREE_LUN,LUN
;
;  Convert the electron density to its base 10 logarithm
;
LEDP=ALOG10(EDP)
;
;  Convert the electron density to plasma frequency
;
FP=SQRT(EDP/1.24E4)
;
;  Set the latitude and longitude grids according to the primary coordinate
;  system type
;
IF CRDTYP EQ 0 THEN BEGIN
   LAT=GLAT(*,0)
   LON=REFORM(GLON(0,*))
   SCRD1='geographic'
   SCRD2='Geographic'
ENDIF ELSE BEGIN
   LAT=MLAT(*,0)
   LON=REFORM(MLON(0,*))
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
   IF (IDUM EQ 1) AND (LON(NLON-1) LT LON(NLON-2)) THEN BEGIN
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
      PRINT,'RECPCUT:Primary longitude grid is not monotonically '+ $
            'increasing in one period of'
      PRINT,'        longitude (360 degree range).'
      STOP,'RECPCUT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plot parameters
;
PRFTYP=3
PRMTYP=1
ILAT=0
ILON=0
IALT=0
X=EDP(*,ILAT,ILON)
XRANGE=[MIN(X),MAX(X)]
Y=ALT
YRANGE=[MIN(Y),MAX(Y)]
;
;  Display the menu
;
DISPLAY_MENU:
PRINT,' 1) File: '+FILE
CASE PRFTYP OF
   1   :BEGIN
           PRINT,' 2) Type of profile: Latitude (constant longitude and '+ $
                 'altitude)'
           PRINT,'$(A,F6.1,A)',' 3) Constant longitude: ',LON(ILON), $
                 ' degrees east '+SCRD1
           PRINT,'$(A,F7.1,A)',' 4) Constant altitude: ',ALT(IALT),' km'
           PRINT,'$(A,F5.1,A,F5.1,A)',' 5) Latitude range: ',YRANGE(0), $
                 ' to ',YRANGE(1),' degrees north '+SCRD1
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,' 6) Parameter: Electron density'
                      PRINT,'$(A,E9.2,A,E9.2)', $
                            ' 7) Electron density range: ', $
                            XRANGE(0),' to ',XRANGE(1)
                   END
              2   :BEGIN
                      PRINT,' 6) Parameter: Log10 electron density'
                      PRINT,'$(A,F6.2,A,F6.2)', $
                            ' 7) Log10 electron density range: ', $
                            XRANGE(0),' to ',XRANGE(1)
                   END
              ELSE:BEGIN
                      PRINT,' 6) Parameter: Plasma frequency'
                      PRINT,'$(A,F5.2,A,F5.2)', $
                            ' 7) Plasma frequency range: ', $
                            XRANGE(0),' to ',XRANGE(1)
                   END
           ENDCASE
        END
   2   :BEGIN
           PRINT,' 2) Type of profile: Longitude (constant latitude and '+ $
                 'altitude)'
           PRINT,'$(A,F6.1,A)',' 3) Constant latitude: ',LAT(ILAT), $
                 ' degrees north '+SCRD1
           PRINT,'$(A,F7.1,A)',' 4) Constant altitude: ',ALT(IALT),' km'
           PRINT,'$(A,F6.1,A,F6.1,A)',' 5) Longitude range: ',XRANGE(0), $
                 ' to ',XRANGE(1),' degrees east '+SCRD1
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,' 6) Parameter: Electron density'
                      PRINT,'$(A,E9.2,A,E9.2)', $
                            ' 7) Electron density range: ', $
                            YRANGE(0),' to ',YRANGE(1)
                   END
              2   :BEGIN
                      PRINT,' 6) Parameter: Log10 electron density'
                      PRINT,'$(A,F6.2,A,F6.2)', $
                            ' 7) Log10 electron density range: ', $
                            YRANGE(0),' to ',YRANGE(1)
                   END
              ELSE:BEGIN
                      PRINT,' 6) Parameter: Plasma frequency'
                      PRINT,'$(A,F5.2,A,F5.2)', $
                            ' 7) Plasma frequency range: ', $
                            YRANGE(0),' to ',YRANGE(1)
                   END
           ENDCASE
        END
   ELSE:BEGIN
           PRINT,' 2) Type of profile: Altitude (constant latitude and '+ $
                 'longitude)'
           PRINT,'$(A,F6.1,A)',' 3) Constant latitude: ',LAT(ILAT), $
                 ' degrees north '+SCRD1
           PRINT,'$(A,F6.1,A)',' 4) Constant longitude: ',LON(ILON), $
                 ' degrees east '+SCRD1
           PRINT,'$(A,F7.1,A,F7.1,A)',' 5) Altitude range: ',YRANGE(0), $
                 ' to ',YRANGE(1),' km'
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,' 6) Parameter: Electron density'
                      PRINT,'$(A,E9.2,A,E9.2)', $
                            ' 7) Electron density range: ', $
                            XRANGE(0),' to ',XRANGE(1)
                   END
              2   :BEGIN
                      PRINT,' 6) Parameter: Log10 electron density'
                      PRINT,'$(A,F6.2,A,F6.2)', $
                            ' 7) Log10 electron density range: ', $
                            XRANGE(0),' to ',XRANGE(1)
                   END
              ELSE:BEGIN
                      PRINT,' 6) Parameter: Plasma frequency'
                      PRINT,'$(A,F5.2,A,F5.2)', $
                            ' 7) Plasma frequency range: ', $
                            XRANGE(0),' to ',XRANGE(1)
                   END
           ENDCASE
        END
ENDCASE
PRINT,' 8) Make plot'
PRINT,' 9) Display case parameters
PRINT,'10) Redisplay the menu'
PRINT,' 0) Quit
;
; Get the menu choice
;
IMENU=-1
WHILE (IMENU LT 0) OR (IMENU GT 10) DO BEGIN
   READ,'What is your choice (1-10,0)? ',IMENU
ENDWHILE
;
;  Process the menu choice
;
CASE IMENU OF
   1   :GOTO,GET_FILE
   2   :BEGIN
           PRFTYP=0
           WHILE (PRFTYP LT 1) OR (PRFTYP GT 3) DO BEGIN
              PRINT,'What kind of profile do you want -'
              PRINT,'   (1) Latitude (constant longitude and altitude),'
              PRINT,'   (2) Longitude (constant latitude and altitude ), or'
              PRINT,'   (3) Altitude (constant latitude and longitude)'
              READ,'(1-3)? ',PRFTYP
           ENDWHILE
           CASE PRFTYP OF
              1   :BEGIN
                      CASE PRMTYP OF
                         1   :X=REFORM(EDP(IALT,*,ILON))
                         2   :X=REFORM(LEDP(IALT,*,ILON))
                         ELSE:X=REFORM(FP(IALT,*,ILON))
                      ENDCASE
                      Y=LAT
                   END
              2   :BEGIN
                      X=LON
                      CASE PRMTYP OF
                         1   :Y=REFORM(EDP(IALT,ILAT,*))
                         2   :Y=REFORM(LEDP(IALT,ILAT,*))
                         ELSE:Y=REFORM(FP(IALT,ILAT,*))
                      ENDCASE
                   END
              ELSE:BEGIN
                      CASE PRMTYP OF
                         1   :X=EDP(*,ILAT,ILON)
                         2   :X=LEDP(*,ILAT,ILON)
                         ELSE:X=FP(*,ILAT,ILON)
                      ENDCASE
                      Y=ALT
                   END
           ENDCASE
           XRANGE=[MIN(X),MAX(X)]
           YRANGE=[MIN(Y),MAX(Y)]
        END
   3   :BEGIN
           DUM=0.
           IDUM=0
           CASE PRFTYP OF
              1   :BEGIN
                      PRINT,'Available '+SCRD1+' longitudes are'
                      PRINT,LON,FORMAT='(10F7.1)'
                      READ,'What constant '+SCRD1+' longitude do you want? ', $
                           DUM
                      IDUM=MIN(ABS(LON-DUM))
                      ILON=!C
                      CASE PRMTYP OF
                         1   :X=REFORM(EDP(IALT,*,ILON))
                         2   :X=REFORM(LEDP(IALT,*,ILON))
                         ELSE:X=REFORM(FP(IALT,*,ILON))
                      ENDCASE
                      XRANGE=[MIN(X),MAX(X)]
                   END
              2   :BEGIN
                      PRINT,'Available '+SCRD1+' latitudes are'
                      PRINT,LAT,FORMAT='(10F7.1)'
                      READ,'What constant '+SCRD1+' latitude do you want? ',DUM
                      IDUM=MIN(ABS(LAT-DUM))
                      ILAT=!C
                      CASE PRMTYP OF
                         1   :Y=REFORM(EDP(IALT,ILAT,*))
                         2   :Y=REFORM(LEDP(IALT,ILAT,*))
                         ELSE:Y=REFORM(FP(IALT,ILAT,*))
                      ENDCASE
                      YRANGE=[MIN(Y),MAX(Y)]
                   END
              ELSE:BEGIN
                      PRINT,'Available '+SCRD1+' latitudes are'
                      PRINT,LAT,FORMAT='(10F7.1)'
                      READ,'What constant '+SCRD1+' latitude do you want? ',DUM
                      IDUM=MIN(ABS(LAT-DUM))
                      ILAT=!C
                      CASE PRMTYP OF
                         1   :X=EDP(*,ILAT,ILON)
                         2   :X=LEDP(*,ILAT,ILON)
                         ELSE:X=FP(*,ILAT,ILON)
                      ENDCASE
                      XRANGE=[MIN(X),MAX(X)]
                   END
           ENDCASE
        END
   4   :BEGIN
           DUM=0.
           IDUM=0
           CASE PRFTYP OF
              1   :BEGIN
                      PRINT,'Available altitudes are'
                      PRINT,ALT,FORMAT='(10F7.1)'
                      READ,'What constant altitude do you want? ',DUM
                      IDUM=MIN(ABS(ALT-DUM))
                      IALT=!C
                      CASE PRMTYP OF
                         1   :X=REFORM(EDP(IALT,*,ILON))
                         2   :X=REFORM(LEDP(IALT,*,ILON))
                         ELSE:X=REFORM(FP(IALT,*,ILON))
                      ENDCASE
                      XRANGE=[MIN(X),MAX(X)]
                   END
              2   :BEGIN
                      PRINT,'Available altitudes are'
                      PRINT,ALT,FORMAT='(10F7.1)'
                      READ,'What constant altitude do you want? ',DUM
                      IDUM=MIN(ABS(ALT-DUM))
                      IALT=!C
                      CASE PRMTYP OF
                         1   :Y=REFORM(EDP(IALT,ILAT,*))
                         2   :Y=REFORM(LEDP(IALT,ILAT,*))
                         ELSE:Y=REFORM(FP(IALT,ILAT,*))
                      ENDCASE
                      YRANGE=[MIN(Y),MAX(Y)]
                   END
              ELSE:BEGIN
                      PRINT,'Available '+SCRD1+' longitudes are'
                      PRINT,LON,FORMAT='(10F7.1)'
                      READ,'What constant '+SCRD1+' longitude do you want? ', $
                           DUM
                      IDUM=MIN(ABS(LON-DUM))
                      ILON=!C
                      CASE PRMTYP OF
                         1   :X=EDP(*,ILAT,ILON)
                         2   :X=LEDP(*,ILAT,ILON)
                         ELSE:X=FP(*,ILAT,ILON)
                      ENDCASE
                      XRANGE=[MIN(X),MAX(X)]
                   END
           ENDCASE
        END
   5   :BEGIN
           CASE PRFTYP OF
              1   :BEGIN
                      PRINT,'Available '+SCRD1+' latitudes are'
                      PRINT,LAT,FORMAT='(10F7.1)'
                      READ,'What range of '+SCRD1+' latitude do you want? ', $
                           YRANGE
                   END
              2   :BEGIN
                      PRINT,'Available '+SCRD1+' longitudes are'
                      PRINT,LON,FORMAT='(10F7.1)'
                      READ,'What range of '+SCRD1+' longitude do you want? ', $
                           XRANGE
                   END
              ELSE:BEGIN
                      PRINT,'Available altitudes are'
                      PRINT,ALT,FORMAT='(10F7.1)'
                      READ,'What range of altitude do you want? ',YRANGE
                   END
           ENDCASE
        END
   6   :BEGIN
           PRMTYP=0
           WHILE (PRMTYP LT 1) OR (PRMTYP GT 3) DO BEGIN
              PRINT,'Do you want to plot'
              PRINT,'   (1) electron density,'
              PRINT,'   (2) log10 of electron density, or'
              PRINT,'   (3) plasma frequency'
              READ,'(1-3)? ',PRMTYP
           ENDWHILE
           CASE PRFTYP OF
              1   :BEGIN
                      CASE PRMTYP OF
                         1   :X=REFORM(EDP(IALT,*,ILON))
                         2   :X=REFORM(LEDP(IALT,*,ILON))
                         ELSE:X=REFORM(FP(IALT,*,ILON))
                      ENDCASE
                      XRANGE=[MIN(X),MAX(X)]
                   END
              2   :BEGIN
                      CASE PRMTYP OF
                         1   :Y=REFORM(EDP(IALT,ILAT,*))
                         2   :Y=REFORM(LEDP(IALT,ILAT,*))
                         ELSE:Y=REFORM(FP(IALT,ILAT,*))
                      ENDCASE
                      YRANGE=[MIN(Y),MAX(Y)]
                   END
              ELSE:BEGIN
                      CASE PRMTYP OF
                         1   :X=EDP(*,ILAT,ILON)
                         2   :X=LEDP(*,ILAT,ILON)
                         ELSE:X=FP(*,ILAT,ILON)
                      ENDCASE
                      XRANGE=[MIN(X),MAX(X)]
                   END
           ENDCASE
        END
   7   :BEGIN
           CASE PRFTYP OF
              1   :BEGIN
                      CASE PRMTYP OF
                         1   :BEGIN
                                 PRINT,'$(A,E9.2,A,E9.2,A)', $
                                       'Electron density ranges from ', $
                                       MIN(X),' to ',MAX(X),'.'
                                 READ,'What range of electron density do '+ $
                                      'you want? ',XRANGE
                              END
                         2   :BEGIN
                                 PRINT,'$(A,F6.2,A,F6.2,A)', $
                                       'Log10 electron density ranges from ', $
                                       MIN(X),' to ',MAX(X),'.'
                                 READ,'What range of log10 electron '+ $
                                      'density do you want? ',XRANGE
                              END
                         ELSE:BEGIN
                                 PRINT,'$(A,F5.2,A,F5.2,A)', $
                                       'Plasma frequency ranges from ', $
                                       MIN(X),' to ',MAX(X),'.'
                                 READ,'What range of plasma frequency do '+ $
                                      'you want? ',XRANGE
                              END
                      ENDCASE
                   END
              2   :BEGIN
                      CASE PRMTYP OF
                         1   :BEGIN
                                 PRINT,'$(A,E9.2,A,E9.2,A)', $
                                       'Electron density ranges from ', $
                                       MIN(Y),' to ',MAX(Y),'.'
                                 READ,'What range of electron density do '+ $
                                      'you want? ',YRANGE
                              END
                         2   :BEGIN
                                 PRINT,'$(A,F6.2,A,F6.2,A)', $
                                       'Log10 electron density ranges from ', $
                                       MIN(Y),' to ',MAX(Y),'.'
                                 READ,'What range of log10 electron '+ $
                                      'density do you want? ',YRANGE
                              END
                         ELSE:BEGIN
                                 PRINT,'$(A,F5.2,A,F5.2,A)', $
                                       'Plasma frequency ranges from ', $
                                       MIN(Y),' to ',MAX(Y),'.'
                                 READ,'What range of plasma frequency do '+ $
                                      'you want? ',YRANGE
                              END
                      ENDCASE
                   END
              ELSE:BEGIN
                      CASE PRMTYP OF
                         1   :BEGIN
                                 PRINT,'$(A,E9.2,A,E9.2,A)', $
                                       'Electron density ranges from ', $
                                       MIN(X),' to ',MAX(X),'.'
                                 READ,'What range of electron density do '+ $
                                      'you want? ',XRANGE
                              END
                         2   :BEGIN
                                 PRINT,'$(A,F6.2,A,F6.2,A)', $
                                       'Log10 electron density ranges from ', $
                                       MIN(X),' to ',MAX(X),'.'
                                 READ,'What range of log10 electron '+ $
                                      'density do you want? ',XRANGE
                              END
                         ELSE:BEGIN
                                 PRINT,'$(A,F5.2,A,F5.2,A)', $
                                       'Plasma frequency ranges from ', $
                                       MIN(X),' to ',MAX(X),'.'
                                 READ,'What range of plasma frequency do '+ $
                                      'you want? ',XRANGE
                              END
                      ENDCASE
                   END
           ENDCASE
        END
   8   :BEGIN
           CASE PRFTYP OF
              1   :BEGIN
                      CASE PRMTYP OF
                         1   :BEGIN
                                 XTITLE='Electron Density (cm!u-3!n)'
                                 XTYPE=1
                              END
                         2   :BEGIN
                                 XTITLE='Log!d10!n Electron Density (cm!u-3!n)'
                                 XTYPE=0
                              END
                         ELSE:BEGIN
                                 XTITLE='f!dp!n (MHz)'
                                 XTYPE=0
                              END
                      ENDCASE
                      YTYPE=0
                      YTITLE=SCRD2+' Latitude (!uo!nN)'
                   END
              2   :BEGIN
                      XTITLE=SCRD2+' Longitude (!uo!nE)'
                      XTYPE=0
                      CASE PRMTYP OF
                         1   :BEGIN
                                 YTITLE='Electron Density (cm!u-3!n)'
                                 YTYPE=1
                              END
                         2   :BEGIN
                                 YTITLE='Log!d10!n Electron Density (cm!u-3!n)'
                                 YTYPE=0
                              END
                         ELSE:BEGIN
                                 YTITLE='f!dp!n (MHz)'
                                 YTYPE=1
                              END
                      ENDCASE
                   END
              ELSE:BEGIN
                      CASE PRMTYP OF
                         1   :BEGIN
                                 XTITLE='Electron Density (cm!u-3!n)'
                                 XTYPE=1
                              END
                         2   :BEGIN
                                 XTITLE='Log!d10!n Electron Density (cm!u-3!n)'
                                 XTYPE=0
                              END
                         ELSE:BEGIN
                                 XTITLE='f!dp!n (MHz)'
                                 XTYPE=0
                              END
                      ENDCASE
                      YTITLE='Altitude (km)'
                      YTYPE=0
                   END
           ENDCASE
           PLOT,X,Y,/NORMAL,POSITION=[.125,.125,.675,.925], $
                XSTYLE=5,XTYPE=XTYPE,XRANGE=XRANGE, $
                YSTYLE=5,YTYPE=YTYPE,YRANGE=YRANGE
           AXIS,.125,.100,XAXIS=0,/NORMAL, $
                XSTYLE=1,XRANGE=XRANGE,XTICKS=1,XTITLE=XTITLE
           AXIS,.125,.950,XAXIS=1,/NORMAL, $
                XSTYLE=1,XRANGE=XRANGE,XTICKS=1,XTICKNAME=REPLICATE(' ',2)
           AXIS,.100,.125,YAXIS=0,/NORMAL, $
                YSTYLE=1,YRANGE=YRANGE,YTICKS=1,YTICKLEN=.01, $
                YTITLE=YTITLE
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
           CASE PRFTYP OF
              1   :BEGIN
                      XYOUTS,/NORMAL,SX1,SY1,'Lon: '+ $
                             STRCOMPRESS(STRING(LON(ILON),'(F6.1)'), $
                             /REMOVE_ALL)+' !uo!nE '+SCRD1,SIZE=1.25
                      SY1=SY1-.050
                      XYOUTS,/NORMAL,SX1,SY1,'Alt: '+ $
                             STRCOMPRESS(STRING(ALT(IALT),'(F7.1)'), $
                             /REMOVE_ALL)+' km',SIZE=1.25
                   END
              2   :BEGIN
                      XYOUTS,/NORMAL,SX1,SY1,'Lat: '+ $
                             STRCOMPRESS(STRING(LAT(ILAT),'(F5.1)'), $
                             /REMOVE_ALL)+' !uo!nN '+SCRD1,SIZE=1.25
                      SY1=SY1-.050
                      XYOUTS,/NORMAL,SX1,SY1,'Alt: '+ $
                             STRCOMPRESS(STRING(ALT(IALT),'(F7.1)'), $
                             /REMOVE_ALL)+' km',SIZE=1.25
                   END
              ELSE:BEGIN
                      XYOUTS,/NORMAL,SX1,SY1,'Lat: '+ $
                             STRCOMPRESS(STRING(LAT(ILAT),'(F5.1)'), $
                             /REMOVE_ALL)+' !uo!nN '+SCRD1,SIZE=1.25
                      SY1=SY1-.050
                      XYOUTS,/NORMAL,SX1,SY1,'Lon: '+ $
                             STRCOMPRESS(STRING(LON(ILON),'(F6.1)'), $
                             /REMOVE_ALL)+' !uo!nE '+SCRD1,SIZE=1.25
                   END
           ENDCASE
           SY1=SY1-.080
           IF PLASPH EQ 0 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: None',SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: Gallagher',SIZE=1.25
           ENDELSE
        END
   9   :BEGIN
           PRINT,'--- Case parameters for file '+FILE+' ---'
           PRINT,'$(A,I4)','Year: ',YEAR
           PRINT,'$(A,I3)','Day: ',DAY
           PRINT,'$(A,F5.2,A)','UT: ',UT,' hr'
           PRINT,'$(A,F5.1)','F10.7: ',F10P7
           PRINT,'$(A,F5.1)','SSN: ',SSN
           PRINT,'$(A,F3.1)','Kp: ',KP
           PRINT,'Primary coordinate system: '+SCRD2
           PRINT,'$(A,F5.1,A,F5.1,A,F5.1,A)','Dataset latitude range: ', $
                 MIN(LAT),' to ',MAX(LAT),' in steps of ',DLAT, $
                 ' degrees north '+SCRD1
           PRINT,'$(A,F6.1,A,F6.1,A,F5.1,A)','Dataset longitude range: ', $
                 MIN(LON),' to ',MAX(LON),' in steps of ',DLON, $
                 ' degrees east '+SCRD1
           PRINT,'$(A,F7.1,A,F7.1,A)','Altitude range: ',MIN(ALT),' to ', $
                 MAX(ALT),' km'
           IF DATTYP EQ 1 THEN BEGIN
              PRINT,'Type of data: EDPs'
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
   10  :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_RECPCUT
ENDCASE
GOTO,DISPLAY_MENU
;
END_RECPCUT:
PRINT,'RECPCUT terminated normally.'
;
END
