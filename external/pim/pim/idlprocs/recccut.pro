;Main program RECCCUT
;
;  PURPOSE
;     To plot contours of PIM electron density and plasma frequency for cuts in
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
;     CUTTYP Integer    Scalar        n/a             1 <= CUTTYP <= 3
;        A flag identifying the type of plot to make, 1 for a cut in latitude
;        (constant longitude), 2 for a cut in longitude (constant latitude), or
;        3 for a cut in latitude and longitude (constant altitude)
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
;     IDUM   Integer    Scalar        n/a             n/a
;        A dummy variable used for reading
;     ILAT   Integer    Scalar        n/a             0 <= ILAT <= NLAT-1
;        The loop counter for the primary latitude grid
;     ILINE  Integer    Scalar        n/a             Varies
;        The index of the line of constant latitude, longitude, or altitude to
;        be plotted
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
;     LEDP   Float      Matrix        n/a             n/a
;                       (NALT,NLAT,
;                        NLON)
;        The logarithm of the electron density altitude profiles on the
;        primary latitude and longitude grids
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
;     PRMTYP Integer    Scalar        n/a             1= < PRMTYP <= 3
;        A flag identifying the quantity to plot, 1 for electron density,
;        2 for the logarithm of the electron density, or 3 for plasma frequency
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
;     SDUM   Float      Scalar        n/a             n/a
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
;     UT     Float      Scalar        Decimal hours   0. <= UT < 24.
;        The universal time
;     X      Float      Vector        Varies          Varies
;                       (?)
;        The x-grid for a plot
;     XRANGE Float      Vector        Varies          Varies
;                       (2)
;        The x-range for a plot
;     XTITLE String     Scalar        n/a             n/a
;        The x-title for the plot
;     Y      Float      Vector        Varies          Varies
;                       (?)
;        The y-grid for a plot
;     YEAR   Integer    Scalar        n/a             n/a
;        The year
;     YRANGE Float      Vector        Varies          Varies
;                       (2)
;        The y-range for a plot
;     YTITLE String     Scalar        n/a             n/a
;        The y-title for the plot
;     Z      Float      Matrix        Varies          Varies
;                       (NLAT,NLON)
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
;     1.5   14-February-1997
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
;                                 Renamed from PIMCCUT.PRO to RECCCUT.PRO.
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
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for RECCCUT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECCCUT
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
   PRINT,'File "'+FILE+'" contains the wrong type of data for RECCCUT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_RECCCUT
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
;  Attempt to insure that the primary longitude grid is monotonic for the
;  CONTOUR routine
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
      PRINT,'RECCCUT:Primary longitude grid is not monotonically '+ $
            'increasing in one period of'
      PRINT,'        longitude (360 degree range).'
      STOP,'RECCCUT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plot parameters
;
CUTTYP=1
X=LAT
Y=ALT
XTITLE=SCRD2+' Latitude (!uo!nN)'
YTITLE='Altitude (km)'
ILINE=0
XRANGE=[MIN(X),MAX(X)]
YRANGE=[MIN(Y),MAX(Y)]
PRMTYP=1
DUM=FINDGEN(9)+1.
LEVELS=[DUM*1.E4,DUM*1.E5,DUM*1.E6,1.E7]
IDUM=(INDGEN(9)+1) MOD 2
C_LABS=[IDUM,IDUM,IDUM,1]
SPARAM='Electron Density (cm!u-3!n)'
SCLEV1='Contours at [1,2,...,8,9]*10!un!n cm!u-3!n'
SCLEV2='from 10!u4!n to 10!u7!n cm!u-3!n'
;
;  Display the menu
;
DISPLAY_MENU:
PRINT,'1) File: '+FILE
CASE CUTTYP OF
   1   :BEGIN
           PRINT,'2) Type of cut: Through latitude and altitude (constant '+ $
                 'longitude)'
           PRINT,'$(A,F6.1,A)','3) Line of constant longitude: ',LON(ILINE), $
                 ' degrees east '+SCRD1
           PRINT,'$(A,F5.1,A,F5.1,A)','4) Latitude range: ',XRANGE(0),' to ', $
                  XRANGE(1),' degrees north '+SCRD1
           PRINT,'$(A,F7.1,A,F7.1,A)','5) Altitude range: ',YRANGE(0),' to ', $
                 YRANGE(1),' km'
        END
   2   :BEGIN
           PRINT,'2) Type of cut: Through longitude and altitude (constant '+ $
                 'latitude)'
           PRINT,'$(A,F6.1,A)','3) Line of constant latitude: ',LAT(ILINE), $
                 ' degrees north '+SCRD1
           PRINT,'$(A,F6.1,A,F6.1,A)','4) Longitude range: ',XRANGE(0), $
                 ' to ',XRANGE(1),' degrees east '+SCRD1
           PRINT,'$(A,F7.1,A,F7.1,A)','5) Altitude range: ',YRANGE(0),' to ', $
                 YRANGE(1),' km'
        END
   ELSE:BEGIN
           PRINT,'2) Type of cut: Through latitude and longitude (constant '+ $
                 'altitude)'
           PRINT,'$(A,F7.1,A)','3) Constant altitude: ',ALT(ILINE),' km'
           PRINT,'$(A,F5.1,A,F5.1,A)','4) Latitude range: ',YRANGE(0),' to ', $
                 YRANGE(1),' degrees north '+SCRD1
           PRINT,'$(A,F6.1,A,F6.1,A)','5) Longitude range: ',XRANGE(0), $
                 ' to ',XRANGE(1),' degrees east '+SCRD1
        END
ENDCASE
CASE PRMTYP OF
   1   :PRINT,'6) Parameter: Electron density'
   2   :PRINT,'6) Parameter: Log10 electron density'
   ELSE:PRINT,'6) Parameter: Plasma frequency'
ENDCASE
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
           CUTTYP=0
           WHILE (CUTTYP LT 1) OR (CUTTYP GT 3) DO BEGIN
              PRINT,'Do you want to take a cut through'
              PRINT,'   (1) latitude and altitude (constant longitude),'
              PRINT,'   (2) longitude and altitude (constant latitude), or'
              PRINT,'   (3) latitude and longitude (constant altitude)'
              READ,'(1-3)? ',CUTTYP
           ENDWHILE
           ILINE=0
           CASE CUTTYP OF
              1   :BEGIN
                      X=LAT
                      Y=ALT
                      XTITLE=SCRD2+' Latitude (!uo!nN)'
                      YTITLE='Altitude (km)'
                   END
              2   :BEGIN
                      X=LON
                      Y=ALT
                      XTITLE=SCRD2+' Longitude (!uo!nE)'
                      YTITLE='Altitude (km)'
                   END
              ELSE:BEGIN
                      X=LON
                      Y=LAT
                      XTITLE=SCRD2+' Longitude (!uo!nE)'
                      YTITLE=SCRD2+' Latitude (!uo!nN)'
                   END
           ENDCASE
           XRANGE=[MIN(X),MAX(X)]
           YRANGE=[MIN(Y),MAX(Y)]
        END
   3   :BEGIN
           DUM=0.
           IDUM=0
           CASE CUTTYP OF
              1   :BEGIN
                      PRINT,'Available '+SCRD1+' longitudes are'
                      PRINT,LON,FORMAT='(10F7.1)'
                      READ,'What line of constant '+SCRD1+' longitude do '+ $
                           'you want? ',DUM
                      IDUM=MIN(ABS(LON-DUM))
                      ILINE=!C
                   END
              2   :BEGIN
                      PRINT,'Available '+SCRD1+' latitudes are'
                      PRINT,LAT,FORMAT='(10F7.1)'
                      READ,'What line of constant '+SCRD1+' latitude do '+ $
                           'you want? ',DUM
                      IDUM=MIN(ABS(LAT-DUM))
                      ILINE=!C
                   END
              ELSE:BEGIN
                      PRINT,'Available altitudes are'
                      PRINT,ALT,FORMAT='(10F7.1)'
                      READ,'What constant altitude do you want? ',DUM
                      IDUM=MIN(ABS(ALT-DUM))
                      ILINE=!C
                   END
           ENDCASE
        END
   4   :BEGIN
           CASE CUTTYP OF
              1   :BEGIN
                      PRINT,'Available '+SCRD1+' latitudes are'
                      PRINT,LAT,FORMAT='(10F7.1)'
                      READ,'What range of '+SCRD1+' latitude do you want? ', $
                           XRANGE
                   END
              2   :BEGIN
                      PRINT,'Available '+SCRD1+' longitudes are'
                      PRINT,LON,FORMAT='(10F7.1)'
                      READ,'What range of '+SCRD1+' longitude do you want? ', $
                           XRANGE
                   END
              ELSE:BEGIN
                      PRINT,'Available '+SCRD1+' latitudes are'
                      PRINT,LAT,FORMAT='(10F7.1)'
                      READ,'What range of '+SCRD1+' latitude do you want? ', $
                           YRANGE
                   END
           ENDCASE
        END
   5   :BEGIN
           CASE CUTTYP OF
              1   :BEGIN
                      PRINT,'Available altitudes are'
                      PRINT,ALT,FORMAT='(10F7.1)'
                      READ,'What range of altitude do you want? ',YRANGE
                   END
              2   :BEGIN
                      PRINT,'Available altitudes are'
                      PRINT,ALT,FORMAT='(10F7.1)'
                      READ,'What range of altitude do you want? ',YRANGE
                   END
              ELSE:BEGIN
                      PRINT,'Available '+SCRD1+' longitudes are'
                      PRINT,LON,FORMAT='(10F7.1)'
                      READ,'What range of '+SCRD1+' longitude do you want? ', $
                           XRANGE
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
           CASE PRMTYP OF
              1   :BEGIN
                      DUM=FINDGEN(9)+1.
                      LEVELS=[DUM*1.E4,DUM*1.E5,DUM*1.E6,1.E7]
                      IDUM=(INDGEN(9)+1) MOD 2
                      C_LABS=[IDUM,IDUM,IDUM,1]
                      SPARAM='Electron Density (cm!u-3!n)'
                      SCLEV1='Contours at [1,2,...,8,9]*10!un!n cm!u-3!n'
                      SCLEV2='from 10!u4!n to 10!u7!n cm!u-3!n'
                   END
              2   :BEGIN
                      DUM=[1.,2.,5.]
                      LEVELS=ALOG10([DUM,DUM*10.,DUM*1.E2,DUM*1.E3,DUM*1.E4, $
                                     DUM*1.E5,DUM*1.E6,1.E7])
                      IDUM=[1,0,0]
                      C_LABS=[IDUM,IDUM,IDUM,IDUM,IDUM,IDUM,IDUM,1]
                      SPARAM='Log!d10!n Electron Density (cm!u-3!n)'
                      SCLEV1='Contours at log!d10!n ([1,2,5]*10!un!n cm!u-3!n)'
                      SCLEV2='from 1 to 10!u7!n cm!u-3!n'
                   END
              ELSE:BEGIN
                      LEVELS=INDGEN(30)
                      C_LABS=INDGEN(30) MOD 2
                      SPARAM='f!dp!n (MHz)'
                      SCLEV1='Contours every 1 MHz'
                      SCLEV2='from 0 to 29 MHz'
                   END
           ENDCASE
        END
   7   :BEGIN
           CASE CUTTYP OF
              1   :BEGIN
                      CASE PRMTYP OF
                         1   :Z=TRANSPOSE(EDP(*,*,ILINE))
                         2   :Z=TRANSPOSE(LEDP(*,*,ILINE))
                         ELSE:Z=TRANSPOSE(FP(*,*,ILINE))
                      ENDCASE
                   END
              2   :BEGIN
                      Z=FLTARR(NALT,NLON)
                      FOR ILON=0,NLON-1 DO BEGIN
                         CASE PRMTYP OF
                            1   :Z(*,ILON)=EDP(*,ILINE,ILON)
                            2   :Z(*,ILON)=LEDP(*,ILINE,ILON)
                            ELSE:Z(*,ILON)=FP(*,ILINE,ILON)
                         ENDCASE
                      ENDFOR
                      Z=TRANSPOSE(Z)
                   END
              ELSE:BEGIN
                      CASE PRMTYP OF
                         1   :Z=TRANSPOSE(REFORM(EDP(ILINE,*,*)))
                         2   :Z=TRANSPOSE(REFORM(LEDP(ILINE,*,*)))
                         ELSE:Z=TRANSPOSE(REFORM(FP(ILINE,*,*)))
                      ENDCASE
                   END
           ENDCASE
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
           CASE CUTTYP OF
              1   :XYOUTS,/NORMAL,SX1,SY1,'Lon: '+ $
                          STRCOMPRESS(STRING(LON(ILINE),'(F6.1)'), $
                          /REMOVE_ALL)+'!uo!nE '+SCRD1,SIZE=1.25
              2   :XYOUTS,/NORMAL,SX1,SY1,'Lat: '+ $
                          STRCOMPRESS(STRING(LAT(ILINE),'(F5.1)'), $
                          /REMOVE_ALL)+'!uo!nN '+SCRD1,SIZE=1.25
              ELSE:XYOUTS,/NORMAL,SX1,SY1,'Alt: '+ $
                          STRCOMPRESS(STRING(ALT(ILINE),'(F7.1)'), $
                          /REMOVE_ALL)+' km',SIZE=1.25
           ENDCASE
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
   9   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_RECCCUT
ENDCASE
GOTO,DISPLAY_MENU
;
END_RECCCUT:
PRINT,'RECCCUT terminated normally.'
;
END
