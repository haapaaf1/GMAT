;Main program AECCUT
;
;  PURPOSE
;     To plot contours of PIM electron density and plasma frequency for cuts in
;     azimuth, elevation, or altitude from azimuth/elevation grid output.
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
;     AZ     Float      Vector        Degrees         n/a
;                       (NAZ)
;        The azimuth grid
;     CRDTYP Integer    Scalar        n/a             0 or 1
;        A flag identifying the primary coordinate system used, 0 for
;        geographic or 1 for geomagnetic
;     CUTTYP Integer    Scalar        n/a             1 <= CUTTYP <= 3
;        A flag identifying the type of plot to make, 1 for a cut in azimuth
;        (constant elevation), 2 for a cut in elevation (constant azimuth), or
;        3 for a cut in azimuth and elevation (constant altitude)
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
;     DAZ    Float      Scalar        Degrees         n/a
;        The spacing of the azimuth grid
;     DEL    Float      Scalar        Degrees         n/a
;        The spacing of the elevation grid
;     DUM    Float      Scalar        n/a             n/a
;        An intermediate dummy variable
;     DUM1   Float      Vector        n/a             n/a
;                       (5)
;        A dummy array used for reading
;     DUM2   Float      Vector        n/a             n/a
;                       (NALT)
;        A dummy array used for reading
;     EAZ    Float      Scalar        Degrees         n/a
;        The ending azimuth of the azimuth grid
;     EDP    Float      Matrix        cm-3            > 0.
;                       (NALT,NAZ,
;                        NEL)
;        Electron density altitude profiles on the azimuth and elevation grids
;     EEL    Float      Scalar        Degrees         0. <= EEL <= 90.
;        The ending elevation of the elevation grid
;     EL     Float      Vector        Degrees         0. <= EL <= 90.
;                       (NEL)
;        The elevation grid
;     F10P7  Float      Scalar        Solar Flux      >= 0.
;                                     Units
;        The 10.7 cm solar flux
;     FILE   String     Scalar        n/a             File must exist
;        The name of the PIM output file
;     FP     Float      Matrix        MHz             >= 0.
;                       (NALT,NAZ,
;                        NEL)
;        Plasma frequencies on the azimuth and elevation grids
;     GRDTYP Integer    Scalar        n/a             0 <= GRDTYP <= 2
;        A flag identifying the type of output grid, 0 for a rectangular
;        latitude/longitude output grid, 1 for a latitude/longitude pairs
;        output grid, or 2 for an azimuth/elevation (ground-based) output grid
;     IDUM   Integer    Scalar        n/a             n/a
;        A dummy variable used for reading
;     IAZ    Integer    Scalar        n/a             0 <= IAZ <= NAZ-1
;        The loop counter for the azimuth grid
;     IEL    Integer    Scalar        n/a             0 <= IEL <= NEL-1
;        The loop counter for the elevation grid
;     ILINE  Integer    Scalar        n/a             Varies
;        The index of the line of constant azimuth, elevation, or altitude to
;        be plotted
;     IMENU  Integer    Scalar        n/a             0 <= IMENU <= 9
;        The menu choice
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
;     LEDP   Float      Matrix        n/a             n/a
;                       (NALT,NAZ,
;                        NEL)
;        The logarithm of the electron density altitude profiles on the
;        azimuth and elevation grids
;     LEVELS Float      Vector        Varies          Varies
;                       (?)
;        The contour levels to plot
;     LUN    Long       Scalar        n/a             n/a
;        The logical unit number used to access the PIM output file
;     NALT   Integer    Scalar        n/a             > 0
;        The number of altitudes on the altitude grid
;     NAZ    Integer    Scalar        n/a             > 0
;        The number of azimuths on the azimuth grid
;     NEL    Integer    Scalar        n/a             > 0
;        The number of elevations on the elevation grid
;     PLASPH Integer    Scalar        n/a             0 or 1
;        The plasmasphere flag, 0 for no plasmasphere or 1 for a Gallagher
;        plasmasphere
;     PRMTYP Integer    Scalar        n/a             1= < PRMTYP <= 3
;        A flag identifying the quantity to plot, 1 for electron density,
;        2 for the logarithm of the electron density, or 3 for plasma frequency
;     OBSLAT Float      Scalar        Degrees north   -90. <= OBSLAT <= 90.
;        The latitude of the observer, in the primary coordinate system
;     OBSLON Float      Scalar        Degrees east    n/a
;        The longitude of the observer, in the primary coordinate system
;     SAZ    Float      Scalar        Degrees         n/a
;        The starting azimuth of the azimuth grid
;     SCLEV1 String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the contour
;        levels
;     SCLEV2 String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the range of the
;        contour levels
;     SDUM   Float      Scalar        n/a             n/a
;        A dummy variable used for reading
;     SEL    Float      Scalar        Degrees         0. <= SEL <= 90.
;        The starting elevation of the elevation grid
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
;                       (?,?)
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
;     Varies       Formatted  Varies Azimuth/elevation grid output file from
;                                    PIM
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Waltham, MA 02154  USA
;     (617)-487-2250
;
;  VERSION
;     1.2   14-February-1997
;
;  MODIFICATIONS
;     ----Person---- ----Date---- -----------------Description-----------------
;     L. Brown       28-Apr-1995  1.0 ==> Created
;     L. Brown       24-Jan-1996  1.0 ==> 1.1
;                                 Renamed from RADCCUT.PRO to AECCUT.PRO.
;                                 Renamed output grid type "satellite" to
;                                 "latitude/longitude pairs".
;                                 Renamed output grid type
;                                 "radar azimuth/elevation" to
;                                 "azimuth/elevation (ground-based)".
;     L. Brown       14-Feb-1997  1.1 ==> 1.2
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
READ,'What is the name of the PIM azimuth/elevation grid output file? ',FILE
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
IF GRDTYP NE 2 THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for AECCUT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_AECCUT
   ENDELSE
ENDIF
;
;  Read grid information from the PIM output file
;
SAZ=0.
SEL=0.
EAZ=0.
EEL=0.
NAZ=0
NEL=0
DAZ=0.
DEL=0.
READF,LUN,SDUM
READF,LUN,SDUM
READF,LUN,SAZ,EAZ,SEL,EEL,NAZ,NEL,DAZ,DEL
;
;  Read the output data type from the PIM output file
;
DATTYP=0
READF,LUN,DATTYP
IF (DATTYP NE 1) AND (DATTYP NE 2) THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of data for AECCUT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_AECCUT
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
AZ=FLTARR(NAZ)
EL=FLTARR(NEL)
OBSLAT=0.
OBSLON=0.
EDP=FLTARR(NALT,NAZ,NEL)
DUM1=FLTARR(5)
DUM2=FLTARR(NALT)
FOR IAZ=0,NAZ-1 DO BEGIN
   FOR IEL=0,NEL-1 DO BEGIN
      READF,LUN,SDUM
      READF,LUN,DUM1
      AZ(IAZ)=DUM1(0)
      EL(IEL)=DUM1(1)
      OBSLAT=DUM1(2)
      OBSLON=DUM1(3)
      READF,LUN,SDUM
      READF,LUN,DUM2
      EDP(*,IAZ,IEL)=DUM2
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
;  Attempt to insure that the azimuth grid is monotonic for the CONTOUR routine
;  Note: This is for cases where the user's desired azimuth range for PIM is
;        other than [0,360], for example [-180,180].
;
AZ=(360.+(AZ MOD 360.)) MOD 360.
DUM=WHERE(INDGEN(NAZ) EQ SORT(AZ),IDUM)
IF IDUM LT NAZ THEN BEGIN
   IF (IDUM EQ 1) AND (AZ(NAZ-1) LT AZ(NAZ-2)) THEN BEGIN
      AZ(NAZ-1)=AZ(NAZ-1)+360.
   ENDIF ELSE BEGIN
      IAZ=0
      WHILE (AZ(IAZ) LE AZ(IAZ+1)) AND (IAZ LT NAZ-2) DO BEGIN
         AZ(IAZ)=AZ(IAZ)-360.
         IAZ=IAZ+1
      ENDWHILE
      IF IAZ EQ NAZ-2 THEN BEGIN
         IF AZ(IAZ) LE AZ(IAZ+1) THEN AZ(IAZ)=AZ(IAZ)-360.
      ENDIF ELSE BEGIN
         AZ(IAZ)=AZ(IAZ)-360.
      ENDELSE
   ENDELSE
   DUM=WHERE(INDGEN(NAZ) EQ SORT(AZ),IDUM)
   IF IDUM LT NAZ THEN BEGIN
      PRINT,'AECCUT:Azimuth grid is not monotonically increasing '+ $
            'in one period of'
      PRINT,'        azimuth (360 degree range).'
      STOP,'AECCUT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plot parameters
;
CUTTYP=1
X=AZ
Y=ALT
XTITLE='Azimuth (!uo!n)'
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
           PRINT,'2) Type of cut: Through azimuth and altitude (constant '+ $
                 'elevation)'
           PRINT,'$(A,F5.1,A)','3) Line of constant elevation: ',EL(ILINE), $
                 ' degrees'
           PRINT,'$(A,F6.1,A,F6.1,A)','4) Azimuth range: ',XRANGE(0),' to ', $
                  XRANGE(1),' degrees'
           PRINT,'$(A,F7.1,A,F7.1,A)','5) Altitude range: ',YRANGE(0),' to ', $
                 YRANGE(1),' km'
        END
   2   :BEGIN
           PRINT,'2) Type of cut: Through elevation and altitude (constant '+ $
                 'azimuth)'
           PRINT,'$(A,F6.1,A)','3) Line of constant azimuth: ',AZ(ILINE), $
                 ' degrees'
           PRINT,'$(A,F5.1,A,F5.1,A)','4) Elevation range: ',XRANGE(0), $
                 ' to ',XRANGE(1),' degrees'
           PRINT,'$(A,F7.1,A,F7.1,A)','5) Altitude range: ',YRANGE(0),' to ', $
                 YRANGE(1),' km'
        END
   ELSE:BEGIN
           PRINT,'2) Type of cut: Through azimuth and elevation (constant '+ $
                 'altitude)'
           PRINT,'$(A,F7.1,A)','3) Constant altitude: ',ALT(ILINE),' km'
           PRINT,'$(A,F6.1,A,F6.1,A)','4) Azimuth range: ',XRANGE(0),' to ', $
                 XRANGE(1),' degrees'
           PRINT,'$(A,F5.1,A,F5.1,A)','5) Elevation range: ',YRANGE(0), $
                 ' to ',YRANGE(1),' degrees'
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
              PRINT,'   (1) azimuth and altitude (constant elevation),'
              PRINT,'   (2) elevation and altitude (constant azimuth), or'
              PRINT,'   (3) azimuth and elevation (constant altitude)'
              READ,'(1-3)? ',CUTTYP
           ENDWHILE
           ILINE=0
           CASE CUTTYP OF
              1   :BEGIN
                      X=AZ
                      Y=ALT
                      XTITLE='Azimuth (!uo!n)'
                      YTITLE='Altitude (km)'
                   END
              2   :BEGIN
                      X=EL
                      Y=ALT
                      XTITLE='Elevation (!uo!n)'
                      YTITLE='Altitude (km)'
                   END
              ELSE:BEGIN
                      X=AZ
                      Y=EL
                      XTITLE='Azimuth (!uo!n)'
                      YTITLE='Elevation (!uo!n)'
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
                      PRINT,'Available elevations are'
                      PRINT,EL,FORMAT='(10F7.1)'
                      READ,'What line of constant elevation do you want? ',DUM
                      IDUM=MIN(ABS(EL-DUM))
                      ILINE=!C
                   END
              2   :BEGIN
                      PRINT,'Available azimuths are'
                      PRINT,AZ,FORMAT='(10F7.1)'
                      READ,'What line of constant azimuth do you want? ',DUM
                      IDUM=MIN(ABS(AZ-DUM))
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
                      PRINT,'Available azimuths are'
                      PRINT,AZ,FORMAT='(10F7.1)'
                      READ,'What range of azimuth do you want? ',XRANGE
                   END
              2   :BEGIN
                      PRINT,'Available elevations are'
                      PRINT,EL,FORMAT='(10F7.1)'
                      READ,'What range of elevation do you want? ',XRANGE
                   END
              ELSE:BEGIN
                      PRINT,'Available azimuths are'
                      PRINT,AZ,FORMAT='(10F7.1)'
                      READ,'What range of azimuth do you want? ',XRANGE
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
                      PRINT,'Available elevations are'
                      PRINT,EL,FORMAT='(10F7.1)'
                      READ,'What range of elevation do you want? ',YRANGE
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
                      Z=FLTARR(NALT,NEL)
                      FOR IEL=0,NEL-1 DO BEGIN
                         CASE PRMTYP OF
                            1   :Z(*,IEL)=EDP(*,ILINE,IEL)
                            2   :Z(*,IEL)=LEDP(*,ILINE,IEL)
                            ELSE:Z(*,IEL)=FP(*,ILINE,IEL)
                         ENDCASE
                      ENDFOR
                      Z=TRANSPOSE(Z)
                   END
              ELSE:BEGIN
                      CASE PRMTYP OF
                         1   :Z=REFORM(EDP(ILINE,*,*))
                         2   :Z=REFORM(LEDP(ILINE,*,*))
                         ELSE:Z=REFORM(FP(ILINE,*,*))
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
           IF CRDTYP EQ 0 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Coord. System: Geographic',SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Coord. System: Geomagnetic',SIZE=1.25
           ENDELSE
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'Obs. lat.: '+STRCOMPRESS(STRING(OBSLAT, $
                  '(F6.2)'),/REMOVE_ALL)+'!uo!nN',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'Obs. lon.: '+STRCOMPRESS(STRING(OBSLON, $
                  '(F7.2)'),/REMOVE_ALL)+'!uo!nE',SIZE=1.25
           SY1=SY1-.080
           CASE CUTTYP OF
              1   :XYOUTS,/NORMAL,SX1,SY1,'El: '+ $
                          STRCOMPRESS(STRING(EL(ILINE),'(F5.1)'), $
                          /REMOVE_ALL)+'!uo!n',SIZE=1.25
              2   :XYOUTS,/NORMAL,SX1,SY1,'Az: '+ $
                          STRCOMPRESS(STRING(AZ(ILINE),'(F6.1)'), $
                          /REMOVE_ALL)+'!uo!n',SIZE=1.25
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
           IF CRDTYP EQ 0 THEN BEGIN
              PRINT,'Primary coordinate system: Geographic'
           ENDIF ELSE BEGIN
              PRINT,'Primary coordinate system: Geomagnetic'
           ENDELSE
           PRINT,'$(A,F6.2,A)','Observer latitude: ',OBSLAT,' degrees north'
           PRINT,'$(A,F7.2,A)','Observer longitude: ',OBSLON,' degrees east'
           PRINT,'$(A,F5.1,A,F5.1,A,F5.1,A)','Dataset azimuth range: ', $
                 MIN(AZ),' to ',MAX(AZ),' in steps of ',DAZ,' degrees'
           PRINT,'$(A,F6.1,A,F6.1,A,F5.1,A)','Dataset elevation range: ', $
                 MIN(EL),' to ',MAX(EL),' in steps of ',DEL,' degrees'
           IF DATTYP EQ 1 THEN BEGIN
              PRINT,'Type of data: EDPs'
           ENDIF ELSE BEGIN
              PRINT,'Type of data: EDPs, critical frequencies and heights, ', $
                    'and TEC'
           ENDELSE
           PRINT,'Type of grid: Azimuth/elevation'
           IF PLASPH GT 0 THEN BEGIN
              PRINT,'Plasmasphere: Gallagher'
           ENDIF ELSE BEGIN
              PRINT,'Plasmasphere: None'
           ENDELSE
           PRINT,'--- Press <ENTER> to continue ---'
           SDUM=GET_KBRD(1)
        END
   9   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_AECCUT
ENDCASE
GOTO,DISPLAY_MENU
;
END_AECCUT:
PRINT,'AECCUT terminated normally.'
;
END
