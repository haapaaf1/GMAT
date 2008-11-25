;Main program AECCRIT
;
;  PURPOSE
;     To plot contours of PIM critical frequencies and heights and TEC vs.
;     azimuth and elevation from azimuth/elevation grid output.
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
;     AZ     Float      Matrix        Degrees         n/a
;                       (NAZ,NEL)
;        Azimuths on the azimuth and elevation grids
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
;     DAZ    Float      Scalar        Degrees         n/a
;        The spacing of the azimuth grid
;     DEL    Float      Scalar        Degrees         n/a
;        The spacing of the elevation grid
;     DUM1   Float      Vector        n/a             n/a
;                       (5)
;        A dummy array used for reading
;     DUM2   Float      Vector        n/a             n/a
;                       (7)
;        A dummy array used for reading
;     EAZ    Float      Scalar        Degrees         n/a
;        The ending azimuth of the azimuth grid
;     EDP    Float      Vector        cm-3            > 0.
;                       (NALT)
;        An electron density altitude profile
;     EEL    Float      Scalar        Degrees         0. <= EEL <= 90.
;        The ending elevation of the elevation grid
;     EL     Float      Matrix        Degrees         0. <= EL <= 90.
;                       (NAZ,NEL)
;        Elevations on the azimuth and elevation grids
;     F10P7  Float      Scalar        Solar Flux      >= 0.
;                                     Units
;        The 10.7 cm solar flux
;     FILE   String     Scalar        n/a             Valid file name
;        The name of the PIM output file
;     FPMAX  Float      Matrix        MHz             >= 0.
;                       (NAZ,NEL)
;        The plasma frequency maximum
;     GRDTYP Integer    Scalar        n/a             0 <= GRDTYP <= 2
;        A flag identifying the type of output grid, 0 for a rectangular
;        latitude/longitude output grid, 1 for a latitude/longitude pairs
;        output grid, or 2 for an azimuth/elevation (ground-based) output grid
;     HMAX   Float      Matrix        Km              >= 0.
;                       (NAZ,NEL)
;        The height of the plasma frequency maximum
;     IDUM   Integer    Scalar        n/a             n/a
;        A dummy variable used for reading
;     IAZ    Integer    Scalar        n/a             0 <= IAZ <= NAZ-1
;        The loop counter for the azimuth grid
;     IEL    Integer    Scalar        n/a             0 <= IEL <= NEL-1
;        The loop counter for the elevation grid
;     IMENU  Integer    Scalar        n/a             1 <= IMENU <= 8
;        The menu choice
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
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
;     OBSLAT Float      Scalar        Degrees north   -90. <= OBSLAT <= 90.
;        The latitude of the observer, in the primary coordinate system
;     OBSLON Float      Scalar        Degrees east    n/a
;        The longitude of the observer, in the primary coordinate system
;     PLASPH Integer    Scalar        n/a             0 or 1
;        The plasmasphere flag, 0 for no plasmasphere or 1 for a Gallagher
;        plasmasphere
;     PRMTYP Integer    Scalar        n/a             1 <= PRMTYP <= 4
;        A flag identifying the quantity to plot, 1 for fpmax, 2 for nmax,
;        3 for hmax, or 4 for TEC
;     SCLEV1 String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the contour
;        levels
;     SCLEV2 String     Scalar        n/a             n/a
;        A string used for annotating the plot that specifies the range of the
;        contour levels
;     SAZ    Float      Scalar        Degrees         n/a
;        The starting azimuth of the azimuth grid
;     SDUM   String     Scalar        n/a             n/a
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
;     TEC    Float      Matrix        TEC Units       >= 0.
;                       (NAZ,NEL)
;        Total electron content
;     UT     Float      Scalar        Decimal hours   0. <= UT < 24.
;        The universal time
;     X      Float      Vector        Degrees         n/a
;                       (NAZ)
;        The azimuth grid for a plot
;     XRANGE Float      Vector        Degrees         n/a
;                       (2)
;        The azimuth range for a plot
;     XTITLE String     Scalar        n/a             n/a
;        The x-axis title
;     Y      Float      Vector        Degrees         0. <= Y <= 90.
;                       (NEL)
;        The elevation grid for a plot
;     YEAR   Integer    Scalar        n/a             n/a
;        The year
;     YRANGE Float      Vector        Degrees         0. <= YRANGE <= 90.
;                       (2)
;        The elevation range for a plot
;     YTITLE String     Scalar        n/a             n/a
;        The y-axis title
;     Z      Float      Matrix        Varies          Varies
;                       (NAZ,NEL)
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
;     1.3   14-February-1997
;
;  MODIFICATIONS
;     ----Person---- ----Date---- -----------------Description-----------------
;     L. Brown       28-Apr-1995  1.0 ==> Created
;     L. Brown       25-Sep-1995  1.0 ==> 1.1
;                                 A header line now precedes the critical
;                                 parameters line in the output file for
;                                 DATTYP=0.
;     L. Brown       24-Jan-1996  1.1 ==> 1.2
;                                 Renamed from RADCCRIT.PRO to AECCRIT.PRO.
;                                 Renamed output grid type "satellite" to
;                                 "latitude/longitude pairs".
;                                 Renamed output grid type
;                                 "radar azimuth/elevation" to
;                                 "azimuth/elevation (ground-based)".
;     L. Brown       14-Feb-1997  1.2 ==> 1.3
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
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for AECCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_AECCRIT
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
IF (DATTYP NE 0) AND (DATTYP NE 2) THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of output data for AECCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_AECCRIT
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
AZ=FLTARR(NAZ,NEL)
EL=FLTARR(NAZ,NEL)
OBSLAT=0.
OBSLON=0.
FPMAX=FLTARR(NAZ,NEL)
HMAX=FLTARR(NAZ,NEL)
TEC=FLTARR(NAZ,NEL)
DUM1=FLTARR(5)
DUM2=FLTARR(7)
FOR IAZ=0,NAZ-1 DO BEGIN
   FOR IEL=0,NEL-1 DO BEGIN
      READF,LUN,SDUM
      READF,LUN,DUM1
      AZ(IAZ,IEL)=DUM1(0)
      EL(IAZ,IEL)=DUM1(1)
      OBSLAT=DUM1(2)
      OBSLON=DUM1(3)
      IF DATTYP EQ 2 THEN BEGIN
         READF,LUN,SDUM
         READF,LUN,EDP
      ENDIF
      READF,LUN,SDUM
      READF,LUN,DUM2
      FPMAX(IAZ,IEL)=DUM2(0)
      HMAX(IAZ,IEL)=DUM2(1)
      TEC(IAZ,IEL)=DUM2(6)
   ENDFOR
ENDFOR
;
;  Close the PIM output file
;
CLOSE,LUN
FREE_LUN,LUN
;
;  Set the azimuth and elevation grids
;
X=AZ(*,0)
Y=REFORM(EL(0,*))
XTITLE='Azimuth (!uo!n)'
YTITLE='Elevation (!uo!n)'
;
;  Attempt to insure that the azimuth grid is monotonic for the CONTOUR routine
;  Note: This is for cases where the user's desired azimuth range for PIM is
;        other than [0,360], for example [-180,180].
;
X=(360.+(X MOD 360.)) MOD 360.
DUM=WHERE(INDGEN(NAZ) EQ SORT(X),IDUM)
IF IDUM LT NAZ THEN BEGIN
   IF (IDUM EQ 1) AND (X(NAZ-1) LE X(NAZ-2)) THEN BEGIN
      X(NAZ-1)=X(NAZ-1)+360.
   ENDIF ELSE BEGIN
      IAZ=0
      WHILE (X(IAZ) LE X(IAZ+1)) AND (IAZ LT NAZ-2) DO BEGIN
         X(IAZ)=X(IAZ)-360.
         IAZ=IAZ+1
      ENDWHILE
      IF IAZ EQ NAZ-2 THEN BEGIN
         IF X(IAZ) LE X(IAZ+1) THEN X(IAZ)=X(IAZ)-360.
      ENDIF ELSE BEGIN
         X(IAZ)=X(IAZ)-360.
      ENDELSE
   ENDELSE
   DUM=WHERE(INDGEN(NAZ) EQ SORT(X),IDUM)
   IF IDUM LT NAZ THEN BEGIN
      PRINT,'AECCRIT:Azimuth grid is not monotonically increasing '+$
            'in one period of'
      PRINT,'         azimuth (360 degree range).'
      STOP,'AECCRIT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plotting parameters
;
PRMTYP=1
Z=FPMAX
LEVELS=INDGEN(30)
C_LABS=INDGEN(30) MOD 2
SPARAM='f!dp,max!n (MHz)
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
   1   :PRINT,'2) Parameter: fpmax'
   2   :PRINT,'2) Parameter: nmax'
   3   :PRINT,'2) Parameter: hmax'
   ELSE:PRINT,'2) Parameter: TEC'
ENDCASE
PRINT,'$(A,F7.2,A,F7.2,A)','3) Azimuth range: ',XRANGE(0),' to ',XRANGE(1), $
      ' degrees'
PRINT,'$(A,F6.2,A,F6.2,A)','4) Elevation range: ',YRANGE(0),' to ',YRANGE(1), $
      ' degrees'
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
           WHILE (PRMTYP LT 1) OR (PRMTYP GT 4) DO BEGIN
              PRINT,'Do you want to plot'
              PRINT,'   (1) fpmax,'
              PRINT,'   (2) nmax,'
              PRINT,'   (3) hmax, or'
              PRINT,'   (4) TEC'
              READ,'(1-4)? ',PRMTYP
           ENDWHILE
           CASE PRMTYP OF
           1   :BEGIN
                   Z=FPMAX
                   LEVELS=INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='f!dp,max!n (MHz)
                   SCLEV1='Contours every 1 MHz'
                   SCLEV2='from 0 to 29 MHz'
                END
           2   :BEGIN
                   Z=1.24E4*FPMAX^2
                   DUM=FINDGEN(9)+1.
                   LEVELS=[DUM*1.E4,DUM*1.E5,DUM*1.E6,1.E7]
                   IDUM=(INDGEN(9)+1) MOD 2
                   C_LABS=[IDUM,IDUM,IDUM,1]
                   SPARAM='n!dmax!n (cm!u-3!n)
                   SCLEV1='Contours at [1,2,...,8,9]*10!un!n cm!u-3!n'
                   SCLEV2='from 10!u4!n to 10!u7!n cm!u-3!n'
                END
           3   :BEGIN
                   Z=HMAX
                   LEVELS=250+10*INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='h!dmax!n (km)
                   SCLEV1='Contours every 10 km'
                   SCLEV2='from 250 to 540 km'
                END
           ELSE:BEGIN
                   Z=TEC
                   LEVELS=5*INDGEN(30)
                   C_LABS=INDGEN(30) MOD 2
                   SPARAM='TEC (TEC Units)'
                   SCLEV1='Contours every 5 TEC Units'
                   SCLEV2='from 0 to 145 TEC Units'
                END
           ENDCASE
        END
   3   :BEGIN
           PRINT,'Available azimuths are'
           PRINT,X,FORMAT='(10F7.1)'
           READ,'What range of azimuth do you want? ',XRANGE
        END
   4   :BEGIN
           PRINT,'Available elevations are'
           PRINT,Y,FORMAT='(10F7.1)'
           READ,'What range of elevation do you want? ',YRANGE
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
           IF CRDTYP EQ 0 THEN BEGIN
              PRINT,'Primary coordinate system: Geographic'
           ENDIF ELSE BEGIN
              PRINT,'Primary coordinate system: Geomagnetic'
           ENDELSE
           PRINT,'$(A,F6.2,A)','Observer latitude: ',OBSLAT,' degrees north'
           PRINT,'$(A,F7.2,A)','Observer longitude: ',OBSLON,' degrees east'
           PRINT,'$(A,F5.1,A,F5.1,A,F5.1,A)','Dataset azimuth range: ', $
                 MIN(X),' to ',MAX(X),' in steps of ',DAZ,' degrees'
           PRINT,'$(A,F6.1,A,F6.1,A,F5.1,A)','Dataset elevation range: ', $
                 MIN(Y),' to ',MAX(Y),' in steps of ',DEL,' degrees'
           IF DATTYP EQ 0 THEN BEGIN
              PRINT,'Type of data: Critical frequencies and heights and TEC'
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
   7   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_AECCRIT
ENDCASE
GOTO,DISPLAY_MENU
;
END_AECCRIT:
PRINT,'AECCRIT terminated normally.'
;
END
