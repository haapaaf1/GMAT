;Main program AEPCRIT
;
;  PURPOSE
;     To plot profiles of PIM critical frequencies and heights and TEC vs.
;     azimuth or elevation from azimuth/elevation grid output.
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
;     EL     Float      Vector        Degrees         0. <= EL <= 90.
;                       (NEL)
;        The elevation grid
;     F10P7  Float      Scalar        Solar Flux      >= 0.
;                                     Units
;        The 10.7 cm solar flux
;     FILE   String     Scalar        n/a             File must exist
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
;     IMENU  Integer    Scalar        n/a             0 <= IMENU <= 9
;        The menu choice
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
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
;     PRFTYP Integer    Scalar        n/a             1 <= PRFTYP <= 2
;        A flag identifying the type of plot to make, 1 for an azimuth profile
;        (constant elevation) or 2 for an elevation profile (constant azimuth)
;     PRMTYP Integer    Scalar        n/a             1 <= PRMTYP <= 4
;        A flag identifying the quantity to plot, 1 for fpmax, 2 for nmax,
;        3 for hmax, or 4 for TEC
;     OBSLAT Float      Scalar        Degrees north   -90. <= OBSLAT <= 90.
;        The latitude of the observer, in the primary coordinate system
;     OBSLON Float      Scalar        Degrees east    n/a
;        The longitude of the observer, in the primary coordinate system
;     SAZ    Float      Scalar        Degrees         n/a
;        The starting azimuth of the azimuth grid
;     SDUM   String     Scalar        n/a             n/a
;        A dummy variable used for reading
;     SEL    Float      Scalar        Degrees         0. <= SEL <= 90.
;        The starting elevation of the elevation grid
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
;                                 Renamed from RADPCRIT.PRO to AEPCRIT.PRO.
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
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for AEPCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_AEPCRIT
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
   PRINT,'File "'+FILE+'" contains the wrong type of output data for AEPCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_AEPCRIT
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
AZ=FLTARR(NAZ)
EL=FLTARR(NEL)
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
      AZ(IAZ)=DUM1(0)
      EL(IEL)=DUM1(1)
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
;  Attempt to insure that the azimuth grid is monotonic
;  Note: This is for cases where the user's desired azimuth range for PIM is
;        other than [0,360], for example [-180,180].
;
AZ=(360.+(AZ MOD 360.)) MOD 360.
DUM=WHERE(INDGEN(NAZ) EQ SORT(AZ),IDUM)
IF IDUM LT NAZ THEN BEGIN
   IF (IDUM EQ 1) AND (AZ(NAZ-1) LE AZ(NAZ-2)) THEN BEGIN
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
      PRINT,'AEPCRIT:Azimuth grid is not monotonically increasing '+$
            'in one period of'
      PRINT,'         azimuth (360 degree range).'
      STOP,'AEPCRIT terminated with error.'
   ENDIF
ENDIF
;
;  Initialize plotting parameters
;
PRFTYP=1
PRMTYP=1
IAZ=0
IEL=0
X=AZ
XRANGE=[MIN(X),MAX(X)]
Y=FPMAX(*,IEL)
YRANGE=[MIN(Y),MAX(Y)]
;
;  Display the menu
;
DISPLAY_MENU:
PRINT,'1) File: '+FILE
IF PRFTYP EQ 1 THEN BEGIN
   PRINT,'2) Type of profile: Azimuth (constant elevation)'
   PRINT,'$(A,F5.1,A)','3) Constant elevation: ',EL(IEL),' degrees'
   PRINT,'$(A,F6.1,A,F6.1,A)','4) Azimuth range: ',XRANGE(0),' to ', $
         XRANGE(1),' degrees'
   CASE PRMTYP OF
      1   :BEGIN
              PRINT,'5) Parameter: fpmax'
              PRINT,'$(A,F5.1,A,F5.1)','6) fpmax range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      2   :BEGIN
              PRINT,'5) Parameter: nmax'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmax range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      3   :BEGIN
              PRINT,'5) Parameter: hmax'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmax range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
      ELSE:BEGIN
              PRINT,'5) Parameter: TEC'
              PRINT,'$(A,F5.1,A,F5.1)','6) TEC range: ',YRANGE(0),' to ', $
                    YRANGE(1)
           END
   ENDCASE
ENDIF ELSE BEGIN
   PRINT,'2) Type of profile: Elevation (constant azimuth)'
   PRINT,'$(A,F6.1,A)','3) Constant azimuth: ',AZ(IAZ),' degrees'
   PRINT,'$(A,F5.1,A,F5.1,A)','4) Elevation range: ',YRANGE(0),' to ', $
         YRANGE(1),' degrees'
   CASE PRMTYP OF
      1   :BEGIN
              PRINT,'5) Parameter: fpmax'
              PRINT,'$(A,F5.1,A,F5.1)','6) fpmax range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      2   :BEGIN
              PRINT,'5) Parameter: nmax'
              PRINT,'$(A,E9.2,A,E9.2)','6) nmax range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      3   :BEGIN
              PRINT,'5) Parameter: hmax'
              PRINT,'$(A,F5.1,A,F5.1)','6) hmax range: ',XRANGE(0),' to ', $
                    XRANGE(1)
           END
      ELSE:BEGIN
              PRINT,'5) Parameter: TEC'
              PRINT,'$(A,F5.1,A,F5.1)','6) TEC range: ',XRANGE(0),' to ', $
                    XRANGE(1)
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
              PRINT,'   (1) Azimuth (constant elevation), or'
              PRINT,'   (2) Elevation (constant azimuth)'
              READ,'(1,2)? ',PRFTYP
           ENDWHILE
           IF PRFTYP EQ 1 THEN BEGIN
              X=AZ
              CASE PRMTYP OF
                 1   :Y=FPMAX(*,IEL)
                 2   :Y=1.24E4*(FPMAX(*,IEL)^2)
                 3   :Y=HMAX(*,IEL)
                 ELSE:Y=TEC(*,IEL)
              ENDCASE
           ENDIF ELSE BEGIN
              CASE PRMTYP OF
                 1   :X=REFORM(FPMAX(IAZ,*))
                 2   :X=REFORM(1.24E4*(FPMAX(IAZ,*)^2))
                 3   :X=REFORM(HMAX(IAZ,*))
                 ELSE:X=REFORM(TEC(IAZ,*))
              ENDCASE
              Y=EL
           ENDELSE
           XRANGE=[MIN(X),MAX(X)]
           YRANGE=[MIN(Y),MAX(Y)]
        END
   3   :BEGIN
           DUM=0.
           IDUM=0
           IF PRFTYP EQ 1 THEN BEGIN
              PRINT,'Available elevations are'
              PRINT,EL,FORMAT='(10F7.1)'
              READ,'What constant elevation do you want? ',DUM
              IDUM=MIN(ABS(EL-DUM))
              IEL=!C
              CASE PRMTYP OF
                 1   :Y=FPMAX(*,IEL)
                 2   :Y=1.24E4*(FPMAX(*,IEL)^2)
                 3   :Y=HMAX(*,IEL)
                 ELSE:Y=TEC(*,IEL)
              ENDCASE
              YRANGE=[MIN(Y),MAX(Y)]
           ENDIF ELSE BEGIN
              PRINT,'Available azimuths are'
              PRINT,AZ,FORMAT='(10F7.1)'
              READ,'What constant azimuth do you want? ',DUM
              IDUM=MIN(ABS(AZ-DUM))
              IAZ=!C
              CASE PRMTYP OF
                 1   :X=REFORM(FPMAX(IAZ,*))
                 2   :X=REFORM(1.24E4*(FPMAX(IAZ,*)^2))
                 3   :X=REFORM(HMAX(IAZ,*))
                 ELSE:X=REFORM(TEC(IAZ,*))
              ENDCASE
              XRANGE=[MIN(X),MAX(X)]
           ENDELSE
        END
   4   :IF PRFTYP EQ 1 THEN BEGIN
           PRINT,'Available azimuths are'
           PRINT,AZ,FORMAT='(10F7.1)'
           READ,'What range of azimuth do you want? ',XRANGE
        ENDIF ELSE BEGIN
           PRINT,'Available elevations are'
           PRINT,EL,FORMAT='(10F7.1)'
           READ,'What range of elevation do you want? ',YRANGE
        ENDELSE
   5   :BEGIN
           PRMTYP=0
           WHILE (PRMTYP LT 1) OR (PRMTYP GT 4) DO BEGIN
              PRINT,'Do you want to plot'
              PRINT,'   (1) fpmax,'
              PRINT,'   (2) nmax,'
              PRINT,'   (3) hmax,'
              PRINT,'   (4) TEC'
              READ,'(1-4)? ',PRMTYP
           ENDWHILE
           IF PRFTYP EQ 1 THEN BEGIN
              CASE PRMTYP OF
                 1   :Y=FPMAX(*,IEL)
                 2   :Y=1.24E4*(FPMAX(*,IEL)^2)
                 3   :Y=HMAX(*,IEL)
                 ELSE:Y=TEC(*,IEL)
              ENDCASE
              YRANGE=[MIN(Y),MAX(Y)]
           ENDIF ELSE BEGIN
              CASE PRMTYP OF
                 1   :X=REFORM(FPMAX(IAZ,*))
                 2   :X=REFORM(1.24E4*(FPMAX(IAZ,*)^2))
                 3   :X=REFORM(HMAX(IAZ,*))
                 ELSE:X=REFORM(TEC(IAZ,*))
              ENDCASE
              XRANGE=[MIN(X),MAX(X)]
           ENDELSE
        END
   6   :IF PRFTYP EQ 1 THEN BEGIN
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','fpmax ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of fpmax do you want? ',YRANGE
                   END
              2   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmax ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of nmax do you want? ',YRANGE
                   END
              3   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmax ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of hmax do you want? ',YRANGE
                   END
              ELSE:BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','TEC ranges from ', $
                            MIN(Y),' to ',MAX(Y),'.'
                      READ,'What range of TEC do you want? ',YRANGE
                   END
           ENDCASE
        ENDIF ELSE BEGIN
           CASE PRMTYP OF
              1   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','fpmax ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of fpmax do you want? ',XRANGE
                   END
              2   :BEGIN
                      PRINT,'$(A,E9.2,A,E9.2,A)','nmax ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of nmax do you want? ',XRANGE
                   END
              3   :BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','hmax ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of hmax do you want? ',XRANGE
                   END
              ELSE:BEGIN
                      PRINT,'$(A,F5.1,A,F5.1,A)','TEC ranges from ', $
                            MIN(X),' to ',MAX(X),'.'
                      READ,'What range of TEC do you want? ',XRANGE
                   END
           ENDCASE
        ENDELSE
   7   :BEGIN
           IF PRFTYP EQ 1 THEN BEGIN
              XTITLE='Azimuth (!uo!n)'
              XTYPE=0
              CASE PRMTYP OF
                 1   :BEGIN
                         YTITLE='f!dp,max!n (MHz)'
                         YTYPE=0
                      END
                 2   :BEGIN
                         YTITLE='n!dmax!n (cm!u-3!n)'
                         YTYPE=1
                      END
                 3   :BEGIN
                         YTITLE='h!dmax!n (km)'
                         YTYPE=0
                      END
                 ELSE:BEGIN
                         YTITLE='TEC (TEC Units)'
                         YTYPE=0
                      END
              ENDCASE
           ENDIF ELSE BEGIN
              CASE PRMTYP OF
                 1   :BEGIN
                         XTITLE='f!dp,max!n (MHz)'
                         XTYPE=0
                      END
                 2   :BEGIN
                         XTITLE='n!dmax!n (cm!u-3!n)'
                         XTYPE=1
                      END
                 3   :BEGIN
                         XTITLE='h!dmax!n (km)'
                         XTYPE=0
                      END
                 ELSE:BEGIN
                         XTITLE='TEC (TEC Units)'
                         XTYPE=0
                      END
              ENDCASE
              YTITLE='Elevation (!uo!n)'
              YTYPE=0
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
           IF PRFTYP EQ 1 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'El: '+ $
                     STRCOMPRESS(STRING(EL(IEL),'(F5.1)'), $
                     /REMOVE_ALL)+'!uo!n',SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Az: '+ $
                     STRCOMPRESS(STRING(AZ(IAZ),'(F6.1)'), $
                     /REMOVE_ALL)+'!uo!nN',SIZE=1.25
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
   9   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_AEPCRIT
ENDCASE
GOTO,DISPLAY_MENU
;
END_AEPCRIT:
PRINT,'AEPCRIT terminated normally.'
;
END
