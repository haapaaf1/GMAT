;Main program LLPPCRIT
;
;  PURPOSE
;     To plot profiles of PIM critical frequencies and heights and TEC from
;     latitude/longitude pairs grid output.
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
;        The ending latitude of the latitude/longitude pairs output grid
;     ELON   Float      Scalar        Degrees east    n/a
;        The ending longitude of the latitude/longitude pairs output grid
;     F10P7  Float      Scalar        Solar Flux      >= 0.
;                                     Units
;        The 10.7 cm solar flux
;     FILE   String     Scalar        n/a             File must exist
;        The name of the PIM output file
;     FOE    Float      Vector        MHz             >= 0.
;                       (NPR)
;        The E-layer critical frequency
;     FOF1   Float      Vector        MHz             >= 0.
;                       (NPR)
;        The F1-layer critical frequency
;     FOF2   Float      Vector        MHz             >= 0.
;                       (NPR)
;        The F2-layer critical frequency
;     GLAT   Float      Vector        Degrees north   -90. <= GLAT <= 90.
;                       (NPR)
;        Geographic latitudes on the latitude/longitude pairs output grid
;     GLON   Float      Vector        Degrees east    n/a
;                       (NPR)
;        Geographic longitudes on the latitude/longitude pairs output grid
;     GRDTYP Integer    Scalar        n/a             0 <= GRDTYP <= 2
;        A flag identifying the type of output grid, 0 for a rectangular
;        latitude/longitude output grid, 1 for a latitude/longitude pairs
;        output grid, or 2 for an azimuth/elevation (ground-based) output grid
;     HME    Float      Vector        Km              >= 0.
;                       (NPR)
;        The E-layer critical height
;     HMF1   Float      Vector        Km              >= 0.
;                       (NPR)
;        The F1-layer critical height
;     HMF2   Float      Vector        Km              >= 0.
;                       (NPR)
;        The F2-layer critical height
;     IDUM   Integer    Scalar        n/a             n/a
;        A dummy variable used for reading
;     IMENU  Integer    Scalar        n/a             1 <= IMENU <= 9
;        The menu choice
;     IPR    Integer    Scalar        n/a             0 <= IPR <= NPR-1
;        The loop counter for the latitude/longitude pairs output grid points
;     IPRE   Integer    Scalar        n/a             IPRS < IPRE <= NPR-1
;        The starting latitude/longitude pairs output grid index for a plot
;     IPRS   Integer    Scalar        n/a             0 <= IPRS < IPRE
;        The starting latitude/longitude pairs output grid index for a plot
;     KP     Float      Scalar        n/a             0. <= Kp <= 9.
;        The 3-hour magnetic Kp index
;     LUN    Long       Scalar        n/a             n/a
;        The logical unit number used to access the PIM output file
;     MLAT   Float      Vector        Degrees north   -90. <= MLAT <= 90.
;                       (NPR)
;        Geomagnetic latitudes on the latitude/longitude pairs output grid
;     MLON   Float      Vector        Degrees east    n/a
;                       (NPR)
;        Geomagnetic longitudes on the latitude/longitude pairs output grid
;     MLT    Float      Vector        Decimal hours   0. <= MLT < 24.
;                       (NPR)
;        Geomagnetic local times on the latitude/longitude pairs output grid
;     NALT   Integer    Scalar        n/a             > 0
;        The number of altitudes on the altitude grid
;     NPR    Integer    Scalar        n/a             > 0
;        The number of latitude/longitude pairs output grid points
;     PLASPH Integer    Scalar        n/a             0 or 1
;        The plasmasphere flag, 0 for no plasmasphere or 1 for a Gallagher
;        plasmasphere
;     PRMTYP Integer    Scalar        n/a             1 <= PRMTYP <= 10
;        A flag identifying the quantity to plot, 1 for foF2, 2 for nmF2,
;        3 for hmF2, 4 for foF1, 5 for nmF1, 6 for hmF1, 7 for foE, 8 for nmE,
;        9 for hmE, or 10 for TEC
;     SDUM   String     Scalar        n/a             n/a
;        A dummy variable used for reading
;     SLAT   Float      Scalar        Degrees north   -90. <= SLAT <= 90.
;        The starting latitude of the latitude/longitude pairs output grid
;     SLON   Float      Scalar        Degrees east    n/a
;        The starting longitude of the latitude/longitude pairs output grid
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
;     TEC    Float      Vector        TEC Units       >= 0.
;                       (NPR)
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
;     Varies       Formatted  Varies Latitude/longitude pairs grid output file
;                                    from PIM
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
;                                 Renamed from SATPCRIT.PRO to LLPPCRIT.PRO.
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
READ,'What is the name of the PIM latitude/longitude pairs output file? ',FILE
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
IF GRDTYP NE 1 THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of output grid for LLPPCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_LLPPCRIT
   ENDELSE
ENDIF
;
;  Read grid information from the PIM output file
;
SLAT=0.
SLON=0.
ELAT=0.
ELON=0.
NPR=0
READF,LUN,SDUM
READF,LUN,SDUM
READF,LUN,SLAT,ELAT,SLON,ELON,NPR
;
;  Read the output data type from the PIM output file
;
DATTYP=0
READF,LUN,DATTYP
IF (DATTYP NE 0) AND (DATTYP NE 2) THEN BEGIN
   CLOSE,LUN
   FREE_LUN,LUN
   PRINT,'File "'+FILE+'" contains the wrong type of output data for LLPPCRIT.'
   READ,'Do you want to try another file (Y/[N])? ',SDUM
   IF STRUPCASE(STRMID(STRCOMPRESS(SDUM,/REMOVE_ALL),0,1)) EQ 'Y' THEN BEGIN
      GOTO,GET_FILE
   ENDIF ELSE BEGIN
      GOTO,END_LLPPCRIT
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
GLAT=FLTARR(NPR)
GLON=FLTARR(NPR)
MLAT=FLTARR(NPR)
MLON=FLTARR(NPR)
MLT=FLTARR(NPR)
FOF2=FLTARR(NPR)
HMF2=FLTARR(NPR)
FOF1=FLTARR(NPR)
HMF1=FLTARR(NPR)
FOE=FLTARR(NPR)
HME=FLTARR(NPR)
TEC=FLTARR(NPR)
DUM1=FLTARR(5)
DUM2=FLTARR(7)
FOR IPR=0,NPR-1 DO BEGIN
   READF,LUN,SDUM
   READF,LUN,DUM1
   GLAT(IPR)=DUM1(0)
   GLON(IPR)=DUM1(1)
   MLAT(IPR)=DUM1(2)
   MLON(IPR)=DUM1(3)
   MLT(IPR)=DUM1(4)
   IF DATTYP EQ 2 THEN BEGIN
      READF,LUN,SDUM
      READF,LUN,EDP
   ENDIF
   READF,LUN,SDUM
   READF,LUN,DUM2
   FOF2(IPR)=DUM2(0)
   HMF2(IPR)=DUM2(1)
   FOF1(IPR)=DUM2(2)
   HMF1(IPR)=DUM2(3)
   FOE(IPR)=DUM2(4)
   HME(IPR)=DUM2(5)
   TEC(IPR)=DUM2(6)
ENDFOR
;
;  Close the PIM output file
;
CLOSE,LUN
FREE_LUN,LUN
;
;  Initialize plotting parameters
;
PRMTYP=1
IPRS=0
IPRE=NPR-1
X=IPRS+INDGEN(IPRE-IPRS+1)
XRANGE=[MIN(X),MAX(X)]
XTITLE='Grid index'
XTYPE=0
Y=FOF2(IPRS:IPRE)
YRANGE=[MIN(Y),MAX(Y)]
;
;  Display the menu
;
DISPLAY_MENU:
PRINT,'1) File: '+FILE
PRINT,'$(A,I4,A)','2) Starting grid point: ',IPRS
PRINT,'$(A,I4,A)','3) Ending grid point: ',IPRE
CASE PRMTYP OF
   1   :BEGIN
           PRINT,'4) Parameter: foF2'
           PRINT,'$(A,F5.1,A,F5.1)','5) foF2 range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   2   :BEGIN
           PRINT,'4) Parameter: nmF2'
           PRINT,'$(A,E9.2,A,E9.2)','5) nmF2 range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   3   :BEGIN
           PRINT,'4) Parameter: hmF2'
           PRINT,'$(A,F5.1,A,F5.1)','5) hmF2 range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   4   :BEGIN
           PRINT,'4) Parameter: foF1'
           PRINT,'$(A,F5.1,A,F5.1)','5) foF1 range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   5   :BEGIN
           PRINT,'4) Parameter: nmF1'
           PRINT,'$(A,E9.2,A,E9.2)','5) nmF1 range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   6   :BEGIN
           PRINT,'4) Parameter: hmF1'
           PRINT,'$(A,F5.1,A,F5.1)','5) hmF1 range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   7   :BEGIN
           PRINT,'4) Parameter: foE'
           PRINT,'$(A,F5.1,A,F5.1)','5) foE range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   8   :BEGIN
           PRINT,'4) Parameter: nmE'
           PRINT,'$(A,E9.2,A,E9.2)','5) nmE range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   9   :BEGIN
           PRINT,'4) Parameter: hmE'
           PRINT,'$(A,F5.1,A,F5.1)','5) hmE range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
   ELSE:BEGIN
           PRINT,'4) Parameter: TEC'
           PRINT,'$(A,F5.1,A,F5.1)','5) TEC range: ',YRANGE(0),' to ', $
                 YRANGE(1)
        END
ENDCASE
PRINT,'6) Make plot'
PRINT,'7) Display case parameters
PRINT,'8) Redisplay the menu'
PRINT,'9) Quit
;
; Get the menu choice
;
IMENU=-1
WHILE (IMENU LT 1) OR (IMENU GT 9) DO BEGIN
   READ,'What is your choice (1-9)? ',IMENU
ENDWHILE
;
;  Process the menu choice
;
CASE IMENU OF
   1   :GOTO,GET_FILE
   2   :BEGIN
           NROW=20
           NPAGE=NPR/NROW
           IF NROW*NPAGE LT NPR THEN NPAGE=NPAGE+1
           IPAGE=0
           ANS='Y'
           WHILE (ANS EQ 'Y') AND (IPAGE LT NPAGE) DO BEGIN
              PRINT,'Index GLat   GLon  MLat   MLon  MLT'
              PRINT,'      DegN   DegE  DegN   DegE   Hr'
              PRINT,'----- ----- ------ ----- ------ ----'
              FOR IPR=NROW*IPAGE,MIN([NROW*(IPAGE+1)-1,NPR-1]) DO $
                 PRINT,'$(I5,1X,F5.1,1X,F6.1,1X,F5.1,1X,F6.1,1X,F4.1)',IPR, $
                       GLAT(IPR),GLON(IPR),MLAT(IPR),MLON(IPR),MLT(IPR)
              IPAGE=IPAGE+1
              IF IPAGE LT NPAGE THEN  $
                 READ,'Do you want to see more grid points (Y/[N])? ',ANS
           ENDWHILE
           IPRS=-1
           WHILE (IPRS LT 0) OR (IPRS GE NPR) DO BEGIN
              READ,'What is the starting grid index? ',IPRS
           ENDWHILE
           IF IPRE LT IPRS THEN IPRE=IPRS
           X=IPRS+INDGEN(IPRE-IPRS+1)
           XRANGE=[MIN(X),MAX(X)]
           CASE PRMTYP OF
              1   :Y=FOF2(IPRS:IPRE)
              2   :Y=1.24E4*(FOF2(IPRS:IPRE)^2)
              3   :Y=HMF2(IPRS:IPRE)
              4   :Y=FOF1(IPRS:IPRE)
              5   :Y=1.24E4*(FOF1(IPRS:IPRE)^2)
              6   :Y=HMF1(IPRS:IPRE)
              7   :Y=FOE(IPRS:IPRE)
              8   :Y=1.24E4*(FOE(IPRS:IPRE)^2)
              9   :Y=HME(IPRS:IPRE)
              ELSE:Y=TEC(IPRS:IPRE)
           ENDCASE
           YRANGE=[MIN(Y),MAX(Y)]
        END
   3   :BEGIN
           NROW=20
           NPAGE=NPR/NROW
           IF NROW*NPAGE LT NPR THEN NPAGE=NPAGE+1
           IPAGE=0
           ANS='Y'
           WHILE (ANS EQ 'Y') AND (IPAGE LT NPAGE) DO BEGIN
              PRINT,'Index GLat   GLon  MLat   MLon  MLT'
              PRINT,'      DegN   DegE  DegN   DegE   Hr'
              PRINT,'----- ----- ------ ----- ------ ----'
              FOR IPR=NROW*IPAGE,MIN([NROW*(IPAGE+1)-1,NPR-1]) DO $
                 PRINT,'$(I5,1X,F5.1,1X,F6.1,1X,F5.1,1X,F6.1,1X,F4.1)',IPR, $
                       GLAT(IPR),GLON(IPR),MLAT(IPR),MLON(IPR),MLT(IPR)
              IPAGE=IPAGE+1
              IF IPAGE LT NPAGE THEN  $
                 READ,'Do you want to see more grid points (Y/[N])? ',ANS
           ENDWHILE
           IPRE=-1
           WHILE (IPRE LT 0) OR (IPRE GE NPR) DO BEGIN
              READ,'What is the ending grid index? ',IPRE
           ENDWHILE
           IF IPRS GT IPRE THEN IPRS=IPRE
           X=IPRS+INDGEN(IPRE-IPRS+1)
           XRANGE=[MIN(X),MAX(X)]
           CASE PRMTYP OF
              1   :Y=FOF2(IPRS:IPRE)
              2   :Y=1.24E4*(FOF2(IPRS:IPRE)^2)
              3   :Y=HMF2(IPRS:IPRE)
              4   :Y=FOF1(IPRS:IPRE)
              5   :Y=1.24E4*(FOF1(IPRS:IPRE)^2)
              6   :Y=HMF1(IPRS:IPRE)
              7   :Y=FOE(IPRS:IPRE)
              8   :Y=1.24E4*(FOE(IPRS:IPRE)^2)
              9   :Y=HME(IPRS:IPRE)
              ELSE:Y=TEC(IPRS:IPRE)
           ENDCASE
           YRANGE=[MIN(Y),MAX(Y)]
        END
   4   :BEGIN
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
              1   :Y=FOF2(IPRS:IPRE)
              2   :Y=1.24E4*(FOF2(IPRS:IPRE)^2)
              3   :Y=HMF2(IPRS:IPRE)
              4   :Y=FOF1(IPRS:IPRE)
              5   :Y=1.24E4*(FOF1(IPRS:IPRE)^2)
              6   :Y=HMF1(IPRS:IPRE)
              7   :Y=FOE(IPRS:IPRE)
              8   :Y=1.24E4*(FOE(IPRS:IPRE)^2)
              9   :Y=HME(IPRS:IPRE)
              ELSE:Y=TEC(IPRS:IPRE)
           ENDCASE
           YRANGE=[MIN(Y),MAX(Y)]
        END
   5   :BEGIN
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
        END
   6   :BEGIN
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
           XYOUTS,/NORMAL,SX1,SY1,'Starting grid index: '+ $
                  STRCOMPRESS(STRING(IPRS,'(I4)'), $
                  /REMOVE_ALL),SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  GLat: '+ $
                  STRCOMPRESS(STRING(GLAT(IPRS),'(F5.1)'), $
                  /REMOVE_ALL)+'!uo!nN',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  GLon: '+ $
                  STRCOMPRESS(STRING(GLON(IPRS),'(F6.1)'), $
                  /REMOVE_ALL)+'!uo!nE',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  MLat: '+ $
                  STRCOMPRESS(STRING(MLAT(IPRS),'(F5.1)'), $
                  /REMOVE_ALL)+'!uo!nN',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  MLon: '+ $
                  STRCOMPRESS(STRING(MLON(IPRS),'(F6.1)'), $
                  /REMOVE_ALL)+'!uo!nE',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  MLT: '+ $
                  STRCOMPRESS(STRING(MLT(IPRS),'(F4.1)'), $
                  /REMOVE_ALL)+' hr',SIZE=1.25
           SY1=SY1-.080
           XYOUTS,/NORMAL,SX1,SY1,'Ending grid index: '+ $
                  STRCOMPRESS(STRING(IPRE,'(I4)'), $
                  /REMOVE_ALL),SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  GLat: '+ $
                  STRCOMPRESS(STRING(GLAT(IPRE),'(F5.1)'), $
                  /REMOVE_ALL)+'!uo!nN',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  GLon: '+ $
                  STRCOMPRESS(STRING(GLON(IPRE),'(F6.1)'), $
                  /REMOVE_ALL)+'!uo!nE',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  MLat: '+ $
                  STRCOMPRESS(STRING(MLAT(IPRE),'(F5.1)'), $
                  /REMOVE_ALL)+'!uo!nN',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  MLon: '+ $
                  STRCOMPRESS(STRING(MLON(IPRE),'(F6.1)'), $
                  /REMOVE_ALL)+'!uo!nE',SIZE=1.25
           SY1=SY1-.050
           XYOUTS,/NORMAL,SX1,SY1,'  MLT: '+ $
                  STRCOMPRESS(STRING(MLT(IPRE),'(F4.1)'), $
                  /REMOVE_ALL)+' hr',SIZE=1.25
           SY1=SY1-.080
           IF PLASPH EQ 0 THEN BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: None',SIZE=1.25
           ENDIF ELSE BEGIN
              XYOUTS,/NORMAL,SX1,SY1,'Plasmasphere: Gallagher',SIZE=1.25
           ENDELSE
        END
   7   :BEGIN
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
           IF DATTYP EQ 0 THEN BEGIN
              PRINT,'Type of data: Critical frequencies and heights and TEC'
           ENDIF ELSE BEGIN
              PRINT,'Type of data: EDPs, critical frequencies and heights, ', $
                    'and TEC'
           ENDELSE
           PRINT,'Type of grid: Latitude/longitude pairs'
           IF PLASPH GT 0 THEN BEGIN
              PRINT,'Plasmasphere: Gallagher'
           ENDIF ELSE BEGIN
              PRINT,'Plasmasphere: None'
           ENDELSE
           PRINT,'--- Press <ENTER> to continue ---'
           SDUM=GET_KBRD(1)
        END
   8   :GOTO,DISPLAY_MENU
   ELSE:GOTO,END_LLPPCRIT
ENDCASE
GOTO,DISPLAY_MENU
;
END_LLPPCRIT:
PRINT,'LLPPCRIT terminated normally.'
;
END
