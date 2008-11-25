      SUBROUTINE OUTPUT(YEAR,DAY,UT,OUTTYP,GRDTYP,OUTFILE,LUTEXT,ANS)
C
C  PURPOSE
C     To output the final results of the model.
C
C  METHOD
C     Call GRID_OUTPUT, PRS_OUTPUT, or AZEL_OUTPUT for the data on the final
C     grid.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY     description
C    ANS     CHARACTER*1          Holds user response to a menu question.
C    DAY     INTEGER              The day of the year.
C    GRDTYP  INTEGER              The switch for the type of output grid:
C                                 [0] Rectangular latitude/longitude grid
C                                 [1] Latitude/longitude pairs
C                                 [2] Azimuth/elevation (ground-based)
C    LUD     INTEGER              Logical unit number of direct data file
C    LUC1    INTEGER              Logical unit number of first calculated data
C                                 file
C    LUSTAT  INTEGER              Logical unit number of file to output
C                                 station results
C    LUTEXT  INTEGER              Logical unit number of output file
C    LUOUT   INTEGER              Logical unit number of station file
C    MLAT    REAL                 Corrected Geomagnetic latitude
C    MLT     REAL                 Corrected Geomagnetic local time
C    OUTFILE CHARACTER*32         The name of the output grid file.
C    OUTTYP  INTEGER              The switch for type of output:
C                                 [0] Critical frequencies and heights and
C                                     vertical TEC
C                                 [1] Vertical EDPs
C                                 [2] Vertical EDPs, critical frequencies and
C                                     heights, and vertical TEC
C                                 [3] No output
C    UT      REAL                 Universal time in seconds
C    YEAR    INTEGER              The calendar year
C
C  OUTPUT PARAMETERS
C    NONE
C
C  LOCAL VARIABLES
C    NONE
C
C  SUBROUTINES CALLED
C    NAME            description
C    GRID_OUTPUT     does the grid for the output
C    AZEL_OUTPUT     Azimuth/elevation (ground-based) output
C    PRS_OUTPUT      Latitude/longitude pairs output
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     William Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   23-January-1996
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby     1-May-1990   1.0.6
C     L. Brown        18-Nov-1994   1.0.6 ==> 1.1
C                                   Input parameter IGRIDSW has been renamed to
C                                   OUTTYP.
C                                   Input parameter GRDTYP has been added.
C                                   If satellite track output has been
C                                   requested, then routine SAT_OUTPUT is
C                                   called instead of routine GRID_OUTPUT.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   The output grid type can now be 2, for
C                                   radar azimuth/elevation.  Output for this
C                                   output grid type is produced by a call to
C                                   routine RADAR_OUTPUT.
C                                   The description of input parameter GRDTYP
C                                   has been modified to include the radar
C                                   output grid type.
C     L. Brown        23-Jan-1996   1.2 ==> 1.4
C                                   Changed name of output grid type
C                                   "satellite track" to
C                                   "latitude/longitude pairs".
C                                   Renamed routine SAT_OUTPUT to PRS_OUTPUT.
C                                   Changed name of output grid type
C                                   "radar azimuth/elevation" to
C                                   "azimuth/elevation (ground-based)".
C                                   Renamed routine RADAR_OUTPUT to
C                                   AZEL_OUTPUT.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER YEAR,DAY,OUTTYP,GRDTYP
      REAL UT
      INTEGER LUTEXT
      CHARACTER*32 OUTFILE
      CHARACTER*1 ANS
C
      IF ( ANS .EQ. 'N') THEN
C      If there were any indirect parameters for this run initialize
C      them.
C
       IF (OUTTYP .LT. 3) THEN
C
C       Do the final output grid.
C
        OPEN(UNIT = LUTEXT, FILE = OUTFILE,STATUS = 'NEW')
C
       ENDIF
C
       IF(GRDTYP .EQ. 0) THEN
          CALL GRID_OUTPUT(YEAR,DAY,UT,LUTEXT,OUTTYP)
       ELSE IF(GRDTYP .EQ. 1) THEN
          CALL PRS_OUTPUT(YEAR,DAY,UT,LUTEXT,OUTTYP)
       ELSE
          CALL AZEL_OUTPUT(YEAR,DAY,UT,LUTEXT,OUTTYP)
       ENDIF
C
      ELSE IF ( ANS .EQ. 'Y') THEN
C
C      Merely report the intermediate results
C
      ENDIF
      RETURN
      END
C
      SUBROUTINE GRID_OUTPUT(YEAR,DAY,UT,LUN,ISWITCH)
C
C  PURPOSE
C     To calculate the high latitude ionosphere in comprehensive mode.
C
C  METHOD
C    The output grid is received from ICED, onto which complete EDP's are
C    calculated.
C
C  INPUT PARAMETERS
C     AP      REAL              The Ap index
C     DAY     INTEGER           The day of the year
C     LUN                       Logical unit associated with the
C                               file where the output is written
C     NALT    INTEGER           The number of points on the altitude grid
C     UT      REAL              The universal time (in seconds)
C     YEAR    INTEGER           The year
C
C  OUTPUT PARAMETERS
C     Output over the geographic lat-lon grid specified in common block
C     GRID.
C
C     CDATA   REAL       ARRAY  A vector containing the calculated quantity
C                               corresponding to a direct data record
C     GGLAT   REAL              The geographic latitude, in degrees north
C     GGLON   REAL              The geographic longitude, in degrees east
C     MLAT    REAL              The magnetic latitude, in degrees north
C     MLT     REAL              The magnetic local time, in hours
C
C  LOCAL VARIABLES
C     DATA    REAL       ARRAY  A vector containing a direct data record
C     DSGNTR  CHARACTER         A code designating a type of direct data
C     I       INTEGER           A loop counter
C     IL      INTEGER           A loop counter
C     J       INTEGER           A loop counter
C     LON     REAL              Longitude
C
C  SUBROUTINES REQUIRED
C     CGLAL1   Transforms geographic to corrected geomagnetic coordinates
C     REGION   Determines the high latitude region of a data record
C     REGMOD   Calculates a quantity using regional models
C
C  FILES ACCESSED
C    File attached to LUN (LUN input to subroutine).
C  AUTHOR
C     Robert E. Daniell,Lincoln D. Brown And William Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby    14-Apr-1994   1.0.8
C     L. Brown        18-Nov-1994   1.0.8 ==> 1.1
C                                   TEC has been added to output mode 0.
C                                   Output mode 2 has been redefined as a
C                                   combination of output modes 0 and 1.
C                                   Optimized altitude loop in EDP designator
C                                   setup.
C                                   The format of the output has been modified
C                                   to match PRISM output specifications.
C                                   Status messages showing the progress of
C                                   the gridded output are displayed to the
C                                   default output.
C                                   Removed call to routine MONTH.
C                                   Removed references to function FLPYR.
C                                   Removed local variables LEAPY, IERR,
C                                   IMPNTH, IDOM, SZA, NZNEU, and ZNEU.
C                                   Removed commented-out calls to routines
C                                   NEUATM and SOLZA.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   Changed the dimension of local variable
C                                   DATA from (100) to (108) and of local
C                                   variable CDATA from (100) to (107) to
C                                   avoid out-of-bounds errors.  These bugs
C                                   were reported by Dean Schulz.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Modified FORMATs 1300 and 1430 to be
C                                   consistent with PRISM.
C                                   Replaced WRITE statements to unit 6 with
C                                   PRINT statements.
C                                   Added header line above critical parameters
C                                   line in output file for ISWITCH=0.
C     L. Brown        23-Jan-1996   1.3 ==> 1.4
C                                   Changed "1X,0PF6.2" to "0PF7.2" in FORMAT
C                                   statement 1050 to accommodate the valid
C                                   ranges of the latitude and longitude
C                                   increments.
C                                   Changed "0PF7.2,' '" to "' ',0PF7.2" in
C                                   FORMAT statement 1200 to accommodate the
C                                   valid range of the longitude of the primary
C                                   coordinate system.
C                                   The original value (before it was
C                                   restricted to the range [0.,360.]) of the
C                                   longitude of the primary coordinate system
C                                   is used for output.
C                                   Added local variable LON.
C     L. Brown        14-Feb-1997   1.4 ==> 1.6
C                                   The plasmasphere flag is now encoded in the
C                                   combined coordinate system type / output
C                                   grid type flag.
C                                   Modified FORMAT 1410 to allow for
C                                   plasmaspheric altitudes.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     Common block
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'ssnstuff.inc'
C
C     Dimension and declaration statements
C
      INTEGER I,J,YEAR,DAY,LUN,IL
      INTEGER ISWITCH,NDAT
C
      REAL MLAT,MLT,DATA(108),CDATA(107),UT,MLON
      REAL GGLAT,GGLON,MLATSV,MLONSV
      REAL LON
C
      CHARACTER*16 DSGNTR
      LOGICAL TEST
C
      CALL GETSSN(USESSN,F10P7,SSN)
      IF (ISWITCH .LT. 3) THEN
C
C      We have grid output... determine the type of data
C
       IF (ISWITCH .EQ. 1) THEN
        DSGNTR = 'EDP'
C
C       Set up the altitude grid for the calls to REGMOD
        NDAT = NALT
        DATA(1) = FLOAT(NALT)
        DO I =1, NALT
         DATA(I+1) = ZOUT(I)
        END DO
       ELSE IF (ISWITCH .EQ. 0) THEN
        DSGNTR = 'IONOSONDE'
        DATA(5) = 1.0
        DATA(6) = 115.
        NDAT = 7
       ELSE IF (ISWITCH .EQ. 2) THEN
        DSGNTR = 'ALL'
        NDAT = NALT+7
        DATA(5) = 1.0
        DATA(6) = 115.
        DATA(8) = FLOAT(NALT)
        DO I =1, NALT
         DATA(I+8) = ZOUT(I)
        END DO
       ELSE
        PRINT *,' OUTTYP ERROR... OUTTYP = ',ISWITCH
        STOP
       ENDIF
       WRITE(LUN,1000)YEAR,DAY,UT,F10P7,RKP,SSN
C
C  Determine the combined coordinate system type / output grid type /
C  plasmasphere flag and write it to the PIM output file
C  Note:The combined coordinate system type / output grid type / plasmasphere
C       flag is determined as follows:
C         0 for geographic or 1 for corrected geomagnetic coordinate system
C       + 10*0 for rectangular latitude/longitude output grid type
C       + 100*0 for no plasmasphere or 100*1 for Gallagher plasmasphere
C
       IF(GEOG) THEN
          I=0
       ELSE
          I=1
       ENDIF
       IF(PLASPH) I=I+100
       WRITE(LUN,1020) I
C
C      Write the grid data
C
       WRITE(LUN,1050)GGLAT0,GGLATF,GGLON0,GGLONF,NUMLAT,NUMLON,
     1 DLAT, DLON
       WRITE (LUN,1070) ISWITCH
C
       TEST = .TRUE.
       GGLAT = (GGLAT0-DLAT)
       DO I = 1, NUMLAT
        GGLAT = GGLAT + DLAT
        LON=GGLON0-DLON
        DO J = 1, NUMLON
         LON=LON+DLON
         GGLON=LON
         IF (GGLON .LT. 0.0) GGLON=GGLON+360.0
         IF (GGLON .GE. 360.0) GGLON = GGLON-360.
         WRITE(*,'(1X,A,0PF6.2,A,0PF7.2,A)')
     &      'Output status:[Lat,Lon]=[',GGLAT,',',LON,']'
C
         IF (GEOG) THEN
C         The grid is in geographic coordinates ... get the corrected
C         geomagnetic coordinates.
          CALL GCXCGM(DAY,UT,GGLAT,GGLON,MLAT,MLON,MLT)
         ELSE
C         The grid is in magnetic coordinates... get the geographic stuff
          MLATSV = GGLAT
          MLONSV = GGLON
          CALL CGINV1(MLATSV,MLONSV,GGLAT,GGLON)
          CALL GCXCGM(DAY,UT,GGLAT,GGLON,MLAT,MLON,MLT)
          MLAT = MLATSV
          MLON = MLONSV
         ENDIF
C
C        Only call the model for the actual data
         CALL REGMOD(DSGNTR,YEAR,DAY,UT,GGLAT,GGLON,MLAT,MLON,
     2   MLT,NDAT,DATA,CDATA)
C
C  Restore the longitude of the primary coordinate system to its original value
C  (before it was restricted to the range [0.,360.]) for output
C
         IF(GEOG) THEN
            GGLON=LON
         ELSE
            MLON=LON
         ENDIF
C
         IF (ISWITCH .EQ. 1) THEN
C
C         Output EDP data
C
          IF (TEST) THEN
           WRITE(LUN,1400) NDAT
           WRITE(LUN,1410) (ZOUT(IL),IL=1,NDAT)
           TEST = .FALSE.
          ENDIF
          WRITE(LUN,1200) GGLAT,GGLON,MLAT,MLON,MLT
          WRITE(LUN,1405)
          WRITE(LUN,1415) (CDATA(IL),IL=1,NDAT)
         ELSE IF (ISWITCH .EQ. 0) THEN
C         Ionosonde data
          WRITE(LUN,1200) GGLAT,GGLON,MLAT,MLON,MLT
          WRITE(LUN,1430)
          WRITE(LUN,1300) (CDATA(IL),IL=1,NDAT)
         ELSE IF (ISWITCH .EQ. 2) THEN
          IF (TEST) THEN
           WRITE(LUN,1400) NALT
           WRITE(LUN,1410) (ZOUT(IL),IL=1,NALT)
           TEST = .FALSE.
          ENDIF
          WRITE(LUN,1200) GGLAT,GGLON,MLAT,MLON,MLT
          WRITE(LUN,1405)
          WRITE(LUN,1415) (CDATA(IL+7),IL=1,NALT)
          WRITE(LUN,1430)
          WRITE(LUN,1300) (CDATA(IL),IL=1,7)
         ELSE
C
C         Wrong ISWITCH
          PRINT *,ISWITCH
          STOP 'ISWITCH ERROR IN GRID_OUTPUT'
         ENDIF
C
         IF (.NOT. GEOG) THEN
C         Reload the GGLAT and GGLON with the magnetic coordinates
          GGLAT = MLATSV
          GGLON = MLONSV
         ENDIF
C
C       Next longitude
        END DO
C      Next latitude
       END DO
      ENDIF
      RETURN
C
 1000 FORMAT(' YEAR',3X,' DAY',2X,' UT (sec)',2X,' F10.7',2X,' Kp',3X,
     1 ' Solar Sunspot Number',/,60('-'),/,
     1 1X,I4,4X,I3,3X,F7.1,4X,F5.1,2X,F4.1,4X,F6.2,/)
 1020 FORMAT(/,1X,I4)
 1050 FORMAT (6x,' Latitude',6x,' Longitude',6x,'  Latitude',1x,
     1  ' Longitude Latitude Longitude',/,
     1  2(1x,' Starting Ending '),5x,2(' Step',2x),5x,2(' Delta',2x),/,
     2  4(1x,0PF7.2,1X),2(2X,I6),7x,2(0PF7.2,1x))
 1070 FORMAT(1X,I2)
 1100 FORMAT(2I3,' ',0PF8.2,4(0PF7.2,' '),/,
     1 ' LAT OUT OF RANGE OF THE MODEL')
 1200 FORMAT(/,' ',0PF8.2,4(' ',0PF7.2))
 1210 FORMAT(' Universal time = ',0PF5.2,/,
     1' Geographic latitude = ',0PF6.2,'  Geographic longitude = ',
     2 0PF6.2,/,' Magnetic Latitude = ',0PF6.2,
     3'   Magnetic Local time = ',0PF5.2)
 1212 FORMAT(A80)
 1250 FORMAT(F15.2,F9.2,F17.2,2(F10.2),2X,1PE16.3)
 1300 FORMAT(7(0PF7.2,' '))
 1400 FORMAT ( ' Number of altitude points = ',I6,/,
     1'   Altitudes')
 1405 FORMAT(' Densities')
 1410 FORMAT(5(1X,0PF8.2))
 1415 FORMAT(5(1X,1PE8.2))
 1420 FORMAT (' Region = ',A6,/)
 1430 FORMAT ('   FoF2,  HmF2,    FoF1,  HmF1,    FoE,  HmE    TEC')
      END
C
      SUBROUTINE PRS_OUTPUT(YEAR,DAY,UT,LUN,ISWITCH)
C
C  PURPOSE
C     To calculate latitude/longitude pairs output.
C
C  METHOD
C
C  INPUT PARAMETERS
C     AP      REAL              The Ap index
C     DAY     INTEGER           The day of the year
C     LUN                       Logical unit associated with the
C                               file where the output is written
C     NALT    INTEGER           The number of points on the altitude grid
C     UT      REAL              The universal time (in seconds)
C     YEAR    INTEGER           The year
C
C  OUTPUT PARAMETERS
C     Output over the geographic lat-lon grid specified in common block
C     GRID.
C
C     CDATA   REAL       ARRAY  A vector containing the calculated quantity
C                               corresponding to a direct data record
C     GGLAT   REAL              The geographic latitude, in degrees north
C     GGLON   REAL              The geographic longitude, in degrees east
C     MLAT    REAL              The magnetic latitude, in degrees north
C     MLT     REAL              The magnetic local time, in hours
C
C  LOCAL VARIABLES
C     DATA    REAL       ARRAY  A vector containing a direct data record
C     DSGNTR  CHARACTER         A code designating a type of direct data
C     I       INTEGER           A loop counter
C     IL      INTEGER           A loop counter
C     J       INTEGER           A loop counter
C
C  SUBROUTINES REQUIRED
C     CGLAL1   Transforms geographic to corrected geomagnetic coordinates
C     REGION   Determines the high latitude region of a data record
C     REGMOD   Calculates a quantity using regional models
C
C  FILES ACCESSED
C    File attached to LUN (LUN input to subroutine).
C
C  AUTHOR
C     Robert E. Daniell,Lincoln D. Brown And William Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        18-Nov-1994   1.1 ==> Created
C                                   Modified version of GRID_OUTPUT to work
C                                   with satellite track lat/lon.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   Modified WRITE statement for grid
C                                   parameters and FORMAT statement 1050 to
C                                   correctly identify grid parameters.
C                                   Changed the dimension of local variable
C                                   DATA from (100) to (108) and of local
C                                   variable CDATA from (100) to (107) to
C                                   avoid out-of-bounds errors.  These bugs
C                                   were reported by Dean Schulz.
C                                   Replaced "I" with "10+I" in format 1020
C                                   WRITE statement to output file to identify
C                                   output as satellite track output.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Modified FORMATs 1300 and 1430 to be
C                                   consistent with PRISM.
C                                   Replaced WRITE statements to unit 6 with
C                                   PRINT statements.
C                                   Added header line above critical parameters
C                                   line in output file for ISWITCH=0.
C     L. Brown        23-Jan-1996   1.3 ==> 1.4
C                                   Renamed from SAT_OUTPUT to PRS_OUTPUT.
C                                   Changed name of output grid type
C                                   "satellite track" to
C                                   "latitude/longitude pairs".
C                                   Variables MSAT, NSAT, SATLAT, and SATLON
C                                   renamed to MPR, NPR, PRLAT, and PRLON in
C                                   common block GRID.
C                                   Changed "1X,0PF6.2" to "0PF7.2" in FORMAT
C                                   statement 1050 to be consistent with
C                                   routine GRID_OUTPUT.
C                                   Changed "0PF7.2,' '" to "' ',0PF7.2" in
C                                   FORMAT statement 1200 to accommodate the
C                                   valid range of the longitude of the
C                                   latitude/longitude pair.
C                                   The original value (before it was
C                                   restricted to the range [0.,360.]) of the
C                                   longitude of the latitude/longitude pair
C                                   is used for output.
C     L. Brown        14-Feb-1997   1.4 ==> 1.6
C                                   The plasmasphere flag is now encoded in the
C                                   combined coordinate system type / output
C                                   grid type flag.
C                                   Modified FORMAT 1410 to allow for
C                                   plasmaspheric altitudes.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     Common block
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'ssnstuff.inc'
C
C     Dimension and declaration statements
C
      INTEGER I,YEAR,DAY,LUN,IL
      INTEGER ISWITCH,NDAT
C
      REAL MLAT,MLT,DATA(108),CDATA(107),UT,MLON
      REAL GGLAT,GGLON,MLATSV,MLONSV
C
      CHARACTER*16 DSGNTR
      LOGICAL TEST
C
      CALL GETSSN(USESSN,F10P7,SSN)
      IF (ISWITCH .LT. 3) THEN
C
C      We have grid output... determine the type of data
C
       IF (ISWITCH .EQ. 1) THEN
        DSGNTR = 'EDP'
C
C       Set up the altitude grid for the calls to REGMOD
        NDAT = NALT
        DATA(1) = FLOAT(NALT)
        DO I =1, NALT
         DATA(I+1) = ZOUT(I)
        END DO
       ELSE IF (ISWITCH .EQ. 0) THEN
        DSGNTR = 'IONOSONDE'
        DATA(5) = 1.0
        DATA(6) = 115.
        NDAT = 7
       ELSE IF (ISWITCH .EQ. 2) THEN
        DSGNTR = 'ALL'
        NDAT = NALT+7
        DATA(5) = 1.0
        DATA(6) = 115.
        DATA(8) = FLOAT(NALT)
        DO I =1, NALT
         DATA(I+8) = ZOUT(I)
        END DO
       ELSE
        PRINT *,' OUTTYP ERROR... OUTTYP = ',ISWITCH
        STOP
       ENDIF
       WRITE(LUN,1000)YEAR,DAY,UT,F10P7,RKP,SSN
C
C  Determine the combined coordinate system type / output grid type /
C  plasmasphere flag and write it to the PIM output file
C  Note:The combined coordinate system type / output grid type / plasmasphere
C       flag is determined as follows:
C         0 for geographic or 1 for corrected geomagnetic coordinate system
C       + 10*1 for latitude/longitude pairs output grid type
C       + 100*0 for no plasmasphere or 100*1 for Gallagher plasmasphere
C
       IF(GEOG) THEN
          I=0
       ELSE
          I=1
       ENDIF
       I=I+10
       IF(PLASPH) I=I+100
       WRITE(LUN,1020) I
C
C      Write the grid data
C
       WRITE(LUN,1050) PRLAT(1),PRLAT(NPR),PRLON(1),PRLON(NPR),
     &                 NPR,0,0.,0.
       WRITE (LUN,1070) ISWITCH
C
       TEST = .TRUE.
       DO I = 1, NPR
        GGLAT = PRLAT(I)
         GGLON = PRLON(I)
         IF (GGLON .LT. 0.0) GGLON=GGLON+360.0
         IF (GGLON .GE. 360.0) GGLON = GGLON-360.
         WRITE(*,'(1X,A,0PF6.2,A,0PF7.2,A)')
     &      'Output status:[Lat,Lon]=[',GGLAT,',',PRLON(I),']'
C
         IF (GEOG) THEN
C         The grid is in geographic coordinates ... get the corrected
C         geomagnetic coordinates.
          CALL GCXCGM(DAY,UT,GGLAT,GGLON,MLAT,MLON,MLT)
         ELSE
C         The grid is in magnetic coordinates... get the geographic stuff
          MLATSV = GGLAT
          MLONSV = GGLON
          CALL CGINV1(MLATSV,MLONSV,GGLAT,GGLON)
          CALL GCXCGM(DAY,UT,GGLAT,GGLON,MLAT,MLON,MLT)
          MLAT = MLATSV
          MLON = MLONSV
         ENDIF
C
C        Only call the model for the actual data
         CALL REGMOD(DSGNTR,YEAR,DAY,UT,GGLAT,GGLON,MLAT,MLON,
     2   MLT,NDAT,DATA,CDATA)
C
C  Restore the longitude of the latitude/longitude pair to its original value
C  (before it was restricted to the range [0.,360.]) for output
C
         IF(GEOG) THEN
            GGLON=PRLON(I)
         ELSE
            MLON=PRLON(I)
         ENDIF
C
         IF (ISWITCH .EQ. 1) THEN
C
C         Output EDP data
C
          IF (TEST) THEN
           WRITE(LUN,1400) NDAT
           WRITE(LUN,1410) (ZOUT(IL),IL=1,NDAT)
           TEST = .FALSE.
          ENDIF
          WRITE(LUN,1200) GGLAT,GGLON,MLAT,MLON,MLT
          WRITE(LUN,1405)
          WRITE(LUN,1415) (CDATA(IL),IL=1,NDAT)
         ELSE IF (ISWITCH .EQ. 0) THEN
C         Ionosonde data
          WRITE(LUN,1200) GGLAT,GGLON,MLAT,MLON,MLT
          WRITE(LUN,1430)
          WRITE(LUN,1300) (CDATA(IL),IL=1,NDAT)
         ELSE IF (ISWITCH .EQ. 2) THEN
          IF (TEST) THEN
           WRITE(LUN,1400) NALT
           WRITE(LUN,1410) (ZOUT(IL),IL=1,NALT)
           TEST = .FALSE.
          ENDIF
          WRITE(LUN,1200) GGLAT,GGLON,MLAT,MLON,MLT
          WRITE(LUN,1405)
          WRITE(LUN,1415) (CDATA(IL+7),IL=1,NALT)
          WRITE(LUN,1430)
          WRITE(LUN,1300) (CDATA(IL),IL=1,7)
         ELSE
C
C         Wrong ISWITCH
          PRINT *,ISWITCH
          STOP 'ISWITCH ERROR IN PRS_OUTPUT'
         ENDIF
C
         IF (.NOT. GEOG) THEN
C         Reload the GGLAT and GGLON with the magnetic coordinates
          GGLAT = MLATSV
          GGLON = MLONSV
         ENDIF
C
       END DO
      ENDIF
      RETURN
C
 1000 FORMAT(' YEAR',3X,' DAY',2X,' UT (sec)',2X,' F10.7',2X,' Kp',3X,
     1 ' Solar Sunspot Number',/,60('-'),/,
     1 1X,I4,4X,I3,3X,F7.1,4X,F5.1,2X,F4.1,4X,F6.2,/)
 1020 FORMAT(/,1X,I4)
 1050 FORMAT (6x,' Latitude',6x,' Longitude',6x,'   Number ',1x,
     1  '    n/a      n/a       n/a   ',/,
     1  2(1x,' Starting Ending '),5x,' Pts ',2x,' n/a ',2x,5x,
     &  2('  n/a ',2x),/,
     2  4(1x,0PF7.2,1X),2(2X,I6),7x,2(0PF7.2,1x))
 1070 FORMAT(1X,I2)
 1100 FORMAT(2I3,' ',0PF8.2,4(0PF7.2,' '),/,
     1 ' LAT OUT OF RANGE OF THE MODEL')
 1200 FORMAT(/,' ',0PF8.2,4(' ',0PF7.2))
 1210 FORMAT(' Universal time = ',0PF5.2,/,
     1' Geographic latitude = ',0PF6.2,'  Geographic longitude = ',
     2 0PF6.2,/,' Magnetic Latitude = ',0PF6.2,
     3'   Magnetic Local time = ',0PF5.2)
 1212 FORMAT(A80)
 1250 FORMAT(F15.2,F9.2,F17.2,2(F10.2),2X,1PE16.3)
 1300 FORMAT(7(0PF7.2,' '))
 1400 FORMAT ( ' Number of altitude points = ',I6,/,
     1'   Altitudes')
 1405 FORMAT(' Densities')
 1410 FORMAT(5(1X,0PF8.2))
 1415 FORMAT(5(1X,1PE8.2))
 1420 FORMAT (' Region = ',A6,/)
 1430 FORMAT ('   FoF2,  HmF2,    FoF1,  HmF1,    FoE,  HmE    TEC')
      END
C
      SUBROUTINE AZEL_OUTPUT(YEAR,DAY,UT,LUN,ISWITCH)
C
C  PURPOSE
C     To calculate the high latitude ionosphere in comprehensive mode.
C
C  METHOD
C    The output grid is received from ICED, onto which complete EDP's are
C    calculated.
C
C  INPUT PARAMETERS
C     AP      REAL              The Ap index
C     DAY     INTEGER           The day of the year
C     LUN                       Logical unit associated with the
C                               file where the output is written
C     NALT    INTEGER           The number of points on the altitude grid
C     UT      REAL              The universal time (in seconds)
C     YEAR    INTEGER           The year
C
C  OUTPUT PARAMETERS
C     Output over the geographic lat-lon grid specified in common block
C     GRID.
C
C     CDATA   REAL       ARRAY  A vector containing the calculated quantity
C                               corresponding to a direct data record
C     GGLAT   REAL              The geographic latitude, in degrees north
C     GGLON   REAL              The geographic longitude, in degrees east
C     MLAT    REAL              The magnetic latitude, in degrees north
C     MLT     REAL              The magnetic local time, in hours
C
C  LOCAL VARIABLES
C     DATA    REAL       ARRAY  A vector containing a direct data record
C     DSGNTR  CHARACTER         A code designating a type of direct data
C     I       INTEGER           A loop counter
C     IL      INTEGER           A loop counter
C     J       INTEGER           A loop counter
C
C  SUBROUTINES REQUIRED
C     CGLAL1   Transforms geographic to corrected geomagnetic coordinates
C     REGION   Determines the high latitude region of a data record
C     REGMOD   Calculates a quantity using regional models
C
C  FILES ACCESSED
C    File attached to LUN (LUN input to subroutine).
C  AUTHOR
C     Robert E. Daniell,Lincoln D. Brown And William Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        27-Apr-1995   1.2 ==> Created
C                                   Modified version of GRID_OUTPUT to work
C                                   with radar azimuth/elevation.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Modified FORMATs 1300 and 1430 to be
C                                   consistent with PRISM.
C                                   Replaced WRITE statements to unit 6 with
C                                   PRINT statements.
C                                   Added header line above critical parameters
C                                   line in output file for ISWITCH=0.
C     L. Brown        23-Jan-1996   1.3 ==> 1.4
C                                   Corrected calculation of slant TEC by
C                                   replacing variable ZOUT with RNG in calls
C                                   to routine TECCLC.
C                                   Renamed from RADAR_OUTPUT to AZEL_OUTPUT.
C                                   Changed name of output grid type
C                                   "radar azimuth/elevation" to
C                                   "azimuth/elevation (ground-based)".
C                                   Variables RADLAT and RADLON renamed to
C                                   OBSLAT and OBSLON in common block GRID.
C                                   Changed "1X,0PF6.2" to "0PF7.2" in FORMAT
C                                   statement 1050 to accommodate the valid
C                                   range of the azimuth increment.
C                                   Changed "0PF7.2,' '" to "' ',0PF7.2" in
C                                   FORMAT statement 1200 to accommodate the
C                                   valid ranges of the observer longitude and
C                                   the azimuth.
C     L. Brown        14-Feb-1997   1.4 ==> 1.6
C                                   The plasmasphere flag is now encoded in the
C                                   combined coordinate system type / output
C                                   grid type flag.
C                                   Modified FORMAT 1410 to allow for
C                                   plasmaspheric altitudes.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     Common block
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'ssnstuff.inc'
C
C     Dimension and declaration statements
C
      INTEGER I,J,K,YEAR,DAY,LUN,IL
      INTEGER ISWITCH,NDAT
C
      REAL MLAT,MLT,DATA(108),CDATA(107),UT,MLON
      REAL GGLAT,GGLON,MLATSV,MLONSV,AZ,EL,ED(100),RNG(100),EDMAX,
     &     RNGMAX,ALTMAX,FPMAX,LKALT,TEC,LKRANG
C
      CHARACTER*16 DSGNTR
      LOGICAL TEST
C
      CALL GETSSN(USESSN,F10P7,SSN)
      IF (ISWITCH .LT. 3) THEN
C
C      We have grid output
C
       DSGNTR = 'EDP'
C
C       Set up the altitude grid for the calls to REGMOD
       NDAT = 1
       DATA(1) = FLOAT(1)
C
       WRITE(LUN,1000)YEAR,DAY,UT,F10P7,RKP,SSN
C
C  Determine the combined coordinate system type / output grid type /
C  plasmasphere flag and write it to the PIM output file
C  Note:The combined coordinate system type / output grid type / plasmasphere
C       flag is determined as follows:
C         0 for geographic or 1 for corrected geomagnetic coordinate system
C       + 10*2 for azimuth/elevation (ground-based) output grid type
C       + 100*0 for no plasmasphere or 100*1 for Gallagher plasmasphere
C
       IF(GEOG) THEN
          I=0
       ELSE
          I=1
       ENDIF
       I=I+20
       IF(PLASPH) I=I+100
       WRITE(LUN,1020) I
C
C      Write the grid data
C
       WRITE(LUN,1050)SAZ,SAZ+DAZ*FLOAT(NAZ-1),SEL,SEL+DEL*FLOAT(NEL-1),
     &                NAZ,NEL,DAZ,DEL
       WRITE (LUN,1070) ISWITCH
C
       TEST = .TRUE.
       AZ = (SAZ-DAZ)
       DO I = 1, NAZ
        AZ = AZ + DAZ
        EL = SEL - DEL
        DO J = 1, NEL
         EL = EL + DEL
         WRITE(*,'(1X,A,0PF7.2,A,0PF6.2,A)')
     &      'Output status:[Az,El]=[',AZ,',',EL,']'
         DO K=1,NALT
          DATA(2)=ZOUT(K)
          IF(EL .GT. 89.9999) THEN
             GGLAT=OBSLAT
             GGLON=OBSLON
             RNG(K)=ZOUT(K)
          ELSE
             CALL LKCRDS(OBSLAT,OBSLON,AZ,EL,ZOUT(K),GGLAT,GGLON)
             RNG(K)=LKRANG(EL,ZOUT(K))
          ENDIF
          IF (GGLON .LT. 0.0) GGLON=GGLON+360.0
          IF (GGLON .GE. 360.0) GGLON = GGLON-360.
C
          IF (GEOG) THEN
C         The grid is in geographic coordinates ... get the corrected
C         geomagnetic coordinates.
           CALL GCXCGM(DAY,UT,GGLAT,GGLON,MLAT,MLON,MLT)
          ELSE
C         The grid is in magnetic coordinates... get the geographic stuff
           MLATSV = GGLAT
           MLONSV = GGLON
           CALL CGINV1(MLATSV,MLONSV,GGLAT,GGLON)
           CALL GCXCGM(DAY,UT,GGLAT,GGLON,MLAT,MLON,MLT)
           MLAT = MLATSV
           MLON = MLONSV
          ENDIF
C
C        Only call the model for the actual data
          CALL REGMOD(DSGNTR,YEAR,DAY,UT,GGLAT,GGLON,MLAT,MLON,
     2    MLT,NDAT,DATA,CDATA)
          ED(K)=CDATA(1)
         ENDDO
C
         IF (ISWITCH .EQ. 1) THEN
C
C         Output EDP data
C
          IF (TEST) THEN
           WRITE(LUN,1400) NDAT
           WRITE(LUN,1410) (ZOUT(IL),IL=1,NDAT)
           TEST = .FALSE.
          ENDIF
          WRITE(LUN,1200) AZ,EL,OBSLAT,OBSLON,0.
          WRITE(LUN,1405)
          WRITE(LUN,1415) (ED(IL),IL=1,NALT)
         ELSE IF (ISWITCH .EQ. 0) THEN
C         Ionosonde data
          CALL FNDMAX(NALT,RNG,ED,EDMAX,RNGMAX)
          FPMAX=SQRT(EDMAX/1.24E4)
          IF(EL .GT. 89.9999) THEN
             ALTMAX=RNGMAX
          ELSE
             ALTMAX=LKALT(EL,RNGMAX)
          ENDIF
          CALL TECCLC(NALT,RNG,ED,TEC)
          TEC=TEC*1.E-12
          WRITE(LUN,1200) AZ,EL,OBSLAT,OBSLON,0.
          WRITE(LUN,1430)
          WRITE(LUN,1300) FPMAX,ALTMAX,0.,0.,0.,0.,TEC
         ELSE IF (ISWITCH .EQ. 2) THEN
          CALL FNDMAX(NALT,RNG,ED,EDMAX,RNGMAX)
          FPMAX=SQRT(EDMAX/1.24E4)
          IF(EL .GT. 89.9999) THEN
             ALTMAX=RNGMAX
          ELSE
             ALTMAX=LKALT(EL,RNGMAX)
          ENDIF
          CALL TECCLC(NALT,RNG,ED,TEC)
          TEC=TEC*1.E-12
          IF (TEST) THEN
           WRITE(LUN,1400) NALT
           WRITE(LUN,1410) (ZOUT(IL),IL=1,NALT)
           TEST = .FALSE.
          ENDIF
          WRITE(LUN,1200) AZ,EL,OBSLAT,OBSLON,0.
          WRITE(LUN,1405)
          WRITE(LUN,1415) (ED(IL),IL=1,NALT)
          WRITE(LUN,1430)
          WRITE(LUN,1300) FPMAX,ALTMAX,0.,0.,0.,0.,TEC
         ELSE
C
C         Wrong ISWITCH
          PRINT *,ISWITCH
          STOP 'ISWITCH ERROR IN AZEL_OUTPUT'
         ENDIF
C
         IF (.NOT. GEOG) THEN
C         Reload the GGLAT and GGLON with the magnetic coordinates
          GGLAT = MLATSV
          GGLON = MLONSV
         ENDIF
C
C       Next longitude
        END DO
C      Next latitude
       END DO
      ELSE
       PRINT *,' OUTTYP ERROR... OUTTYP = ',ISWITCH
       STOP
      ENDIF
      RETURN
C
 1000 FORMAT(' YEAR',3X,' DAY',2X,' UT (sec)',2X,' F10.7',2X,' Kp',3X,
     1 ' Solar Sunspot Number',/,60('-'),/,
     1 1X,I4,4X,I3,3X,F7.1,4X,F5.1,2X,F4.1,4X,F6.2,/)
 1020 FORMAT(/,1X,I4)
 1050 FORMAT (6x,' Azimuth ',6x,' Elevation',6x,'  Azimuth ',1x,
     1  ' Elevation Azimuth  Elevation',/,
     1  2(1x,' Starting Ending '),5x,2(' Step',2x),5x,2(' Delta',2x),/,
     2  4(1x,0PF7.2,1X),2(2X,I6),7x,2(0PF7.2,1x))
 1070 FORMAT(1X,I2)
 1100 FORMAT(2I3,' ',0PF8.2,4(0PF7.2,' '),/,
     1 ' LAT OUT OF RANGE OF THE MODEL')
 1200 FORMAT(/,' ',0PF8.2,4(' ',0PF7.2))
 1210 FORMAT(' Universal time = ',0PF5.2,/,
     1' Geographic latitude = ',0PF6.2,'  Geographic longitude = ',
     2 0PF6.2,/,' Magnetic Latitude = ',0PF6.2,
     3'   Magnetic Local time = ',0PF5.2)
 1212 FORMAT(A80)
 1250 FORMAT(F15.2,F9.2,F17.2,2(F10.2),2X,1PE16.3)
 1300 FORMAT(7(0PF7.2,' '))
 1400 FORMAT ( ' Number of altitude points = ',I6,/,
     1'   Altitudes')
 1405 FORMAT(' Densities')
 1410 FORMAT(5(1X,0PF8.2))
 1415 FORMAT(5(1X,1PE8.2))
 1420 FORMAT (' Region = ',A6,/)
 1430 FORMAT ('  fpmax,  hmax,     n/a,   n/a,    n/a,  n/a    TEC')
      END
