      SUBROUTINE GETDAT(IYEAR,IDOY,IMON,UT,OUTFILE,OUTTYP,GRDTYP)
C
C  PURPOSE
C     To get the input data for this run
C
C  METHOD
C     Get all the input data for a run
C      1) Let user input the year, day and Universal time for this run
C      2) Read the paths for the input data
C      3) Access "real-time" data, reformat it, and write it to the
C      "Direct Data" file
C      4) open the range output data file if there is range data available
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    LUD     INTEGER           logical unit number of direct data file
C    NINDIR  INTEGER           dimension of INDRCT in calling program
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    GRDTYP  INTEGER           The switch for the type of output grid:
C                              [0] Rectangular latitude/longitude grid
C                              [1] Latitude/longitude pairs
C                              [2] Azimuth/elevation (ground-based)
C    IDOY    INTEGER           day of year
C    OUTTYP  INTEGER           The switch for type of output:
C                              [0] Critical frequencies and heights and
C                                  vertical TEC
C                              [1] Vertical EDPs
C                              [2] Vertical EDPs, critical frequencies and
C                                  heights, and vertical TEC
C                              [3] No output
C    IYEAR   INTEGER           year (four digits)
C    OUTFILE CHARACTER         The name of the file containing the output
C                              data
C    UT      REAL              Universal Time (decimal hours)
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C
C  SUBROUTINES CALLED
C    GETDATE Reads the date,time,type of output,and output file name
C    GETPLA  Reads the plasmasphere flag from the PIM input stream
C    GETSGP  Reads the station geophysical parameters
C    SET_UP  Sets up the direct data file for the HLISM run.
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     William G. Whartenby
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
C     W. Whartenby     1-Sep-1990   1.0.6
C     L. Brown        18-Nov-1994   1.0.6 ==> 1.1
C                                   Output parameter IGRIDSW has been renamed
C                                   to OUTTYP.
C                                   Output parameter GRDTYP has neen added.
C                                   The description of output parameter OUTTYP
C                                   in the header comments has changed.
C                                   The argument list of routine GETDATE has
C                                   changed.
C                                   The argument list of routine SET_UP has
C                                   changed.
C                                   The argument list of routine SGI has
C                                   changed.
C                                   Removed local variables PSGP and LEAPY.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   The description of input parameter GRDTYP
C                                   has been modified to include the radar
C                                   output grid type.
C                                   Variable F10P7A from common block INDIRECT
C                                   has been removed from the call to routine
C                                   SGI since it is not used by PIM.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Removed argument BATCH from the call to
C                                   routine GETDATE.
C                                   Removed argument ONLYOP from the call to
C                                   routine SGI.
C                                   Removed input parameter BATCH since it is
C                                   no longer used.
C     L. Brown        23-Jan-1996   1.3 ==> 1.4
C                                   Changed name of output grid type
C                                   "satellite track" to
C                                   "latitude/longitude pairs".
C                                   Changed name of output grid type
C                                   "radar azimuth/elevation" to
C                                   "azimuth/elevation (ground-based)".
C     L. Brown        14-Feb-1997   1.4 ==> 1.6
C                                   Added call to routine GETPLA after call to
C                                   routine GET_GRID.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C    LURT     INTEGER           logical unit number to be used for accessing
C                              "real-time" data
C
      EXTERNAL FAP
      REAL FAP
C
C
      INCLUDE 'indirect.inc'
      INCLUDE 'logicuni.inc'
      INCLUDE 'grid.inc'
C
      INTEGER OUTTYP,GRDTYP
      INTEGER IYEAR,IDOY,IMON,IDOM,I
      CHARACTER*32 OUTFILE
      REAL UT
C
C     Get run date, output files, and station lists
C
      CALL GETDATE(IDOY,IYEAR,IMON,IDOM,UT,OUTTYP,GRDTYP,OUTFILE)
C
C
C      Get the station geophysical parameters
C
      CALL SGI(RF10P7,RKP,BY,URSISW,ESWITCH)
C
C      There will be some kind of grid output...get the output grid
C
      IF (OUTTYP .LT. 3) CALL GET_GRID
C
C  Get the plasmasphere flag
C
      CALL GETPLA
C
C     Set up the temporary files and get path names.
C
      CALL SET_UP(LUCGDB)
C     Initialize F10.7 and E layer Kp to the actual station data
C     reads
C
      F10P7 = RF10P7
      EKP = MIN(RKP,6.5)
      EKP = MAX(EKP,0.0)
      DO I = 0,2
       KP(I) = EKP
      ENDDO
C
C     Determine AP from the real KP value
C
      AP = FAP(RKP)
      RETURN
      END
      SUBROUTINE GETDATE(DOY,YEAR,MONTH,DOM,UT,OUTTYP,GRDTYP,OUTFIL)
C
C  PURPOSE
C     To get the date, time, output type, output grid type, and output file
C     name from the PIM input stream.
C
C  METHOD
C     The year, day of the year, UT in HHMM, output type, output grid type, and
C     name of the output file are read from the PIM input stream.  The year and
C     day of the year are converted to month and day of the month, and the UT
C     is converted from HHMM to decimal hours.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     GRDTYP Integer    Scalar        n/a             0 <= GRDTYP <= 2
C        The output grid type, 0 for a rectangular latitude/longitude grid, 1
C        for latitude/longitude pairs, or 2 for azimuth/elevation
C        (ground-based)
C     MONTH  Integer    Scalar        n/a             1 <= MONTH <= 12
C        The day of the month
C     DOM    Integer    Scalar        n/a             1 <= DOM <= 31
C        The day of the month
C     DOY    Integer    Scalar        n/a             1 <= DOY <= 366
C        The day of the year
C     OUTFIL Char*32    Scalar        n/a             n/a
C        The name of the output file
C     OUTTYP Integer    Scalar        n/a             0 <= OUTTYP <= 2
C        The output type, 0 for critical frequencies and heights and TEC, 1 for
C        EDPs, or 2 for EDPs, critical frequencies and heights, and TEC
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        The universal time
C     YEAR   Integer    Scalar        n/a             1800 <= YEAR <= 2000
C        The year
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DOYMAX Integer    Scalar        n/a             365 or 366
C        The maximum allowed day of the year
C     UTHH   Integer    Scalar        Hours           0 <= UTHH <= 23
C        The hours of Universal Time
C     UTHHMM Integer    Scalar        HHMM            0000 <= UTHHMM <= 2359
C        The Universal Time
C     UTMM   Integer    Scalar        Minutes         0 <= UTMM <= 59
C        The minutes of Universal Time
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     TIMMDM Determines the month and day of the month from the year and day of
C            the year, taking leap years into consideration
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     TIMLPY Logical    Determines if a year is a leap year
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     William G. Whartenby
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
C     W. Whartenby    20-Jul-1990   1.0.6
C     L. Brown        18-Nov-1994   1.0.6 ==> 1.1
C                                   Complete rewrite.
C                                   Removed input parameter LEAPY.
C                                   The operating mode is set to .TRUE. for
C                                   batch (noninteractive) operating mode.
C                                   Output parameter GRDTYP has been added.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   The output grid type can now be 2, for
C                                   radar azimuth/elevation.
C                                   The description of input parameter GRDTYP
C                                   has been modified to include the radar
C                                   output grid type.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Removed input parameter BATCH since it is
C                                   no longer used.
C     L. Brown        23-Jan-1996   1.3 ==> 1.4
C                                   Added validation of the year.
C                                   Leap years are now accounted for when
C                                   validating the day of the year.
C                                   Improved the validation of the Universal
C                                   Time by separately checking the hours and
C                                   minutes of Universal Time.
C                                   Changed name of output grid type
C                                   "satellite track" to
C                                   "latitude/longitude pairs".
C                                   Changed name of output grid type
C                                   "radar azimuth/elevation" to
C                                   "azimuth/elevation (ground-based)".
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Output variables
C
      INTEGER YEAR,DOY,MONTH,DOM,OUTTYP,GRDTYP
      REAL UT
      CHARACTER*32 OUTFIL
C
C  Local variables
C
      INTEGER DOYMAX,UTHHMM,UTHH,UTMM
      LOGICAL TIMLPY
C
C  Read the year from the PIM input stream
C
      READ(*,*,END=10,ERR=10) YEAR
      GOTO 20
   10 STOP 'GETDATE:Error reading year.'
   20 CONTINUE
C
C  Check the validity of the year
C
      IF((YEAR .LT. 1800) .OR. (YEAR .GT. 2100))
     &   STOP 'GETDATE:Invalid year.'
C
C  Read the day of the year from the PIM input stream
C
      READ(*,*,END=100,ERR=100) DOY
      GOTO 110
  100 STOP 'GETDATE:Error reading day of the year.'
  110 CONTINUE
C
C  Check the validity of the day of the year
C
      IF(TIMLPY(YEAR)) THEN
         DOYMAX=366
      ELSE
         DOYMAX=365
      ENDIF
      IF((DOY .LT. 1) .OR. (DOY .GT. DOYMAX))
     &   STOP 'GETDATE:Invalid day of the year.'
C
C  Read the Universal Time in HHMM from the PIM input stream
C
      READ(*,*,END=200,ERR=200) UTHHMM
      GOTO 210
  200 STOP 'GETDATE:Error reading UT.'
  210 CONTINUE
C
C  Check the validity of the Universal Time
C
      UTHH=UTHHMM/100
      UTMM=MOD(UTHHMM,100)
      IF((UTHH .LT. 0) .OR. (UTHH .GT. 23) .OR.
     &   (UTMM .LT. 0) .OR. (UTMM .GT. 59))
     &   STOP 'GETDATE:Invalid Universal Time.'
C
C  Read the output type from the PIM input stream
C
      READ(*,*,END=300,ERR=300) OUTTYP
      GOTO 310
  300 STOP 'GETDATE:Error reading output type.'
  310 CONTINUE
C
C  Check the validity of the output type
C
      IF((OUTTYP .LT. 0) .OR. (OUTTYP .GT. 2))
     &   STOP 'GETDATE:Invalid output type.'
C
C  Read the output grid type from the PIM input stream
C
      READ(*,*,END=400,ERR=400) GRDTYP
      GOTO 410
  400 STOP 'GETDATE:Error reading output grid type.'
  410 CONTINUE
C
C  Check the validity of the output grid type
C
      IF((GRDTYP .LT. 0) .OR. (GRDTYP .GT. 2))
     &   STOP 'GETDATE:Invalid output grid type.'
C
C  Read the name of the output file from the PIM input stream
C
      READ(*,'(A)',END=500,ERR=500) OUTFIL
      GOTO 510
  500 STOP 'GETDATE:Error reading name of output file.'
  510 CONTINUE
C
C  Convert the day of the year to month and day of the month
C
      CALL TIMMDM(YEAR,DOY,MONTH,DOM)
C
C  Convert the Universal Time to decimal hours
C
      UT=FLOAT(UTHH)+FLOAT(UTMM)/60.
C
      RETURN
      END
      SUBROUTINE SGI(F10P7,KP,BY,URSISW,ESW)
C
C  PURPOSE
C     To get solar and geophysical parameters and foF2 and foE normalizations
C     from the PIM input stream.
C
C  METHOD
C     The orientation of IMF By, orientation of IMF Bz, monthly average F10.7,
C     sunspot number treatment flag, instantaneous F10.7, sunspot number,
C     magnetic Kp index, foF2 normalization, and foE normalization are read
C     from the PIM input stream.  The orientation of IMF By is converted from a
C     character to a real value.  IF the orientation of IMF Bz is zero or
C     positive, then the magnetic Kp index is set to 1.  Depending on the value
C     of the sunspot number treatment flag, the instantaneous F10.7 may be
C     calculated from the sunspot number or vice versa.  The foF2 and foE
C     normalizations are converted from integral to logical values.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     BY     Real       Scalar        n/a             -1. or 1.
C        The orientation of IMF By, -1. for negative or 1. for positive
C     ESW    Logical    Scalar        n/a             .TRUE. or .FALSE.
C        The foE normalization, .TRUE. if foE is to be normalized by the CCIR
C        foE model, .FALSE. if foE is to be normalized by the parameterized
C        models
C     F10P7  Real       Scalar        n/a             0. <= F10P7 <= 300.
C        The instantaneous F10.7
C     KP     Real       Scalar        n/a             0. <= KP <= 9.
C        The magnetic Kp index
C     URSISW Logical    Scalar        n/a             .TRUE. or .FALSE.
C        The foF2 normalization, .TRUE. if foF2 is to be normalized by URSI
C        foF2, .FALSE. if foF2 is to be normalized by the parameterized USU
C        model
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CBY    Char*1     Scalar        n/a             '-' or '+'
C        The orientation of IMF By, '-' for negative or '+' for positive
C     CBZ    Char*1     Scalar        n/a             '-','0', or '+'
C        The orientation of IMF Bz, '-' for negative, '0' for zero, or '+' for
C        positive; for Bz zero or positive, Kp is set to 1
C     IFOEN  Integer    Scalar        n/a             0 or 1
C        The foE normalization, 0 if foE is to be normalized by the CCIR foE
C        model, 1 if foE is to be normalized by the parameterized models
C     IFOF2N Integer    Scalar        n/a             0 or 1
C        The foF2 normalization, 0 if foF2 is to be normalized by URSI foF2, 1
C        if foF2 is to be normalized by the parameterized USU model
C     ISECT  Integer    Scalar        n/a             0 <= ISECT <= 4
C        The LLF sector flag, 0 if all four longitude sectors are to be used
C        in the LLF parameterized model, 1 if only the Brazilian sector is to
C        be used in the LLF parameterized model, 2 if only the Indian sector is
C        to be used in the LLF parameterized model, 3 if only the Pacific
C        sector is to be used in the LLF parameterized model, or 4 if only the
C        USA sector is to be used in the LLF parameterized model
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     SETSCT Sets the LLF sector flag in common block LOWER
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     W. Whartenby                Created
C     L. Brown       18-Nov-1994  1.1
C                                 Complete rewrite.
C                                 Added input section after foE normalization
C                                 input section to get the sector to use in the
C                                 low-latitude F-region parameterized model
C                                 (LLF).
C                                 Zero IMF By is no longer supported.
C                                 Added IMF Bz to the PIM input stream after
C                                 IMF By.  If IMF Bz is zero or positive, then
C                                 Kp is set to 1.
C     L. Brown       27-Apr-1995  1.1 ==> 1.2
C                                 Removed input parameter F10P7A since it is
C                                 not used by PIM.
C                                 Removed input parameter ONLYOP since it is no
C                                 longer used by PIM.
C     L. Brown       23-Jan-1996  1.2 ==> 1.4
C                                 Removed conditional validation of F10.7.  It
C                                 is now always validated.
C                                 Removed conditional validation of SSN.  It is
C                                 now always validated.
C     L. Brown       14-Feb-1997  1.4 ==> 1.6
C                                 Changed "semi-empirical E-layer model" to
C                                 "CCIR foE model" in comments.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Output variables
C
      REAL F10P7,KP,BY
      LOGICAL URSISW,ESW
C
C  Local variables
C
      INTEGER IFOF2N,IFOEN,ISECT
      CHARACTER*1 CBY,CBZ
C
      INCLUDE 'ssnstuff.inc'
C
C  Read the orientation of IMF By from the PIM input stream
C
      READ(*,'(A)',END=10,ERR=10) CBY
      GOTO 20
   10 STOP 'SGI:Error reading orientation of IMF By.'
   20 CONTINUE
C
C  Check the validity of the orientation of IMF By
C
      IF((CBY .NE. '-') .AND. (CBY .NE. '+'))
     &   STOP 'SGI:Invalid orientation of IMF By.'
C
C  Read the orientation of IMF Bz from the PIM input stream
C
      READ(*,'(A)',END=100,ERR=100) CBZ
      GOTO 110
  100 STOP 'SGI:Error reading orientation of IMF Bz.'
  110 CONTINUE
C
C  Check the validity of the orientation of IMF Bz
C
      IF((CBZ .NE. '-') .AND. (CBZ .NE. '0') .AND. (CBZ .NE. '+'))
     &   STOP 'SGI:Invalid orientation of IMF Bz.'
C
C  Read the sunspot number treatment flag from the PIM input stream
C
      READ(*,*,END=200,ERR=200) USESSN
      GOTO 210
  200 STOP 'SGI:Error reading sunspot number treatment flag.'
  210 CONTINUE
C
C  Check the validity of the sunspot number treatment flag
C
      IF((USESSN .LT. 0) .OR. (USESSN .GT. 2))
     &   STOP 'SGI:Invalid sunspot number treatment flag.'
C
C  Read the instantaneous F10.7 from the PIM input stream
C
      READ(*,*,END=300,ERR=300) F10P7
      GOTO 310
  300 STOP 'SGI:Error reading instantaneous F10.7.'
  310 CONTINUE
C
C  Check the validity of the instantaneous F10.7
C
      IF((F10P7 .LT. 0.) .OR. (F10P7 .GT. 300.))
     &   STOP 'SGI:Invalid instantaneous F10.7.'
C
C  Read the sunspot number from the PIM input stream
C
      READ(*,*,END=400,ERR=400) SSN
      GOTO 410
  400 STOP 'SGI:Error reading sunspot number.'
  410 CONTINUE
C
C  Check the validity of the sunspot number
C
      IF((SSN .LT. 0.) .OR. (SSN .GT. 300.))
     &   STOP 'SGI:Invalid sunspot number.'
C
C  Read the magnetic Kp index from the PIM input stream
C
      READ(*,*,END=500,ERR=500) KP
      GOTO 510
  500 STOP 'SGI:Error reading magnetic Kp index.'
  510 CONTINUE
C
C  Check the validity of the magnetic Kp index
C
      IF((KP .LT. 0.) .OR. (KP .GT. 9.))
     &   STOP 'SGI:Invalid magnetic Kp index.'
C
C  Get the foF2 normalization from the PIM input stream
C
      READ(*,*,END=600,ERR=600) IFOF2N
      GOTO 610
  600 STOP 'SGI:Error reading foF2 normalization.'
  610 CONTINUE
C
C  Check the validity of the foF2 normalization
C
      IF((IFOF2N .NE. 0) .AND. (IFOF2N .NE. 1))
     &   STOP 'SGI:Invalid foF2 normalization.'
C
C  Get the foE normalization from the PIM input stream
C
      READ(*,*,END=700,ERR=700) IFOEN
      GOTO 710
  700 STOP 'SGI:Error reading foE normalization.'
  710 CONTINUE
C
C  Check the validity of the foE normalization
C
      IF((IFOEN .NE. 0) .AND. (IFOEN .NE. 1))
     &   STOP 'SGI:Invalid foE normalization.'
C
C  Get the LLF sector flag from the PIM input stream
C
      READ(*,*,END=800,ERR=800) ISECT
      GOTO 810
  800 STOP 'SGI:Error reading LLF sector flag.'
  810 CONTINUE
C
C  Check the validity of the LLF sector flag
C
      IF((ISECT .LT. 0) .OR. (ISECT .GT. 4))
     &   STOP 'SGI:Invalid LLF sector flag.'
C
C  Store the orientation of IMF By as a real number
C
      IF(CBY .EQ. '+') THEN
         BY=1.
      ELSE
         BY=-1.
      ENDIF
C
C  If the orientation of IMF Bz is zero or positive, then set the magnetic Kp
C  index to 1.
C
      IF((CBZ .EQ. '0') .OR. (CBZ .EQ. '+')) KP=1.
C
C  Calculate the sunspot number if necessary
C
      IF(USESSN .EQ. 1) SSN=MAX(0.,SQRT(93918.4+1117.3*F10P7)-406.37)
C
C  Calculate the instantaneous F10.7 if necessary
C
      IF(USESSN .EQ. 2)
     &   F10P7=MAX(63.74,((SSN+406.37)**2-93918.4)/1117.3)
C
C  Convert the foF2 normalization to a logical value
C
      IF(IFOF2N .EQ. 0) THEN
         URSISW=.TRUE.
      ELSE
         URSISW=.FALSE.
      ENDIF
C
C  Convert the foE normalization to a logical value
C
      IF(IFOEN .EQ. 0) THEN
         ESW=.TRUE.
      ELSE
         ESW=.FALSE.
      ENDIF
C
C  Set the LLF sector flag in common block LOWER
C
      CALL SETSCT(ISECT)
C
      RETURN
      END
      SUBROUTINE SETSCT(I)
C
C  Set the sector flag for the LLF parameterized model
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        7-Oct-1994  1.1 ==> Created
C
      INCLUDE 'lower.inc'
C
      INTEGER I
C
      LLFSCT=I
C
      RETURN
      END
      SUBROUTINE GET_GRID
C
C  PURPOSE
C     To get the coordinate system type, rectangular latitude/longitude grid,
C     latitude/longitude pairs grid, azimuth/elevation grid, and altitude grid
C     from the PIM input stream.
C
C  METHOD
C     The coordinate system type, rectangular latitude/longitude grid,
C     latitude/longitude pairs grid, azimuth/elevation grid, and altitude grid
C     are read from the PIM input stream and validated.  The coordinate system
C     type is converted from a character to a logical value.  The ending
C     latitude and longitude of the rectangular latitude/longitude grid are
C     calculated.  The altitude grid is sorted into increasing order.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CRDTYP Char*1     Scalar        n/a             'G', 'g', 'M', or 'm'
C        The coordinate system type, 'G' or 'g' for geographic, 'M' or 'm' for
C        corrected geomagnetic
C     EEL    Real       Scalar        Degrees         0. <= EEL <= 90.
C        The ending elevation
C     IALT   Integer    Scalar        n/a             1 <= IALT <= NALT
C        A loop counter for altitude
C     IPR    Integer    Scalar        n/a             1 <= IPR <= NPR
C        A loop counter for latitude/longitude pairs
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     SORT   Sorts an array of real numbers into ascending order
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     W. Whartenby                Created
C     L. Brown       18-Nov-1994  1.1
C                                 Complete rewrite.
C                                 Satellite track latitude and longitude is now
C                                 part of the PIM input stream after the
C                                 rectangular latitude/longitude grid
C                                 information.
C                                 Common block GRID has changed to include
C                                 satellite track latitude and longitude.
C     L. Brown       27-Apr-1995  1.1 ==> 1.2
C                                 Radar azimuth and elevation is now part of
C                                 the PIM input stream after the satellite
C                                 track latitude and longitude information.
C     L. Brown       23-Jan-1996  1.2 ==> 1.4
C                                 Added validation of the latitude increment.
C                                 Added upper limit to the validation of the
C                                 number of longitudes.
C                                 Added validation of the starting longitude.
C                                 Added validation of the longitude increment.
C                                 Changed name of output grid type
C                                 "satellite track" to
C                                 "latitude/longitude pairs".
C                                 Variables MSAT, NSAT, SATLAT, and SATLON
C                                 renamed to MPR, NPR, PRLAT, and PRLON in
C                                 common block GRID.
C                                 Local variable ISAT renamed to IPR.
C                                 Added validation of latitude/longitude pair
C                                 longitudes.
C                                 Changed name of output grid type
C                                 "radar azimuth/elevation" to
C                                 "azimuth/elevation (ground-based)".
C                                 Variables RADLAT and RADLON renamed to OBSLAT
C                                 and OBSLON in common block GRID.
C                                 Added validation of observer longitude.
C                                 Added upper limit to the validation of the
C                                 number of azimuths.
C                                 Added validation of the starting azimuth.
C                                 Added validation of the azimuth increment.
C                                 Added validation of the elevation increment.
C                                 Updated PURPOSE comment section.
C                                 Updated METHOD comment section.
C     L. Brown       14-Feb-1997  1.4 ==> 1.6
C                                 Increased maximum allowed altitude from
C                                 1600 km to 25000 km.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Local variables
C
      INTEGER IALT,IPR
      REAL EEL
      CHARACTER*1 CRDTYP
C
      INCLUDE 'grid.inc'
C
C  Read the coordinate system type from the PIM input stream
C
      READ(*,'(A1)',END=10,ERR=10) CRDTYP
      GOTO 20
   10 STOP 'GET_GRID:Error reading coordinate system type.'
   20 CONTINUE
C
C  Check the validity of the coordinate system type
C
      IF((CRDTYP .NE. 'G') .AND. (CRDTYP .NE. 'g') .AND.
     &   (CRDTYP .NE. 'M') .AND. (CRDTYP .NE. 'm'))
     &   STOP 'GET_GRID:Invalid coordinate system type.'
C
C  Read the number of latitude points from the PIM input stream
C
      READ(*,*,END=100,ERR=100) NUMLAT
      GOTO 110
  100 STOP 'GET_GRID:Error reading number of latitude points.'
  110 CONTINUE
C
C  Check the validity of the number of latitude points
C
      IF(NUMLAT .LT. 1)
     &   STOP 'GET_GRID:Invalid number of latitude points.'
C
C  Read the starting latitude from the PIM input stream
C
      READ(*,*,END=200,ERR=200) GGLAT0
      GOTO 210
  200 STOP 'GET_GRID:Error reading starting latitude.'
  210 CONTINUE
C
C  Check the validity of the starting latitude
C
      IF((GGLAT0 .LT. -90.) .OR. (GGLAT0 .GT. 90.))
     &   STOP 'GET_GRID:Invalid starting latitude.'
C
C  Read the latitude increment from the PIM input stream
C
      READ(*,*,END=300,ERR=300) DLAT
      GOTO 310
  300 STOP 'GET_GRID:Error reading latitude increment.'
  310 CONTINUE
C
C  Check the validity of the latitude increment
C
      IF((DLAT .LT. -180.) .OR. (DLAT .GT. 180.) .OR. (DLAT .EQ. 0.))
     &   STOP 'GET_GRID:Invalid latitude increment.'
C
C  Read the number of longitude points from the PIM input stream
C
      READ(*,*,END=400,ERR=400) NUMLON
      GOTO 410
  400 STOP 'GET_GRID:Error reading number of longitude points.'
  410 CONTINUE
C
C  Check the validity of the number of longitude points
C
      IF((NUMLON .LT. 1) .OR. (NUMLON .GT. 3601))
     &   STOP 'GET_GRID:Invalid number of longitude points.'
C
C  Read the starting longitude from the PIM input stream
C
      READ(*,*,END=500,ERR=500) GGLON0
      GOTO 510
  500 STOP 'GET_GRID:Error reading starting longitude.'
  510 CONTINUE
C
C  Check the validity of the starting longitude
C
      IF((GGLON0 .LT. -360.) .OR. (GGLON0 .GT. 360.))
     &   STOP 'GET_GRID:Invalid starting longitude.'
C
C  Read the longitude increment from the PIM input stream
C
      READ(*,*,END=600,ERR=600) DLON
      GOTO 610
  600 STOP 'GET_GRID:Error reading longitude increment.'
  610 CONTINUE
C
C  Check the validity of the longitude increment
C
      IF((DLON .LT. -360.) .OR. (DLON .GT. 360.) .OR. (DLON .EQ. 0.))
     &   STOP 'GET_GRID:Invalid longitude increment.'
C
C  Read the number of latitude/longitude pairs from the PIM input stream
C
      READ(*,*,END=700,ERR=700) NPR
      GOTO 710
  700 STOP 'GET_GRID:Error reading number of latitude/longitude pairs.'
  710 CONTINUE
C
C  Check the validity of the number of latitude/longitude pairs
C
      IF((NPR .LT. 1) .OR. (NPR .GT. MPR))
     &   STOP 'GET_GRID:Invalid number of latitude/longitude pairs.'
C
C  Read the latitude/longitude pairs from the PIM input stream
C
      DO 820 IPR=1,NPR
         READ(*,*,END=800,ERR=800) PRLAT(IPR),PRLON(IPR)
         GOTO 810
  800    STOP
     &      'GET_GRID:Error reading latitude/longitude pairs.'
  810    CONTINUE
  820 CONTINUE
C
C  Check the validity of the latitude/longitude pairs
C
      DO 900 IPR=1,NPR
         IF((PRLAT(IPR) .LT. -90.) .OR. (PRLAT(IPR) .GT. 90.))
     &      STOP 'GET_GRID:Invalid latitude of latitude/longitude pair.'
         IF((PRLON(IPR) .LT. -360.) .OR. (PRLON(IPR) .GT. 360.))
     &     STOP 'GET_GRID:Invalid longitude of latitude/longitude pair.'
  900 CONTINUE
C
C  Read the observer latitude from the PIM input stream
C
      READ(*,*,END=1000,ERR=1000) OBSLAT
      GOTO 1010
 1000 STOP 'GET_GRID:Error reading observer latitude.'
 1010 CONTINUE
C
C  Check the validity of the observer latitude
C
      IF((OBSLAT .LT. -90.) .OR. (OBSLAT .GT. 90.))
     &   STOP 'GET_GRID:Invalid observer latitude.'
C
C  Read the observer longitude from the PIM input stream
C
      READ(*,*,END=1100,ERR=1100) OBSLON
      GOTO 1110
 1100 STOP 'GET_GRID:Error reading observer longitude.'
 1110 CONTINUE
C
C  Check the validity of the observer longitude
C
      IF((OBSLON .LT. -360.) .OR. (OBSLON .GT. 360.))
     &   STOP 'GET_GRID:Invalid observer longitude.'
C
C  Read the number of azimuth points from the PIM input stream
C
      READ(*,*,END=1200,ERR=1200) NAZ
      GOTO 1210
 1200 STOP 'GET_GRID:Error reading number of azimuth points.'
 1210 CONTINUE
C
C  Check the validity of the number of azimuth points
C
      IF((NAZ .LT. 1) .OR. (NAZ .GT. 3601))
     &   STOP 'GET_GRID:Invalid number of azimuth points.'
C
C  Read the starting azimuth from the PIM input stream
C
      READ(*,*,END=1300,ERR=1300) SAZ
      GOTO 1310
 1300 STOP 'GET_GRID:Error reading starting azimuth.'
 1310 CONTINUE
C
C  Check the validity of the starting azimuth
C
      IF((SAZ .LT. -360.) .OR. (SAZ .GT. 360.))
     &   STOP 'GET_GRID:Invalid starting azimuth.'
C
C  Read the azimuth increment from the PIM input stream
C
      READ(*,*,END=1400,ERR=1400) DAZ
      GOTO 1410
 1400 STOP 'GET_GRID:Error reading azimuth increment.'
 1410 CONTINUE
C
C  Check the validity of the azimuth increment
C
      IF((DAZ .LT. -360.) .OR. (DAZ .GT. 360.) .OR. (DAZ .EQ. 0.))
     &   STOP 'GET_GRID:Invalid azimuth increment.'
C
C  Read the number of elevation points from the PIM input stream
C
      READ(*,*,END=1500,ERR=1500) NEL
      GOTO 1510
 1500 STOP 'GET_GRID:Error reading number of elevation points.'
 1510 CONTINUE
C
C  Check the validity of the number of elevation points
C
      IF(NEL .LT. 1)
     &   STOP 'GET_GRID:Invalid number of elevation points.'
C
C  Read the starting elevation from the PIM input stream
C
      READ(*,*,END=1600,ERR=1600) SEL
      GOTO 1610
 1600 STOP 'GET_GRID:Error reading starting elevation.'
 1610 CONTINUE
C
C  Check the validity of the starting elevation
C
      IF((SEL .LT. 0.) .OR. (SEL .GT. 90.))
     &   STOP 'GET_GRID:Invalid starting elevation.'
C
C  Read the elevation increment from the PIM input stream
C
      READ(*,*,END=1700,ERR=1700) DEL
      GOTO 1710
 1700 STOP 'GET_GRID:Error reading elevation increment.'
 1710 CONTINUE
C
C  Check the validity of the elevation increment
C
      IF((DEL .LT. -90.) .OR. (DEL .GT. 90.) .OR. (DEL .EQ. 0.))
     &   STOP 'GET_GRID:Invalid elevation increment.'
C
C  Read the number of altitude points from the PIM input stream
C
      READ(*,*,END=1800,ERR=1800) NALT
      GOTO 1810
 1800 STOP 'GET_GRID:Error reading number of altitude points.'
 1810 CONTINUE
C
C  Check the validity of the number of altitude points
C
      IF((NALT .LT. 1) .OR. (NALT .GT. MAXALTPT))
     &   STOP 'GET_GRID:Invalid number of altitude points.'
C
C  Read the altitude grid from the PIM input stream
C
      READ(*,*,END=1900,ERR=1900) (ZOUT(IALT),IALT=1,NALT)
      GOTO 1910
 1900 STOP 'GET_GRID:Error reading altitude grid.'
 1910 CONTINUE
C
C  Check the validity of the altitude grid
C
      DO 2000 IALT=1,NALT
         IF((ZOUT(IALT) .LT. 90.) .OR. (ZOUT(IALT) .GT. 25000.))
     &      STOP 'GET_GRID:Invalid altitude grid.'
 2000 CONTINUE
C
C  Convert the coordinate system type to a logical value
C
      IF((CRDTYP .EQ. 'G') .OR. (CRDTYP .EQ. 'g')) THEN
         GEOG=.TRUE.
      ELSE
         GEOG=.FALSE.
      ENDIF
C
C  Calculate the ending latitude
C
      GGLATF=GGLAT0+DLAT*FLOAT(NUMLAT-1)
C
C  Check the validity of the ending latitude
C
      IF((GGLATF .LT. -90.) .OR. (GGLATF .GT. 90.))
     &   STOP 'GET_GRID:Invalid ending latitude.'
C
C  Calculate the ending longitude
C
      GGLONF=GGLON0+DLON*FLOAT(NUMLON-1)
C
C  Calculate the ending elevation
C
      EEL=SEL+DEL*FLOAT(NEL-1)
C
C  Check the validity of the ending elevation
C
      IF((EEL .LT. 0.) .OR. (EEL .GT. 90.))
     &   STOP 'GET_GRID:Invalid ending elevation.'
C
C  Sort the altitude grid into ascending order
C
      CALL SORT(NALT,ZOUT)
C
      RETURN
      END
      SUBROUTINE SORT(N,ARR)
C
C  PURPOSE
C     To sort an array of real numbers into ascending order.
C
C  METHOD
C     Straight insertion is the sorting algorithm.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ARR    Real       Vector        n/a             n/a
C                       (N)
C        The array of real numbers to be sorted
C     N      Integer    Scalar        n/a             > 0
C        The number of real numbers in the array
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ARR    Real       Vector        n/a             n/a
C                       (N)
C        The sorted array of real numbers
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     A      Real       Scalar        n/a             n/a
C        A value from the array
C     I      Integer    Scalar        n/a             1 <= I < N
C        A loop counter
C     J      Integer    Scalar        n/a             2 <= J <= N
C        A loop counter
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     385 Elliot Street
C     Newton, MA 02164  USA
C     (617)-964-7553
C
C  VERSION
C     1.1   5-April-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       17-Feb-1989  1.0 ==> Created
C     L. Brown        5-Apr-1994  1.0 ==> 1.1
C                                 Brought internal documentation up to specs.
C
C  REFERENCES
C     1. Press, W. H., B. P. Flannery, S. A. Teukolsky, and W. T. Vettering,
C        'Numerical Recipes', Cambridge University Press, Cambridge, 1986.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER N
      REAL ARR(N)
C
C  Local variables
C
      INTEGER I,J
      REAL A
C
      DO 30 J=2,N
         A=ARR(J)
         DO 10 I=J-1,1,-1
            IF(ARR(I) .LE. A) GOTO 20
            ARR(I+1)=ARR(I)
   10    CONTINUE
         I=0
   20    ARR(I+1)=A
   30 CONTINUE
C
      RETURN
      END
      SUBROUTINE GETPLA
C
C  PURPOSE
C     To get the plasmasphere flag from the PIM input stream.
C
C  METHOD
C     The plasmasphere flag is read from the PIM input stream and validated.  A
C     missing plasmasphere flag record does not cause a stoppage, for
C     compatibility with older PIM input streams; instead, a default value of
C     'N' is assigned.  The plasmasphere flag is converted from a character to
C     a logical value.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     STMP   Char*1     Scalar        n/a             'Y', 'y', 'N', or 'n'
C        A temporary variable used for reading the plasmasphere flag from the
C        PIM input stream
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       14-Feb-1997  1.6 ==> Created
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Local variables
C
      CHARACTER*1 STMP
C
      INCLUDE 'grid.inc'
C
C  Read the plasmasphere flag from the PIM input stream
C  Note:A missing plasmasphere flag record does not cause a stoppage, for
C       compatibility with older PIM input streams; instead, a default value of
C       'N' is assigned.
C
      READ(*,'(A1)',END=20,ERR=10) STMP
      GOTO 30
   10 STOP 'GETPLA:Error reading plasmsphere flag.'
   20 STMP='N'
   30 CONTINUE
C
C  Check the validity of the plasmasphere flag
C
      IF((STMP .NE. 'Y') .AND. (STMP .NE. 'y') .AND.
     &   (STMP .NE. 'N') .AND. (STMP .NE. 'n'))
     &   STOP 'GETPLA:Invalid plasmasphere flag.'
C
C  Convert the plasmasphere flag to a logical value
C
      IF((STMP .EQ. 'Y') .OR. (STMP .EQ. 'y')) THEN
         PLASPH=.TRUE.
      ELSE
         PLASPH=.FALSE.
      ENDIF
C
      RETURN
      END
      SUBROUTINE SET_UP(LUCGDB)
C
C  PURPOSE
C     To get the type of record-length for direct-access files and the
C     locations of data files from the PIM path names input file PATH_NAM.TXT,
C     and to read the corrected geomagnetic coordinate conversion database from
C     the corrected geomagnetic coordinate conversion database file.
C
C  METHOD
C     The PIM path names input file PATH_NAM.TXT is opened, the type of record-
C     length for direct-access files and the locations of data files are read
C     from it, and it is closed.  The location of the corrected geomagnetic
C     coordinate conversion database is passed to subroutine RDCGDB, which
C     reads the corrected geomagnetic coordinate conversion database from the
C     corrected coordinate conversion database file.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     LUCGDB  Integer    Scalar        n/a             n/a
C        The logical unit number used to access the corrected geomagnetic
C        coordinate conversion database
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C    ISTAT   Integer    Scalar        n/a             n/a
C        The status returned from the STRTRM routine
C    LCGDB   Char*80    Scalar        n/a             n/a
C        The location of the corrected geomagnetic coordinate conversion
C        database
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     RDCGDB Reads the corrected geomagnetic coordinate conversion database
C            from the corrected geomagnetic coordinate conversion database file
C     STRTRM Trims specified leading characters from a character string
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     PATH_NAM.TXT Formatted  1      The PIM path names input file
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     W. Whartenby   29-Aug-1994  1.0.6
C     L. Brown       18-Nov-1994  1.0.6 ==> 1.1
C                                 Complete rewrite.
C                                 Removed output parameter PSGP.
C                                 The location of the solar and geophysical
C                                 parameters has been removed from
C                                 PATH_NAM.TXT since it is not used by PIM.
C     L. Brown       25-Sep-1995  1.1 ==> 1.3
C                                 Removed call to routine STRLEN since it is
C                                 not needed.
C                                 Removed local variable LLCGDB since it is no
C                                 longer used.
C                                 Leading spaces are now removed from all of
C                                 the path names.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER LUCGDB
C
C  Local variables
C
      INTEGER ISTAT
      CHARACTER*80 LCGDB
C
      INCLUDE 'dpath.inc'
C
C  Open the PIM path names input file
C
      OPEN(UNIT=1,FILE='path_nam.txt',STATUS='OLD',FORM='FORMATTED',
     &     ERR=10)
      GOTO 20
   10 STOP 'SET_UP:Error opening PATH_NAM.TXT.'
   20 CONTINUE
C
C  Read the type of record-length for direct-access files from the PIM path
C  names input file
C
      READ(1,*,END=100,ERR=100) JOPEN
      GOTO 110
  100 STOP 'SET_UP:Error reading direct-access record-length type.'
  110 CONTINUE
C
C  Check the validity of the type of record-length for direct-access files
C
      IF((JOPEN .NE. 1) .AND. (JOPEN .NE. 2))
     &   STOP 'SET_UP:Invalid direct-access record-length type.'
C
C  Read the location of the corrected geomagnetic coordinate database from the
C  PIM path names input file
C
      READ(1,'(A)',END=200,ERR=200) LCGDB
      GOTO 210
  200 STOP 'SET_UP:Error reading CGM coordinate database location.'
  210 CALL STRTRM(LCGDB,32,ISTAT)
      WRITE(*,'(1X,A,/,1X,A)') 'Corrected geomagnetic coordinate '//
     &                         'database location:',LCGDB
C
C  Read the location of the parameterized USU database from the PIM path names
C  input file
C
      READ(1,'(A)',END=300,ERR=300) PUSU
      GOTO 310
  300 STOP 'SET_UP:Error reading parameterized USU database location.'
  310 CALL STRTRM(PUSU,32,ISTAT)
      WRITE(*,'(1X,A,/,1X,A)') 'Parameterized USU database location:',
     &                         PUSU
C
C  Read the location of the parameterized mid-latitude F-region database from
C  the PIM path names input file
C
      READ(1,'(A)',END=400,ERR=400) PMID
      GOTO 410
  400 STOP 'SET_UP:Error reading parameterized MLF database location.'
  410 CALL STRTRM(PMID,32,ISTAT)
      WRITE(*,'(1X,A,/,1X,A)') 'Parameterized mid-latitude F-region '//
     &                         'database location:',PMID
C
C  Read the location of the parameterized low-latitude F-region database from
C  the PIM path names input file
C
      READ(1,'(A)',END=500,ERR=500) PLOW
      GOTO 510
  500 STOP 'SET_UP:Error reading parameterized LLF database location.'
  510 CALL STRTRM(PLOW,32,ISTAT)
      WRITE(*,'(1X,A,/,1X,A)') 'Parameterized low-latitude F-region '//
     &                         'database location:',PLOW
C
C  Read the location of the parameterized low- and mid- latitude E-region
C  database from the PIM path names input file
C
      READ(1,'(A)',END=600,ERR=600) PLME
      GOTO 610
  600 STOP 'SET_UP:Error reading parameterized LME database location.'
  610 CALL STRTRM(PLME,32,ISTAT)
      WRITE(*,'(1X,A,/,1X,A)') 'Parameterized low- and mid- latitude '//
     &                         'E-region database location:',PLME
C
C  Read the location of the URSI coefficients database from the PIM path names
C  input file
C
      READ(1,'(A)',END=700,ERR=700) PAWS
      GOTO 710
  700 STOP 'SET_UP:Error reading URSI coefficients database.'
  710 CALL STRTRM(PAWS,32,ISTAT)
      WRITE(*,'(1X,A,/,1X,A)') 'URSI coefficients database location:',
     &                         PAWS
C
C  Close the PIM path names input file
C
      CLOSE(UNIT=1,ERR=800)
      GOTO 810
  800 STOP 'SET_UP:Error closing PATH_NAM.TXT.'
  810 CONTINUE
C
C  Read the corrected geomagnetic coordinate conversion database (required for
C  conversion to and from corrected geomagnetic coordinates) from the corrected
C  geomagnetic coordinate conversion database file
C
      CALL RDCGDB(LCGDB,LUCGDB)
C
      RETURN
      END
