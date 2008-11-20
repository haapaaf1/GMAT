C IRIS13.FOR -------------------------------------------- May 1998
C 
C includes subroutines IRIS13 to compute IRI parameters for specified
C location, date, time, and altitude range and subroutine IRI_WEB to 
c computes IRI parameters for specified location, date, time and
c variable range; variable can be altitude, latitude, longitude, year, 
c month, day of month, day of year, or hour (UT or LT).
C 
C*****************************************************************
C CHANGES FROM  IRIS11.FOR  TO   IRIS12.FOR:
C    - CIRA-1986 INSTEAD OF CIRA-1972 FOR NEUTRAL TEMPERATURE
C    - 10/30/91 VNER FOR NIGHTTIME LAY-VERSION:  ABS(..)
C    - 10/30/91 XNE(..) IN CASE OF LAY-VERSION
C    - 10/30/91 CHANGE SSIN=F/T TO IIQU=0,1,2
C    - 10/30/91 Te > Ti > Tn ENFORCED IN FINAL PROFILE
C    - 10/30/91 SUB ALL NAMES WITH 6 OR MORE CHARACTERS
C    - 10/31/91 CORRECTED HF1 IN HST SEARCH:  NE(HF1)>NME
C    - 11/14/91 C1=0 IF NO F1-REGION
C    - 11/14/91 CORRECTED HHMIN AND HZ FOR LIN. APP.
C    -  1/28/92 RZ12=0 included
C    -  1/29/92 NEQV instead of NE between URSIF2 and URSIFO
C    -  5/ 1/92 CCIR and URSI input as in IRID12
C    -  9/ 2/94 Decimal month (ZMONTH) for IONCOM
C    -  9/ 2/94 Replace B0POL with B0_TAB; better annually
C    -  1/ 4/95 DY for h>hmF2
C    -  2/ 2/95 IG for foF2, topside; RZ for hmF2, B0_TAB, foF1, NmD
C    -  2/ 2/95 winter no longer exclusive for F1 occurrrence
C    -  2/ 2/95 RZ and IG included as DATA statement; smooth annual var.
C CHANGES FROM  IRIS12.FOR  TO   IRIS13.FOR:
C    - 10/26/95 include year as input and corrected MODA; nrm for zmonth
C    - 10/26/95 use TCON and month-month interpolation in foF2, hmF2
C    - 10/26/95 TCON only if date changes
C    - 11/25/95 take out logicals TOPSI, BOTTO, and BELOWE
C    - 12/ 1/95 UT_LT for (date-)correct UT<->LT conversion
C    - 12/22/95 Change ZETA cov term to cov < 180; use cov inst covsat
C    -  2/23/96 take covmax(R<150) for topside; lyear,.. for lt
C    -  3/26/96 topside: 94.5/BETA inst 94.45/..; cov -> covsat(<=188)
C    -  5/01/96 No longer DY for h>hmF2 (because of discontinuity)
C    - 12/01/96 IRIV13: HOUR for IVAR=1 (height)
C    -  4/25/97 D-region: XKK le 10 with D1 calc accordingly.
C    -  1/12/97 DS model for lower ion compoistion DY model
C    -  5/19/98 seamon=zmonth if lati>0; zmonth= ...(1.0*iday)/..
C    -  5/19/98 DY ion composition model below 300 km now DS model
C    -  5/19/98 DS model includes N+, Cl down to 75 km HNIA changed
C    -  5/28/98 User input for Rz12, foF1/NmF1, hmF1, foE/NmE, hmE; jf(17)
C    -  9/ 2/98 1 instead of 0 in MODA after UT_LT call
C    -  4/30/99 program constants moved from DATA statement into program
C    -  4/30/99 changed konsol-unit to 13 (12 is for IG_RZ).
C    -  5/29/99 the limit for IG computed from Rz12-input is 174 not 274
C    - 11/08/99 jf(18)=.true. simple UT to LT conversion, otherwise UT_LT
C    - 11/09/99 added COMMON/const1/humr,dumr also for CIRA86
C
C*****************************************************************
C********* INTERNATIONAL REFERENCE IONOSPHERE (IRI). *************
C*****************************************************************
C**************** ALL-IN-ONE SUBROUTINE  *************************
C*****************************************************************
C
C
       SUBROUTINE IRIS13(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,DHOUR,
     &    HEIBEG,HEIEND,HEISTP,OUTF,OARR)
C-----------------------------------------------------------------
C
C INPUT:  JF(1:20)      true/false switches for several options
C         JMAG          =0 geographic   = 1 geomagnetic coordinates
C         ALATI,ALONG   LATITUDE NORTH AND LONGITUDE EAST IN DEGREES
C         IYYYY         Year as YYYY, e.g. 1985
C         MMDD (-DDD)   DATE (OR DAY OF YEAR AS A NEGATIVE NUMBER)
C         DHOUR         LOCAL TIME (OR UNIVERSAL TIME + 25) IN DECIMAL 
C                          HOURS
C         HEIBEG,       HEIGHT RANGE IN KM; maximal 100 heights, i.e.
C          HEIEND,HEISTP        int((heiend-heibeg)/heistp)+1.le.100
C
C         JF switches to turn off/on (true/false) several options
C
C       jf(i)=          .true.                  .false.
C      ---------------------------------------------------------------------
C        i=1         Ne computed            Ne not computed
C          2         Te, Ti computed        Te, Ti not computed
C          3         Ni computed            Ni not computed
C          4         B0 - Table option      B0 - Gulyaeva (1987)
C          5         foF2 - CCIR            foF2 - URSI
C          6         Ni - Standard          Ni - Danilov-Yaichnikov-Smironova
C          7         Ne - Standard Topside  Ne - IRI-79 Topside
C          8         foF2 from model        foF2 or NmF2 - user input
C          9         hmF2 from model        hmF2 or M3000F2 - user input
C         10         Te - Standard          Te - Using Te/Ne correlation
C         11         Ne - Standard Profile  Ne - Lay-function formalism
C         12         Messages are written to unit
C                             6                          12
C                    {you can also turn off all messages by setting
C                     KONSOL=1 internally}
C         13         foF1 from model        foF1 or NmF1 - user input
C         14         hmF1 from model        hmF1 - user input
C         15         foE  from model        foE or NmE - user input
C         16         hmE  from model        hmE - user input
C         17         Rz12 from file         Rz12 - user input
C         18         simple ut<->lt         using ut_lt subroutine
C         19    free
C         20    free
C      ---------------------------------------------------------------------
C
C  Depending on the jf() settings additional INPUT parameters may be required:
C
C          Setting              INPUT parameter
C       ------------------------------------------------------
C       jf(8)=.false.      OARR(1)=user input for foF2/MHz or NmF2/m-3
C       jf(9)=.false.      OARR(2)=user input for hmF2/km or M(3000)F2
C       jf(10)=.false.     OARR(15),OARR(16),OARR(17)=user input for 
C                            Ne(300km),Ne(400km),Ne(600km)/m-3; use 
C                            OARR()=-1 if one of these values is not
C                            available.
C       jf(13)=.false.     OARR(3)=user input for foF1/MHz or NmF1/m-3 
C       jf(14)=.false.     OARR(4)=user input for hmF1/km
C       jf(15)=.false.     OARR(5)=user input for foE/MHz or NmE/m-3 
C       jf(16)=.false.     OARR(6)=user input for hmE/km
C       jf(17)=.flase.     OARR(33)=user input for Rz12
C
C  OUTPUT:  OUTF(1:11,1:100)
C               OUTF(1,*)  ELECTRON DENSITY/M-3
C               OUTF(2,*)  NEUTRAL TEMPERATURE/K
C               OUTF(3,*)  ION TEMPERATURE/K
C               OUTF(4,*)  ELECTRON TEMPERATURE/K
C               OUTF(5,*)  O+ ION DENSITY/M-3
C               OUTF(6,*)  H+ ION DENSITY/M-3
C               OUTF(7,*)  HE+ ION DENSITY/M-3
C               OUTF(8,*)  O2+ ION DENSITY/M-3
C               OUTF(9,*)  NO+ ION DENSITY/M-3
C                 AND, IF JF(6)=.FALSE.:
C               OUTF(10,*)  PERCENTAGE OF CLUSTER IONS IN %
C               OUTF(11,*)  PERCENTAGE OF N+ IONS IN %
C
C            OARR(1:35)   ADDITIONAL OUTPUT PARAMETERS         
C
C              #OARR(1) = NMF2/M-3           #OARR(2) = HMF2/KM
C              #OARR(3) = NMF1/M-3           #OARR(4) = HMF1/KM
C              #OARR(5) = NME/M-3            #OARR(6) = HME/KM
C               OARR(7) = NMD/M-3             OARR(8) = HMD/KM
C               OARR(9) = HHALF/KM            OARR(10) = B0/KM
C               OARR(11) =VALLEY-BASE/M-3     OARR(12) = VALLEY-TOP/KM
C               OARR(13) = TE-PEAK/K          OARR(14) = TE-PEAK HEIGHT/KM
C              #OARR(15) = TE-MOD(300KM)     #OARR(16) = TE-MOD(400KM)/K
C              #OARR(17) = TE-MOD(600KM)      OARR(18) = TE-MOD(1400KM)/K
C               OARR(19) = TE-MOD(3000KM)     OARR(20) = TE(120KM)=TN=TI/K
C               OARR(21) = TI-MOD(430KM)      OARR(22) = X/KM, WHERE TE=TI
C               OARR(23) = SOL ZENITH ANG/DEG OARR(24) = SUN DECLINATION/DEG
C               OARR(25) = DIP/deg            OARR(26) = DIP LATITUDE/deg
C               OARR(27) = MODIFIED DIP LAT.  OARR(28) = DELA
C               OARR(29) = sunrise/dec. hours OARR(30) = sunset/dec. hours
C               OARR(31) = ISEASON (1=spring) OARR(32) = NSEASON (northern)
C              #OARR(33) = Rz12               OARR(34) = Covington Index
C               OARR(35) = B1                 oarr(36) = M(3000)F2
C     place_holders only:
C               oarr(36) = TEC/m-2            oarr(38) = TEC_top/TEC*100.
C
C      # INPUT as well as OUTPUT parameter
C      lower case:      special for IRIWeb
C-------------------------------------------------------------------
C*****************************************************************
C*** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        ***
C***     ELECTRON DENSITY         60/80 KM       1000 KM       ***
C***     TEMPERATURES              120 KM        3000 KM       ***
C***     ION DENSITIES             100 KM        1000 KM       ***
C*****************************************************************
C*****************************************************************
C*********            INTERNALLY                    **************
C*********       ALL ANGLES ARE IN DEGREE           **************
C*********       ALL DENSITIES ARE IN M-3           **************
C*********       ALL ALTITUDES ARE IN KM            **************
C*********     ALL TEMPERATURES ARE IN KELVIN       **************
C*********     ALL TIMES ARE IN DECIMAL HOURS       **************
C*****************************************************************
C*****************************************************************
C*****************************************************************
      INTEGER           DAYNR,DDO,DO2,SEASON,SEADAY
      REAL              LATI,LONGI,MO2,MO,MODIP,NMF2,MAGBR
      REAL              NMF1,NME,NMD,MM,MLAT,MLONG,NOBO2
c      CHARACTER FILNAM*12
      CHARACTER FILNAM*53
      DIMENSION  F(3),RIF(4),E(4),XDELS(4),DNDS(4)
      DIMENSION  FF0(988),XM0(441),F2(13,76,2),FM3(9,49,2)
      DIMENSION  FF0N(988),XM0N(441),F2N(13,76,2),FM3N(9,49,2)
      DIMENSION  AMP(4),HXL(4),SCL(4),B0B1(5)
      DIMENSION  XSM(4),MM(5),DTI(4)
      DIMENSION  AHH(7),STTE(6),DTE(5),ATE(7),TEA(6),HOA(3),XNAR(3)
      DIMENSION  PG1O(80),PG2O(32),PG3O(80),PF1O(12),PF2O(4),PF3O(12)
      DIMENSION  HO(4),MO(5),DDO(4),HO2(2),MO2(3),DO2(2),DION(7)
      DIMENSION  OUTF(11,100),OARR(38),ARIG(3),RZAR(3)
      LOGICAL           EXT,SCHALT,NIGHT,TECON(3),sam_mon,sam_yea
      LOGICAL           F1REG,FOF2IN,HMF2IN,URSIF2,LAYVER,DY,GULB0
      LOGICAL           FOF1IN,HMF1IN,FOEIN,HMEIN,RZIN
      LOGICAL           NODEN,NOTEM,NOION,TENEOP,sam_doy
      LOGICAL           OLD79,JF(20),URSIFO
      COMMON    /BLOCK1/HMF2,NMF2,HMF1  /CONST/UMR      /const1/humr,dumr
     &          /BLOCK2/B0,B1,C1      /BLOCK3/HZ,T,HST,STR
     &          /BLOCK4/HME,NME,HEF   /BLOCK5/NIGHT,E
     &          /BLOCK6/HMD,NMD,HDX   /BLOCK7/D1,XKK,FP30,FP3U,FP1,FP2
     &          /BLOCK8/HS,TNHS,XSM,MM,DTI,MXSM         
     &          /BLOTN/XSM1,TEXOS,TLBDH,SIGMA /BLOTE/AHH,ATE1,STTE,DTE
     &          /BLO10/BETA,ETA,DELTA,ZETA       /ARGEXP/ARGMAX
      EXTERNAL          XE1,XE2,XE3,XE4,XE5,XE6,TEDER
      DATA      B0B1  /.755566,.778596,.797332,.812928,.826146/

CTEST   save    icalls,rssn,rsat,cov,ttt
        save

        DO 7397 KI=1,11
        do 7397 kk=1,100
7397    OUTF(KI,kk)=-1.

        do 8398 kind=7,14,1
8398    oarr(kind)=-1.
        do 8378 kind=18,32,1
8378    oarr(kind)=-1.
        oarr(34)=-1.
        oarr(35)=-1.
        oarr(36)=-1.
        oarr(37)=-1.
        oarr(38)=-1.

C
C PROGRAM CONSTANTS
C
        icalls=icalls+1
        ARGMAX=88.0
        pi=ATAN(1.0)*4.
        UMR=pi/180.
        humr=pi/12.
        dumr=pi/182.5
        ALOG2=ALOG(2.)
        ALG100=ALOG(100.)
        numhei=int(abs(heiend-heibeg)/abs(heistp))+1
        if(numhei.gt.100) numhei=100
C
C Code inserted to aleviate block data problem for PC version.
C Thus avoiding DATA statement with parameters from COMMON block.
C
        HOA(1)=300.
        HOA(2)=400.
        HOA(3)=600.
        XDELS(1)=5.
        XDELS(2)=5.
        XDELS(3)=5.
        XDELS(4)=10.
        DNDS(1)=.016
        DNDS(2)=.01
        DNDS(3)=.016
        DNDS(4)=.016
        DDO(1)=9
        DDO(2)=5
        DDO(3)=5
        DDO(4)=25
        DO2(1)=5
        DO2(2)=5
        XNAR(1)=0.0
        XNAR(2)=0.0
        XNAR(3)=0.0
        AHH(1)=120.
        AHH(2)=0.
        AHH(3)=300.
        AHH(4)=400.
        AHH(5)=600.
        AHH(6)=1400.
        AHH(7)=3000.
        DTE(1)=5.
        DTE(2)=5.
        DTE(3)=10.
        DTE(4)=20.
        DTE(5)=20.
        DTI(1)=10.
        DTI(2)=10.
        DTI(3)=20.
        DTI(4)=20.
C
C FIRST SPECIFY YOUR COMPUTERS CHANNEL NUMBERS ....................
C AGNR=OUTPUT (OUTPUT IS DISPLAYED OR STORED IN FILE OUTPUT.IRI)...
C IUCCIR=UNIT NUMBER FOR CCIR COEFFICIENTS ........................
c set konsol=1 if you do not want the konsol information
c
      MONITO=6
      IUCCIR=10
c web ---- special for web version
c web      KONSOL=1
      KONSOL=6
        if(.not.jf(12)) konsol=13
c
c selection of density and ion composition options ..................
c

      NODEN=(.not.jf(1))
      NOTEM=(.not.jf(2))
      NOION=(.not.jf(3))
      DY=(.not.jf(6))
      LAYVER=(.not.jf(11))
      OLD79=(.not.jf(7))
      GULB0=(.not.jf(4))
c
c rz12 input option ....................................................
c
      RZIN=(.not.jf(17))
       IF(RZIN) THEN
          ARZIN=OARR(33)
        else
          oarr(33)=-1.
          ENDIF
c
c F2 peak density ....................................................
c
      FOF2IN=(.not.jf(8))
       IF(FOF2IN) THEN
          AFOF2=OARR(1)
          IF(AFOF2.GT.100.) AFOF2=SQRT(AFOF2/1.24E10)
        else
          oarr(1)=-1.
          ENDIF
      URSIF2=(.not.jf(5))
c
c F2 peak altitude ..................................................
c
      HMF2IN=(.not.jf(9))
       IF(HMF2IN) then
                AHMF2=OARR(2)
        else
                oarr(2)=-1.
        endif
c
c F1 peak density ....................................................
c
      FOF1IN=(.not.jf(13))
       IF(FOF1IN) THEN
          AFOF1=OARR(3)
          IF(AFOF1.GT.100.) AFOF1=SQRT(AFOF1/1.24E10)
        else
          oarr(3)=-1.
          ENDIF
c
c F1 peak altitude ..................................................
c
      HMF1IN=(.not.jf(14))
       IF(HMF1IN) then
                AHMF1=OARR(4)
                if(.not.layver.and.(konsol.gt.1)) write(konsol,1939)
1939  format(' *Ne* User input of hmF1 is only possible for the LAY-',
     &          'version')
        else
                oarr(4)=-1.
        endif
c
c E peak density ....................................................
c
      FOEIN=(.not.jf(15))
       IF(FOEIN) THEN
          AFOE=OARR(5)
          IF(AFOE.GT.100.) AFOE=SQRT(AFOE/1.24E10)
        else
          oarr(5)=-1.
          ENDIF
c
c E peak altitude ..................................................
c
      HMEIN=(.not.jf(16))
       IF(HMEIN) then
                AHME=OARR(6)
        else
                oarr(6)=-1.
        endif
c
C TE-NE MODEL OPTION ..............................................
C
      TENEOP=(.not.jf(10))
        IF(TENEOP) THEN
           DO 8154 JXNAR=1,3
              XNAR(JXNAR)=OARR(JXNAR+14)
              TECON(JXNAR)=.FALSE. 
8154          IF(XNAR(JXNAR).GT.0.) TECON(JXNAR)=.TRUE. 
        else
           oarr(3)=-1.
           oarr(4)=-1.
           oarr(5)=-1.
           ENDIF
c
c lists the selected options before starting the table
c

      if(icalls.gt.1.or.konsol.eq.1) goto 8201
        write(*,*) '*** IRI parameters are being calculated ***'
      if(NODEN) goto 2889
        if(LAYVER) write(*,*) 'Ne, E-F: The LAY-Version is ',
     &          'prelimenary. Erroneous profile features can occur.'
        if(GULB0) write(*,*) 'Ne, B0: Bottomside thickness is ',
     &          'obtained with Gulyaeva-1987 model.'
        if(OLD79) write(*,*) 'Ne: Using IRI-79. Correction',
     &          ' of equatorial topside is not included.'
        if(FOF2IN) then
                write(*,*) 'Ne, foF2/NmF2: provided by user.'
                goto 2889
                endif
        if(URSIF2) then
                write(*,*) 'Ne, foF2: URSI model is used.'
        else
                write(*,*) 'Ne, foF2: CCIR model is used.'
        endif
2889    continue
        if(HMF2IN) write(*,*) 'Ne, hmF2/M3000F2: provided by user.'
        if(fof1in) write(*,*) 'Ne, foF1/NmF1: provided by user.'
        if(HMF1IN) write(*,*) 'Ne, hmF1: prvided by user.'
        if(foein) write(*,*) 'Ne, foE/NmE: provided by user.'
        if(HMEIN) write(*,*) 'Ne, hmE: prvided by user.'
        if((.not.NOION).and.(DY)) 
     &    write(*,*) 'Ion Com.: Using Danilov et al. 1985/95.'
        if((.not.NOTEM).and.(TENEOP))
     &    write(*,*) 'Te: Temperature-density correlation is used.'
8201    continue

C
C CALCULATION OF GEOG. OR GEOM. COORDINATES IN DEG....................
C CALCULATION OF MAGNETIC INCLINATION (DIP), DECLINATION (DEC)........
C   DIP LATITUDE (MAGBR) AND MODIFIED DIP (MODIP). ALL IN DEGREE......
C
        IF(JMAG.GT.0) THEN
           MLAT=ALATI
           MLONG=ALONG
        ELSE
           LATI=ALATI
           LONGI=ALONG
        ENDIF
        CALL GGM(JMAG,LONGI,LATI,MLONG,MLAT)
        ABSLAT=ABS(LATI)
        CALL FIELDG(LATI,LONGI,300.0,XMA,YMA,ZMA,BET,DIP,DEC,MODIP)
        MAGBR=ATAN(0.5*TAN(DIP*UMR))/UMR
        ABSMLT=ABS(MLAT)
        ABSMDP=ABS(MODIP)
        ABSMBR=ABS(MAGBR)
C
C CALCULATION OF DAY OF YEAR AND SUN DECLINATION......................
C CALCULATION OF UT/LT AND RELATED YEAR, MONTH, DAYNRs ...............
C CALCULATION OF (UT-)SEASON (SUMMER=2, WINTER=4).....................
C
        iyear=iyyyy
        if(iyear.lt.100) iyear=iyear+1900
        if(MMDD.lt.0) then
                DAYNR=-MMDD
                call MODA(1,iyear,MONTH,IDAY,DAYNR,nrdaym)
        else
                MONTH=MMDD/100
                IDAY=MMDD-MONTH*100
                call MODA(0,iyear,MONTH,IDAY,DAYNR,nrdaym)
        endif
c
c lyear,lmonth,lday,ldaynre,lnrday related to LT
c
        lyear=iyear
        lmonth=month
        lday=iday
        ldaynr=daynr
        lnrday=nrdaym

      IF(DHOUR.le.24.0) goto 2619
        UT=DHOUR-25.
        iytmp=iyear
        idtmp=daynr
        if(jf(18)) then
                hour=ut+longi/15.
                if(hour.gt.24.) hour=hour-24.
        else
                call ut_lt(0,ut,hour,longi,iytmp,idtmp)
                if(idtmp.ne.ldaynr) then
                        lyear=iytmp
                        ldaynr=idtmp
                        call MODA(1,lyear,LMONTH,LDAY,LDAYNR,lnrday)
                        endif
        endif
        goto 2629

2619  HOUR=DHOUR
        iytmp=lyear
        idtmp=ldaynr
        if(jf(18)) then
                ut=hour-longi/15.
                if(ut.lt.0) ut=ut+24.
        else
                call ut_lt(1,ut,hour,longi,iytmp,idtmp)
                if(idtmp.ne.daynr) then
                        iyear=iytmp
                        daynr=idtmp
                        call MODA(1,iyear,MONTH,IDAY,DAYNR,nrdaym)
                        endif
        endif
2629  zmonth = lmonth + (lday*1.)/lnrday

      SEASON=INT((DAYNR+45.0)/92.0)
      IF(SEASON.LT.1) SEASON=4
      NSEASN=SEASON
      seaday=daynr
      seamon=zmonth
      IF(LATI.GT.0.0) GOTO 5592
        SEASON=SEASON-2
        IF(SEASON.LT.1) SEASON=SEASON+4
        seamon=zmonth+6.
        if(seamon.ge.13.0) seamon=seamon-12.
        seaday=daynr+183
        if(seaday.gt.366) seaday=seaday-366
C
C CALCULATION OF MEAN F10.7CM SOLAR RADIO FLUX (COV)................
C CALCULATION OF RESTRICTED SOLAR ACTIVITIES (RSAT,COVSAT)..............
C

5592    sam_mon=(month.eq.montho)
        sam_yea=(iyear.eq.iyearo)
        sam_doy=(daynr.eq.idayno)
        if(sam_yea.and.sam_doy) goto 2910
                call tcon(iyear,month,iday,daynr,rzar,arig,ttt,nmonth)
                if(nmonth.lt.0) goto 3330
                if(RZIN) then
                  rrr = arzin
                  rzar(1) = rrr
                  rzar(2) = rrr
                  rzar(3) = rrr
                  zi=-12.349154+(1.4683266-2.67690893e-03*rrr)*rrr
                  if(zi.gt.174.0) zi=174.0
                  arig(1) = zi
                  arig(2) = zi
                  arig(3) = zi
                  endif
        rssn=rzar(3)
        rsat=arig(3)
        COV=63.75+RSSN*(0.728+RSSN*0.00089)
        rlimit=rsat
        COVSAT=63.75+rlimit*(0.728+rlimit*0.00089)
        if(covsat.gt.188.) covsat=188

C
C CALCULATION OF SOLAR ZENITH ANGLE (XHI/DEG).........................
C NOON VALUE (XHINON).................................................
C

2910    CALL SOCO(ldaynr,HOUR,LATI,LONGI,SUNDEC,XHI,SAX,SUX)
        CALL SOCO(ldaynr,12.0,LATI,LONGI,SUNDE1,XHINON,SAXNON,SUXNON)

        NIGHT=.FALSE.
        if(abs(sax).gt.25.0) then
                if(sax.lt.0.0) NIGHT=.TRUE.
                goto 1334
                endif
        if(SAX.le.SUX) goto 1386
                if((hour.gt.sux).and.(hour.lt.sax)) night=.true.
        goto 1334
1386            IF((HOUR.GT.SUX).OR.(HOUR.LT.SAX)) NIGHT=.TRUE.

C
C CALCULATION OF ELECTRON DENSITY PARAMETERS................
C

1334  HNEA=65.
      IF(NIGHT) HNEA=80.
      HNEE=2000.
      IF(NODEN) GOTO 4933
      DELA=4.32
      IF(ABSMDP.GE.18.) DELA=1.0+EXP(-(ABSMDP-30.0)/10.0)
      DELL=1+EXP(-(ABSLAT-20.)/10.)
C!!!!!!! F-REGION PARAMETERS AND E-PEAK !!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(FOEIN) THEN
          FOE=AFOE
        ELSE
          FOE=FOEEDI(COV,XHI,XHINON,ABSLAT)
        ENDIF
        NME=1.24E10*FOE*FOE

        IF(HMEIN) THEN
          HME=AHMF2
        ELSE
          HME=110.0
        ENDIF
C
C READ CCIR AND URSI COEFFICIENT SET FOR CHOSEN MONTH ............
C
      IF((FOF2IN).AND.(HMF2IN)) GOTO 501
      IF(URSIF2.NEQV.URSIFO) GOTO 7797
      IF(sam_mon.AND.(nmonth.EQ.nmono).and.sam_yea) GOTO 4292
      IF(sam_mon) GOTO 4293
c
c the program expects the coefficients files in ASCII format; if you
C want to use the binary version of the coefficients, please use the
C the statements that are commented-out below and comment-out the
C ASCII-related statements.
c
7797    URSIFO=URSIF2
        WRITE(FILNAM,104) MONTH+10
104   	FORMAT('ccir',I2,'.asc')
c-binary- if binary files than use:
c-binary-104   FORMAT('ccir',I2,'.bin')
c-web- special for web-version:
c-web-104   FORMAT('/usr/local/etc/httpd/cgi-bin/models/IRI/ccir',I2,'.asc')
c
        OPEN(IUCCIR,FILE=FILNAM,STATUS='OLD',ERR=8448,
     &          FORM='FORMATTED')
c-binary- if binary files than use:
c-binary-     &          FORM='UNFORMATTED')
c
        READ(IUCCIR,4689) F2,FM3
4689    FORMAT(1X,4E15.8)
c-binary- if binary files than use:
c-binary-        READ(IUCCIR) F2,FM3

c
        CLOSE(IUCCIR)
C
C then URSI if chosen ....................................
C
        if(URSIF2) then
          WRITE(FILNAM,1144) MONTH+10
1144  	  FORMAT('ursi',I2,'.asc')
c-web- special for web-version:
c-web-1144  	  FORMAT('/usr/local/etc/httpd/cgi-bin/models/IRI/ursi',I2,'.asc')
c-binary- if binary files than use:
c-binary-1144  	  FORMAT('ursi',I2,'.bin')

          OPEN(IUCCIR,FILE=FILNAM,STATUS='OLD',ERR=8448,
     &         FORM='FORMATTED')
c-binary- if binary files than use:
c-binary-     &         FORM='UNFORMATTED')

          READ(IUCCIR,4689) F2
c-binary- if binary files than use:
c-binary-          READ(IUCCIR) F2

          CLOSE(IUCCIR)
        endif

C
C READ CCIR AND URSI COEFFICIENT SET FOR NMONTH, i.e. previous 
c month if day is less than 15 and following month otherwise 
C

4293    continue

c
c first CCIR ..............................................
c

        WRITE(FILNAM,104) NMONTH+10
        OPEN(IUCCIR,FILE=FILNAM,STATUS='OLD',ERR=8448,
     &          FORM='FORMATTED')
c-binary- if binary files than use:
c-binary-     &          FORM='unFORMATTED')

        READ(IUCCIR,4689) F2N,FM3N
c-binary- if binary files than use:
c-binary-        READ(IUCCIR) F2N,FM3N

        CLOSE(IUCCIR)

C
C then URSI if chosen .....................................
C
        if(URSIF2) then
          WRITE(FILNAM,1144) NMONTH+10
          OPEN(IUCCIR,FILE=FILNAM,STATUS='OLD',ERR=8448,
     &         FORM='FORMATTED')
c-binary- if binary files than use:
c-binary-     &         FORM='unFORMATTED')

          READ(IUCCIR,4689) F2N
c-binary- if binary files than use:
c-binary-          READ(IUCCIR) F2N

          CLOSE(IUCCIR)
          endif

        nmono=nmonth
        MONTHO=MONTH
        iyearo=iyear
        idayno=daynr
        GOTO 4291
        
8448    WRITE(MONITO,8449) FILNAM
8449    FORMAT(1X////,
     &    ' The file ',A30,'is not in your directory.')
        GOTO 3330
C
C LINEAR INTERPOLATION IN SOLAR ACTIVITY. RSAT used for foF2
C

4291    RR2=ARIG(1)/100.
        RR2N=ARIG(2)/100.
        RR1=1.-RR2
        RR1N=1.-RR2N
        DO 20 I=1,76
        DO 20 J=1,13
        K=J+13*(I-1)
        FF0N(K)=F2N(J,I,1)*RR1N+F2N(J,I,2)*RR2N
20      FF0(K)=F2(J,I,1)*RR1+F2(J,I,2)*RR2

        RR2=RZAR(1)/100.
        RR2N=RZAR(2)/100.
        RR1=1.-RR2
        RR1N=1.-RR2N
        DO 30 I=1,49
        DO 30 J=1,9
        K=J+9*(I-1)
        XM0N(K)=FM3N(J,I,1)*RR1N+FM3N(J,I,2)*RR2N
30      XM0(K)=FM3(J,I,1)*RR1+FM3(J,I,2)*RR2

4292  zfof2  =  FOUT(MODIP,LATI,LONGI,UT,FF0)
      fof2n  =  FOUT(MODIP,LATI,LONGI,UT,FF0N)
      zm3000 = XMOUT(MODIP,LATI,LONGI,UT,XM0)
      xm300n = XMOUT(MODIP,LATI,LONGI,UT,XM0N)

        midm=15
        if(month.eq.2) midm=14
        if (iday.lt.midm) then
                yfof2 = fof2n + ttt * (zfof2-fof2n)
                xm3000= xm300n+ ttt * (zm3000-xm300n)
        else
                yfof2 = zfof2 + ttt * (fof2n-zfof2)
                xm3000= zm3000+ ttt * (xm300n-zm3000)
        endif
501     IF(FOF2IN) THEN
          FOF2=AFOF2
        ELSE
          FOF2=YFOF2
        ENDIF
        NMF2=1.24E10*FOF2*FOF2

        IF(HMF2IN) THEN
          HMF2=AHMF2
          IF(AHMF2.LT.50.0) HMF2=HMF2ED(MAGBR,RSSN,FOF2/FOE,AHMF2)
        ELSE
          HMF2=HMF2ED(MAGBR,RSSN,FOF2/FOE,XM3000)
        ENDIF
c
c topside profile parameters .............................
c
      COS2=COS(MLAT*UMR)
      COS2=COS2*COS2
        FLU=(COVSAT-40.0)/30.0
corr    FLU=(COV-40.0)/30.0
corr    flueta=188.
corr    flumax=(flueta-40.)/30.0
      IF(OLD79) then
        ETA1=-0.0070305*COS2
      else
        EX=EXP(-MLAT/15.)
        EX1=EX+1
        EPIN=4.*EX/(EX1*EX1)
        ETA1=-0.02*EPIN
      endif
      ETA=0.058798+ETA1+FLU*(-0.014065+0.0069724*COS2)+
     &(0.0024287+0.0042810*COS2-0.00015280*FOF2)*FOF2
        fluu=flu
corr    if(fluu.gt.flumax) fluu=flumax
      ZETA=0.078922-0.0046702*COS2-0.019132*FLUU+0.0076545*FLU*COS2+
     &(0.0032513+0.0060290*COS2-0.00020872*FOF2)*FOF2
      BETA=-128.03+20.253*COS2+FLU*(-8.0755-0.65896*COS2)+(0.44041
     &+0.71458*COS2-0.042966*FOF2)*FOF2
        Z=EXP(94.5/BETA)
corr    Z=EXP(94.45/BETA)
      Z1=Z+1
      Z2=Z/(BETA*Z1*Z1)
      DELTA=(ETA/Z1-ZETA/2.0)/(ETA*Z2+ZETA/400.0)
c
c bottomside profile parameters .............................
C
1501    HMF1=HMF2
        HZ=HMF2
        HEF=HME
        B1=3.0
C!!!!!!! INTERPOLATION FOR B0 OUT OF ARRAY B0F !!!!!!!!!!!!!!!!!!!!!
        if(GULB0) then
          call ROGUL(SEADAY,XHI,SEAX,GRAT)
ctest
          if(NIGHT) GRAT=0.91-HMF2/4000.
          B0CNEW=HMF2*(1.-GRAT)
          B0=B0CNEW/B0B1(1)
        else
          B0 = B0_TAB(HOUR,SAX,SUX,NSEASN,RSSN,MODIP)
        endif
C!!!!!!! F1-REGION PARAMETERS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      F1REG=.FALSE.
      HMF1=0.
      PNMF1=0.
      C1=0.  
      IF(NIGHT) GOTO 150
        IF(FOF1IN) THEN
          FOF1=AFOF1
        ELSE
          FOF1=FOF1ED(ABSMBR,RSSN,XHI)
        ENDIF
        IF(FOF1.LT.1.E-3) GOTO 150
          F1REG=.TRUE.
          C1=.09+.11/DELA
          PNMF1=1.24E10*FOF1*FOF1
150   NMF1=PNMF1
C!!!!!!! PARAMETER FOR E AND VALLEY-REGION !!!!!!!!!!!!!!!!!!!!!
      XDEL=XDELS(SEASON)/DELA
      DNDHBR=DNDS(SEASON)/DELA
      HDEEP=HPOL(HOUR,10.5/DELA,28.,SAX,SUX,1.,1.)
      WIDTH=HPOL(HOUR,17.8/DELA,45.+22./DELA,SAX,SUX,1.,1.)
      DEPTH=HPOL(HOUR,XDEL,81.,SAX,SUX,1.,1.)
      DLNDH=HPOL(HOUR,DNDHBR,.06,SAX,SUX,1.,1.)
      IF(DEPTH.LT.1.0) GOTO 600
        IF(NIGHT) DEPTH=-DEPTH
        CALL TAL(HDEEP,DEPTH,WIDTH,DLNDH,EXT,E)
        IF(.NOT.EXT) GOTO 667
        if(konsol.gt.1) WRITE(KONSOL,650)
650     FORMAT(1X,'*NE* E-REGION VALLEY CAN NOT BE MODELLED')
600   WIDTH=.0
667   HEF=HME+WIDTH
      VNER = (1. - ABS(DEPTH) / 100.) * NME
c
c Parameters below E  .............................
c

2727  NMD=XMDED(XHI,RSSN,4.0E8)
      HMD=HPOL(HOUR,81.0,88.0,SAX,SUX,1.,1.)
      F(1)=HPOL(HOUR,0.02+0.03/DELA,0.05,SAX,SUX,1.,1.)
      F(2)=HPOL(HOUR,4.6,4.5,SAX,SUX,1.,1.)
      F(3)=HPOL(HOUR,-11.5,-4.0,SAX,SUX,1.,1.)
      FP1=F(1)
      FP2=-FP1*FP1/2.0
      FP30=(-F(2)*FP2-FP1+1.0/F(2))/(F(2)*F(2))
      FP3U=(-F(3)*FP2-FP1-1.0/F(3))/(F(3)*F(3))
c indermediate region between D and E region; parameters xkk
c and d1 are found such that the function reaches hdx/xdx/dxdh
      HDX=HMD+F(2)
      X=HDX-HMD
      XDX=NMD*EXP(X*(FP1+X*(FP2+X*FP30)))
      DXDX=XDX*(FP1+X*(2.0*FP2+X*3.0*FP30))
      X=HME-HDX
      XKK=-DXDX*X/(XDX*ALOG(XDX/NME))
c if exponent xkk is larger than xkkmax, then xkk will be set to xkkmax and
c d1 will be determined such that the point hdx/xdx is reached; derivative
c is no longer continuous.
        xkkmax=5.
        if(xkk.gt.xkkmax) then
                xkk=xkkmax
                d1=-alog(xdx/nme)/(x**xkk)
        else
                D1=DXDX/(XDX*XKK*X**(XKK-1.0))
        endif
C
C SEARCH FOR HMF1 ..................................................
C

2726  if(LAYVER) goto 6153

924   IF(.not.F1REG) GOTO 380
        XE2H=XE2(HEF)
        CALL REGFA1(HEF,HMF2,XE2H,NMF2,0.001,NMF1,XE2,SCHALT,HMF1)
        IF(.not.SCHALT) GOTO 380
        if(konsol.gt.1) WRITE(KONSOL,11)
11      FORMAT(1X,'*NE* HMF1 IS NOT EVALUATED BY THE FUNCTION XE2')
        IREGFA=1
c
c change B1 and try again ..........................................
c

9244    IF(B1.GT.4.5) GOTO (7398,8922) IREGFA
                B1=B1+0.5
                if(konsol.gt.1) WRITE(KONSOL,902) B1-0.5,B1
902     FORMAT(6X,'CORR.: B1(OLD)=',F4.1,' B1(NEW)=',F4.1)
                IF(GULB0) then
                        ib1=int(b1*2.-5.)
                        B0=B0CNEW/b0b1(ib1)
                        endif
                GOTO 924
c
c omit F1 feature ....................................................
c

7398  if(konsol.gt.1) WRITE(KONSOL,9269)
9269  FORMAT(1X,'CORR.: NO F1 REGION, B1=3, C1=0.0')
        HMF1=0.
        NMF1=0.
        C1=0.0
        B1=3.
        F1REG=.FALSE.

C
C SEARCH FOR HST [NE3(HST)=NME] ..........................................
C

380     RRRR=0.5
        IF(F1REG) then
                hf1=hmf1
                xf1=nmf1
                GOTO 3972
                ENDIF
        RATHH=0.5
3973    hf1=hef+(hmf2-hef)*RATHH
        xf1=xe3(hf1)
        IF(XF1.LT.NME) THEN
                RATHH=RATHH+.1
                GOTO 3973
                ENDIF
3972    h=hf1
        deh=10.
        XXMIN=XF1
        HHMIN=HF1
3895    h=h-deh
        if(h.lt.HEF) then
          h=h+2*deh
          deh=deh/10.
          if(deh.lt.1.) goto 3885
          endif
        XE3H=XE3(h)
        IF(XE3H.LT.XXMIN) then
          XXMIN=XE3H
          HHMIN=h
          endif
        if(XE3H.gt.NME) goto 3895
      CALL REGFA1(h,HF1,XE3H,XF1,0.001,NME,XE3,SCHALT,HST)
        STR=HST
        IF(.not.SCHALT) GOTO 360
3885  if(konsol.gt.1) WRITE(KONSOL,100)
100   FORMAT(1X,'*NE* HST IS NOT EVALUATED BY THE FUNCTION XE3')
        IREGFA=2
        IF(XXMIN/NME.LT.1.3) GOTO 9244
c
c assume linear interpolation between HZ and HEF ..................
c

8922    HZ=HHMIN+(HF1-HHMIN)*RRRR
        XNEHZ=XE3(HZ)
        if(xnehz-nme.lt.0.001) then
          RRRR=RRRR+.1
          GOTO 8922
          endif
        if(konsol.gt.1) WRITE(KONSOL,901) HZ,HEF
901     FORMAT(6X,'CORR.: LIN. APP. BETWEEN HZ=',F5.1,
     &          ' AND HEF=',F5.1)
        T=(XNEHZ-NME)/(HZ-HEF)
        HST=-333.
        GOTO 4933
c
c calculate HZ, D and T ............................................
c

360     HZ=(HST+HF1)/2.0
        D=HZ-HST
        T=D*D/(HZ-HEF-D)
        GOTO 4933
C
C LAY-functions for middle ionosphere
C

6153    IF(HMF1IN) THEN
          HMF1M=AHMF1
        ELSE
          HMF1M=165.+0.6428*XHI
        ENDIF
        HHALF = GRAT * HMF2
        HV1R = HME + WIDTH
        HV2R = HME + HDEEP
        HHMF2 = HMF2
        CALL INILAY(NIGHT,NMF2,NMF1,NME,VNER,HHMF2,HMF1M,HME,
     &                  HV1R,HV2R,HHALF,HXL,SCL,AMP,IIQU)
        IF((IIQU.EQ.1).and.(konsol.gt.1)) WRITE(KONSOL,7733)
7733    FORMAT('*NE* LAY amplitudes found with 2nd choice of HXL(1).')
        IF((IIQU.EQ.2).and.(konsol.gt.1)) WRITE(KONSOL,7722)
7722    FORMAT('*NE* LAY amplitudes could not be found.')

C---------- CALCULATION OF NEUTRAL TEMPERATURE PARAMETER-------

4933  HTA=120.0
      HTE=3000.0
        IF(NOTEM) GOTO 240
      SEC=UT*3600.
      CALL CIRA86(DAYNR,SEC,LATI,LONGI,HOUR,COV,TEXOS,TN120,SIGMA)
        IF(HOUR.NE.0.0) THEN
                iyz=lyear
                idz=ldaynr
                if(jf(18)) then
                        secni=(24.-longi/15)*3600.
                else
                        call ut_lt(1,utni,0.0,longi,iyz,idz)
                        SECNI=utni*3600.
                endif
      CALL CIRA86(DAYNR,SECNI,LATI,LONGI,0.,COV,TEXNI,TN1NI,SIGNI)
        ELSE
      TEXNI=TEXOS
      TN1NI=TN120
      SIGNI=SIGMA
        ENDIF
      TLBDH=TEXOS-TN120
      TLBDN=TEXNI-TN1NI
C
C--------- CALCULATION OF ELECTRON TEMPERATURE PARAMETER--------
C
881   CONTINUE

C !!!!!!!!!! TE(120KM)=TN(120KM) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ATE(1)=TN120

C !!!!!!!!!! TE-MAXIMUM (JICAMARCA,ARECIBO) !!!!!!!!!!!!!!!!!!!!
      HMAXD=60.*EXP(-(MLAT/22.41)**2)+210.
      HMAXN=150.
      AHH(2)=HPOL(HOUR,HMAXD,HMAXN,SAX,SUX,1.,1.)
      TMAXD=800.*EXP(-(MLAT/33.)**2)+1500.
      TMAXN=TN(HMAXN,TEXNI,TLBDN,SIGNI)+20
      ATE(2)=HPOL(HOUR,TMAXD,TMAXN,SAX,SUX,1.,1.)

C !!!!!!!!!! TE(300,400KM)=TE-AE-C !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! TE(1400,3000KM)=TE-ISIS !!!!!!!!!!!!!!!!!!!!!!!!!!!
        DIPLAT=MAGBR
      CALL TEBA(DIPLAT,HOUR,NSEASN,TEA)
      ATE(3)=TEA(1)
      ATE(4)=TEA(2)
      ATE(6)=TEA(3)
      ATE(7)=TEA(4)

C !!!!!!!!!! TE(600KM)=TE-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ETT=EXP(-MLAT/11.35)
      TET=2900.-5600.*ETT/((ETT+1)**2.)
      TEN=839.+1161./(1.+EXP(-(ABSMLT-45.)/5.))
      ATE(5)=HPOL(HOUR,TET,TEN,SAX,SUX,1.5,1.5)

C !!!!!!!!!! OPTION TO USE TE-NE-RELATION !!!!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! AT 300, 400 OR 600 KM  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(TENEOP) THEN
        DO 3395 I=1,3
3395      IF(TECON(I)) ATE(I+2)=TEDE(HOA(I),XNAR(I),-COV)
        ENDIF
C !!!!!!!!!! TE'S ARE CORRECTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! ALSO TE > TN ENFORCED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      TNAHH2=TN(AHH(2),TEXOS,TLBDH,SIGMA)
      IF(ATE(2).LT.TNAHH2) ATE(2)=TNAHH2
      STTE1=(ATE(2)-ATE(1))/(AHH(2)-AHH(1))
      DO 1901 I=2,6
       TNAHHI=TN(AHH(I+1),TEXOS,TLBDH,SIGMA)
       IF(ATE(I+1).LT.TNAHHI) ATE(I+1)=TNAHHI
       STTE2=(ATE(I+1)-ATE(I))/(AHH(I+1)-AHH(I))
       ATE(I)=ATE(I)-(STTE2-STTE1)*DTE(I-1)*ALOG2
1901  STTE1=STTE2
C !!!!!!!!!! GRADIENTS ARE CALCULATED WITH !!!!!!!!!!!!!!!!!!!!
C !!!!!!!!!! CORRECTED REGION BOUNDARIES !!!!!!!!!!!!!!!!!!!!!!
      DO 1902 I=1,6
1902  STTE(I)=(ATE(I+1)-ATE(I))/(AHH(I+1)-AHH(I))
      ATE1=ATE(1)
887   CONTINUE
C
C------------ CALCULATION OF ION TEMPERATURE PARAMETERS--------
C
C !!!!!!!!!! TI(430KM,DAY)=TI-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!!!
      XSM1=430.0
      XSM(1)=XSM1
      Z1=EXP(-0.09*MLAT)
      Z2=Z1+1.
      TID1 = 1240.0 - 1400.0 * Z1 / ( Z2 * Z2 )
      MM(2)=HPOL(HOUR,3.0,0.0,SAX,SUX,1.,1.)
C !!!!!!!!!!  TI < TE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        TED1=TEA(6)+30.
        IF(TID1.GT.TED1) TID1=TED1

C !!!!!!!!!! TI(430KM,NIGHT)=TI-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!
      Z1=ABSMLT
      Z2=Z1*(0.47+Z1*0.024)*UMR
      Z3=COS(Z2)
      TIN1=1200.0-300.0*SIGN(1.0,Z3)*SQRT(ABS(Z3))
C !!!!!!!!!! TN < TI < TE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        TEN1=TEA(5)
        TNN1=TN(XSM1,TEXNI,TLBDN,SIGNI)
        IF(TEN1.LT.TNN1) TEN1=TNN1
        IF(TIN1.GT.TEN1) TIN1=TEN1
        IF(TIN1.LT.TNN1) TIN1=TNN1

C !!!!!!!!!! TI(430KM,LT) FROM STEP FUNCTION !!!!!!!!!!!!!!!!!!
        TI1=TIN1  
        IF(TID1.GT.TIN1) TI1=HPOL(HOUR,TID1,TIN1,SAX,SUX,1.,1.)

C !!!!!!!!!! TANGENT ON TN DETERMINES HS !!!!!!!!!!!!!!!!!!!!!!
        TI13=TEDER(130.)
        TI50=TEDER(500.)
      CALL REGFA1(130.0,500.0,TI13,TI50,0.01,TI1,TEDER,SCHALT,HS)
      IF(SCHALT) HS=200.
      TNHS=TN(HS,TEXOS,TLBDH,SIGMA)
      MM(1)=DTNDH(HS,TEXOS,TLBDH,SIGMA)
      IF(SCHALT) MM(1)=(TI1-TNHS)/(XSM1-HS)
      MXSM=2

C !!!!!!!!!! XTETI ALTITTUDE WHERE TE=TI !!!!!!!!!!!!!!!!!!!!!!
2391    XTTS=500.
        X=500.
2390    X=X+XTTS
        IF(X.GE.AHH(7)) GOTO 240
        TEX=ELTE(X)
        TIX=TI(X)
        IF(TIX.LT.TEX) GOTO 2390
        X=X-XTTS
        XTTS=XTTS/10.
        IF(XTTS.GT.0.1) GOTO 2390
        XTETI=X+XTTS*5.

C !!!!!!!!!! TI=TE ABOVE XTETI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        MXSM=3
        MM(3)=STTE(6)
        XSM(2)=XTETI
        IF(XTETI.GT.AHH(6)) GOTO 240
        MXSM=4
        MM(3)=STTE(5)
        MM(4)=STTE(6)
        XSM(3)=AHH(6)
        IF(XTETI.GT.AHH(5)) GOTO 240
        MXSM=5
        DTI(1)=5.
        DTI(2)=5.
        MM(3)=STTE(4)
        MM(4)=STTE(5)
        MM(5)=STTE(6)
        XSM(3)=AHH(5)
        XSM(4)=AHH(6)
C
C CALCULATION OF ION DENSITY PARAMETER..................
C
240   IF(NOION) GOTO 141
      HNIA=100.
        if(DY) hnia=75
      HNIE=2000.
C
C INPUT OF THE ION DENSITY PARAMETER ARRAYS PF1O,PF2O AND PF3O......
C
      RIF(1)=2.
      IF(ABSLAT.LT.30.0) RIF(1)=1.
      RIF(2)=2.
      IF(COV.LT.100.0) RIF(2)=1.
      RIF(3)=SEASON
      IF(SEASON.EQ.1) RIF(3)=3.
      RIF(4)=1.
      IF(NIGHT) RIF(4)=2.
      CALL KOEFP1(PG1O)
      CALL KOEFP2(PG2O)
      CALL KOEFP3(PG3O)
      CALL SUFE(PG1O,RIF,12,PF1O)
      CALL SUFE(PG2O,RIF, 4,PF2O)
      CALL SUFE(PG3O,RIF,12,PF3O)
c
c calculate O+ profile parameters
c
      IF(ABS(XHI).LE.90.0) THEN
        ZZZ1=COS(XHI*UMR)
      ELSE
        ZZZ1=0.0
      ENDIF
      msumo=4
      RDOMAX=100.0
      MO(1)=EPSTEP(PF1O(1),PF1O(2),PF1O(3),PF1O(4),ZZZ1)
      MO(2)=EPSTEP(PF1O(5),PF1O(6),PF1O(7),PF1O(8),ZZZ1)
      MO(3)=0.0
      HO(1)=EPSTEP(PF1O(9),PF1O(10),PF1O(11),PF1O(12),ZZZ1)
      HO(2)=290.0
      IF((RIF(2).EQ.2.).AND.(RIF(3).EQ.2.)) HO(2)=237.0
      HO(4)=PF2O(1)
        ho05=pf2o(4)
      MO(4)=PF2O(2)
      MO(5)=PF2O(3)
c
c adjust gradient MO(4) of O+ profile segment above F peak
c
7100    HO(3)=(ALG100-MO(5)*(HO(4)-ho05))/MO(4)+HO(4)
        IF(HO(3).LE.HO(2)+20.) THEN
                MO(4)=MO(4)-0.001
                GOTO 7100
                endif
        hfixo=(ho(2)+ho(3))/2.
c
c find height H0O of maximum O+ relative density
c
      DELX=5.0
      X=HO(2)
      YMAXX=0.0
7102  X=X+DELX
      Y=RPID(X,HFIXO,RDOMAX,msumo,MO,DDO,HO)
      IF(Y.LE.YMAXX) then
        if(delx.le.0.1) GOTO 7104
        x=x-delx
        delx=delx/5.
      ELSE
        YMAXX=Y
      ENDIF
      GOTO 7102 
7104  H0O=X-DELX/2.
7101    if(y.lt.100.0) goto 7103
          rdomax=rdomax-0.01
        y=rpid(h0o,hfixo,rdomax,msumo,mo,ddo,ho)
        goto 7101
7103    yo2h0o=100.-y
        yoh0o=y
c
c calculate parameters for O2+ profile
c
        hfixo2  = pf3o(1)
        rdo2mx = pf3o(2) 
      DO 7105 L=1,2
                I = L * 2
                HO2(L)=PF3O(1+I)+PF3O(2+I)*ZZZ1
7105            MO2(L+1)=PF3O(7+I)+PF3O(8+I)*ZZZ1
      MO2(1)=PF3O(7)+PF3O(8)*ZZZ1
        if(hfixo2.gt.ho2(1)) then
           ymo2z=mo2(2)
        else
           ymo2z=mo2(1)
        endif
        aldo21=alog(rdo2mx)+ymo2z*(ho2(1)-hfixo2)
        hfixo2=(ho2(2)+ho2(1))/2.
        rdo2mx=exp(aldo21+mo2(2)*(hfixo2-ho2(1)))
c
c make sure that rd(O2+) is less or equal 100-rd(O+) at O+ maximum
c
7106  Y=RPID(H0O,hfixo2,rdo2mx,2,MO2,DO2,HO2)
      IF(Y.GT.yo2h0o) then
        MO2(3)=MO2(3)-0.02
        GOTO 7106
        endif
c
C use ratio of NO+ to O2+ density at O+ maximum to calculate
c NO+ density above the O+ maximum (H0O)
c
      IF(y.LT.1.) then
        NOBO2=0.0
      ELSE
        NOBO2= (yo2h0o-y)/y
      ENDIF
C
C CALCULATION FOR THE REQUIRED HEIGHT RANGE.......................
C
141   IF(.NOT.F1REG) HMF1=HZ

        height=heibeg
        kk=1

300   IF(NODEN) GOTO 330
      IF((HEIGHT.GT.HNEE).OR.(HEIGHT.LT.HNEA)) GOTO 330
        IF(LAYVER) THEN
          ELEDE=-9.
          IF(IIQU.LT.2) ELEDE=XEN(HEIGHT,HMF2,NMF2,HME,4,HXL,SCL,AMP)
        ELSE
          ELEDE=XE(HEIGHT)
        ENDIF
        OUTF(1,kk)=ELEDE
330   IF(NOTEM) GOTO 7108
      IF((HEIGHT.GT.HTE).OR.(HEIGHT.LT.HTA)) GOTO 7108
        TNH=TN(HEIGHT,TEXOS,TLBDH,SIGMA)
        TIH=TNH
        IF(HEIGHT.GE.HS) TIH=TI(HEIGHT)
        TEH=ELTE(HEIGHT)
        IF(TIH.LT.TNH) TIH=TNH
        IF(TEH.LT.TIH) TEH=TIH
        OUTF(2,kk)=TNH
        OUTF(3,kk)=TIH
        OUTF(4,kk)=TEH
7108  IF(NOION) GOTO 7118
      IF((HEIGHT.GT.HNIE).OR.(HEIGHT.LT.HNIA)) GOTO 7118
        if(DY) then
c      call IONCOM(HEIGHT,XHI,LATI,COV,seamon,DION)
      call ioncom_new(height,xhi,lati,cov,seamon,dion)
      ROX=DION(1)
      RHX=DION(2)
      RNX=DION(3)
      RHEX=DION(4)
      RNOX=DION(5)
      RO2X=DION(6)
      RCLUST=DION(7)
        else
      ROX=RPID(HEIGHT,HFIXO,RDOMAX,msumo,MO,DDO,HO)
      RO2X=RPID(HEIGHT,HFIXO2,rdo2mx,2,MO2,DO2,HO2)
      CALL RDHHE(HEIGHT,H0O,ROX,RO2X,NOBO2,10.,RHX,RHEX)
      RNOX=RDNO(HEIGHT,H0O,RO2X,ROX,NOBO2)
      RNX=-1.
      RCLUST=-1.
        endif
c
c ion densities are given in percent of total electron density;
c to get ion densities in cm-3 use the following statement
c        xnorm=elede/100.
        xnorm=1.
      OUTF(5,kk)=ROX*xnorm
      OUTF(6,kk)=RHX*xnorm
      OUTF(7,kk)=RHEX*xnorm
      OUTF(8,kk)=RO2X*xnorm
      OUTF(9,kk)=RNOX*xnorm
      OUTF(10,kk)=RCLUST*xnorm
      OUTF(11,kk)=RNX*xnorm

7118    height=height+heistp
        kk=kk+1
        if(kk.le.numhei) goto 300
C
C ADDITIONAL PARAMETER FIELD OARR
C
        IF(NODEN) GOTO 6192
      OARR(1)=NMF2
      OARR(2)=HMF2
      OARR(3)=NMF1
      OARR(4)=HMF1
      OARR(5)=NME
      OARR(6)=HME
      OARR(7)=NMD
      OARR(8)=HMD
      OARR(9)=HHALF
      OARR(10)=B0
      OARR(11)=VNER
      OARR(12)=HEF
6192    IF(NOTEM) GOTO 6092
      OARR(13)=ATE(2)
      OARR(14)=AHH(2)
      OARR(15)=ATE(3)
      OARR(16)=ATE(4)
      OARR(17)=ATE(5)
      OARR(18)=ATE(6)
      OARR(19)=ATE(7)
      OARR(20)=ATE(1)
      OARR(21)=TI1
      OARR(22)=XTETI
6092  OARR(23)=XHI
      OARR(24)=SUNDEC
      OARR(25)=DIP
      OARR(26)=MAGBR
      OARR(27)=MODIP
      OARR(28)=DELA
      OARR(29)=SAX
      OARR(30)=SUX
      OARR(31)=SEASON
      OARR(32)=NSEASN
      OARR(33)=rssn
      OARR(34)=COV
      OARR(35)=B1
      OARR(36)=xm3000
3330  CONTINUE
      RETURN
      END
c
        subroutine iri_web(jmag,jf,alati,along,iyyyy,mmdd,iut,dhour,
     &          height,h_tec_max,ivar,vbeg,vend,vstp,a,b)
c----------------------------------------------------------------
c changes:
c       11/16/99 jf(20) instead of jf(17)
c
c----------------------------------------------------------------
c input:        jmag,alati,along,iyyyy,mmdd,dhour  see IRIS12
c               height  height in km
c               h_tec_max  =0 no TEC otherwise upper boundary for integral
c               iut     =1 for UT       =0 for LT
c               ivar    =1      altitude
c                       =2,3    latitude,longitude
c                       =4,5,6  year,month,day
c                       =7      day of year
c                       =8      hour (UT or LT)
c               vbeg,vend,vstp  variable range (begin,end,step)
c output:       a       similar to outf in IRIS12
c               b       similar to oarr in IRIS12
c
c
c               numstp  number of steps; maximal 100
c----------------------------------------------------------------
        dimension outf(11,100),oar(38),oarr(38),a(11,100),b(38,100)
        dimension       xvar(8)
        logical         jf(20)


        numstp=int((vend-vbeg)/vstp)+1
        if(numstp.gt.100) numstp=100

            do 6249 i=1,38
6249            oar(i)=b(i,1) 

        if(ivar.eq.1) then
            do 1249 i=1,38
1249            oarr(i)=oar(i) 
            xhour=dhour+iut*25.
            call IRIS13(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,XHOUR,
     &                  VBEG,VEND,VSTP,a,OARR)
            if(h_tec_max.gt.50.) then 
                call iri_tec (50.,h_tec_max,2,tec,tect,tecb)
                oarr(37)=tec
                oarr(38)=tect
                endif
            do 1111 i=1,38
1111            b(i,1)=oarr(i)
            return
            endif

        if(height.le.0.0) height=100
        xvar(2)=alati
        xvar(3)=along
        xvar(4)=iyyyy
        xvar(5)=mmdd/100
        xvar(6)=mmdd-xvar(5)*100
        xvar(7)=abs(mmdd*1.)
        xvar(8)=dhour

        xvar(ivar)=vbeg

        alati=xvar(2)
        along=xvar(3)
        iyyyy=int(xvar(4))
        if(ivar.eq.7) then
                mmdd=-int(vbeg)
        else
                mmdd=int(xvar(5)*100+xvar(6))
        endif
        dhour=xvar(8)+iut*25.

        do 1 i=1,numstp
                do 1349 iii=1,38
1349                    oarr(iii)=oar(iii) 
                call IRIS13(JF,JMAG,ALATI,ALONG,IYYYY,MMDD,DHOUR,
     &                  height,height,1.,OUTF,OARR)
                if(h_tec_max.gt.50.) then
                        call iri_tec (50.,h_tec_max,2,tec,tect,tecb)
                        oarr(37)=tec
                        oarr(38)=tect
                        endif
                do 2 ii=1,11
2                       a(ii,i)=outf(ii,1)
                do 2222 ii=1,38
2222                    b(ii,i)=oarr(ii)

                xvar(ivar)=xvar(ivar)+vstp

                alati=xvar(2)
                along=xvar(3)
                iyyyy=int(xvar(4))
                if(ivar.eq.7) then
                        mmdd=-xvar(7)
                else
                        mmdd=int(xvar(5)*100+xvar(6))
                endif
                dhour=xvar(8)+iut*25.
1       continue
        return
        end
