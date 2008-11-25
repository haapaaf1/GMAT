C---------------------------------------------------21 JANUARY 1996
        subroutine LSTID(FI,L,ICEZ,R,AE,TM,NDAY,NDAYS,UTS70,SAX,SUX,
     *DF0F2,DHF2)

C***********************************************************************
C
C   THE COMPUTER PROGRAM FOR UPDATING THE IRI MODEL FOR EFFECTS OF
C   THE LARGE SCALE TRAVELLING DISTURBANCES GENERATING DURING SUBSTORM
C   AND SC"s.
C
C            Institute of Terrestrial Magnetism,Ionosphere and Radio
C            Wave Propagation, Russian Academy of Sciences,
C            142092, Troitsk, Moscow Region, Russia
C***********************************************************************
C
C***       INPUT PARAMETERS:
C       FI- GEOMAGNETIC LATITUDE,
C       L-  GEOGRAFICAL LONGITUDE,
C       ICEZ-INDEX OF SEASON(1-WINTER AND EQUINOX,2-SUMMER),
C       R- WOLF NUMBER,
C       AE- MAXIMUM AE-INDEX REACHED DURING SUBSTORM,
C       TM- LOCAL TIME,
C       NDAY- DAY OF YEAR FOR GIVEN DATE
C       NDAYS- A SET OF DAYS OF YEAR WITH SUBSTORMS
C       UTS70- UT (hrs) OF SUBSTORM ONSETS
C       SAX,SUX- TIME OF SUNSET AND SUNRISE,
C***      OUTPUT PARAMETERS:
C       DF0F2,DHF2- CORRECTIONS TO foF2 AND hmF2 FROM IRI OR
C                   OBSERVATIONAL MEDIANS  OF THOSE VALUES.
C*****************************************************************
        INTEGER ICEZ
	REAL L
        REAL A(5,2,3,2),B(5,2,3,2),C(5,2,3,2),D(5,2,3,2),A1(5,2,3,2),
     *  B1(5,2,3,2),C1(5,2,3,2),D1(5,2,3,2),Y1(60),Y2(60),Y3(60),
     *  Y4(60),YF1(60),YF2(60),YF3(60)

C------------------------------------------------

        DATA Y1/
     *207.8,140.7,158.3,87.2,158.,
     *207.8,140.7,158.3,87.2,158.,
     *183.5,144.2,161.4,151.9,272.4,
     *183.5,144.2,161.4,151.9,272.4,
     *170.6,122.3,139.,79.6,180.6,
     *170.6,122.3,139.,79.6,180.6,
     *381.9,20.1,75.1,151.2,349.5,
     *311.2,241.,187.4,230.1,168.7,
     *294.7,181.2,135.5,237.7,322.,
     *150.2,136.3,137.4,177.,114.,
     *337.8,155.5,157.4,196.7,161.8,
     *159.8,165.6,137.5,132.2,94.3/
        DATA Y2/
     *1.57,2.02,2.12,1.46,2.46,
     *1.57,2.02,2.12,1.46,2.46,
     *1.68,1.65,2.09,2.25,2.82,
     *1.68,1.65,2.09,2.25,2.82,
     *1.41,1.57,1.51,1.46,2.2,
     *1.41,1.57,1.51,1.46,2.2,
     *3.21,3.31,2.61,2.82,2.34,
     *3.32,3.33,2.96,3.43,2.44,
     *2.37,2.79,2.26,3.4,2.28,
     *2.22,1.98,2.33,3.07,1.56,
     *3.3,2.99,3.57,2.98,3.02,
     *1.66,2.04,1.91,1.49,0.43/
        DATA Y3/
     *-1.42,-1.51,-1.53,-1.05,-1.66,
     *-1.42,-1.51,-1.53,-1.05,-1.66,
     *-1.46,-1.39,-1.53,-1.59,-1.9,
     *-1.46,-1.39,-1.53,-1.59,-1.9,
     *-1.41,-1.09,-1.22,-0.84,-1.32,
     *-1.41,-1.09,-1.22,-0.84,-1.32,
     *-2.08,-1.80,-1.35,-1.55,-1.79,
     *-2.08,-2.16,-1.86,-2.19,-1.70,
     *-1.57,-1.62,-1.19,-1.89,-1.47,
     *-1.26,-1.23,-1.52,-1.89,-1.02,
     *-1.76,-1.43,-1.66,-1.54,-1.24,
     *-1.09,-1.23,-1.11,-1.14,-0.4/
        DATA Y4/
     *-5.,0.,0.,0.,2.,
     *-5.,0.,0.,0.,2.,
     *6.,0.,1.,5.,2.,
     *6.,0.,1.,5.,2.,
     *-3.,-6.,2.,2.,3.,
     *-3.,-6.,2.,2.,3.,
     *-11.,-6.,0.,-5.,-6.,
     *1.,4.,-6.,-2.,1.,
     *-10.,0.,-8.,10.,-16.,
     *-7.,-2.,-2.,4.,2.,
     *-13.,0.,0.,7.,0.,
     *-1.,-5.,-7.,4.,-4./
        DATA YF1/
     *-1.03,-1.03,-1.73,-1.46,-1.98,
     *-1.03,-1.03,-1.73,-1.46,-1.98,
     *-1.64,-1.01,-1.55,-1.55,-1.54,
     *-1.64,-1.01,-1.55,-1.55,-1.54,
     *-1.93,-1.60,-1.00,-1.00,-2.90,
     *-1.93,-1.60,-1.00,-1.00,-2.90,
     *-2.86,-1.00,-1.10,-1.73,-2.74,
     *-2.43,-1.40,-1.29,-1.67,-1.50,
     *-1.88,-0.82,-1.44,-2.14,-2.81,
     *-2.73,-2.50,-2.63,-1.19,-1.21,
     *-2.00,-1.24,-1.12,-1.51,-2.28,
     *-2.75,-2.41,-2.33,-3.11,-1.50/
	DATA YF2/
     *1.63,1.63,2.97,2.60,2.84,
     *1.63,1.63,2.97,2.60,2.84,
     *1.34,2.20,2.90,2.90,1.89,
     *1.34,2.20,2.90,2.90,1.89,
     *1.21,1.98,1.20,1.20,3.20,
     *1.21,1.98,1.20,1.20,3.20,
     *1.27,1.18,2.21,2.43,1.56,
     *2.58,2.40,2.50,3.70,2.06,
     *1.17,1.04,-1.56,2.18,0.84,
     *1.98,3.42,3.31,2.21,2.80,
     *0.85,0.87,2.08,1.23,1.30,
     *1.33,2.43,2.15,3.78,1.75/
  	DATA YF3/
     *-1.17,-1.17,-1.84,-1.70,-1.66,
     *-1.17,-1.17,-1.84,-1.70,-1.66,
     *-1.14,-1.30,-1.60,-1.60,-1.16,
     *-1.14,-1.30,-1.60,-1.60,-1.16,
     *-1.13,-1.20,-0.71,-0.71,-1.70,
     *-1.13,-1.20,-0.71,-0.71,-1.70,
     *-1.53,-1.04,-1.34,-1.50,-1.21,
     *-1.61,-1.48,-1.50,-2.20,-1.35,
     *-1.11,-0.77,-1.11,-1.34,-1.08,
     *-1.45,-1.87,-2.03,-1.28,-1.45,
     *-1.02,-0.58,-1.01,-0.67,-0.66,
     *-1.01,-1.43,-1.44,-1.88,-0.98/

        IF(AE.LT.500.)THEN
        WRITE(*,*)'LSTID TAKE PLACE FOR AE>500.'
        GOTO 004
        ENDIF

	IF(FI.LT.0.)FI=ABS(FI)
	IF(FI.LT.15. .OR. FI.GT.65.)THEN
        WRITE(*,*)'LSTID TAKE PLACE FOR 15<FI<65'
	GOTO 004
	END IF

	IF(NDAYS.GT.NDAY .OR. NDAY.GT. NDAYS+1)THEN
	WRITE(*,*)'LSTIDs are absent in case:'
        WRITE(*,*)'NDAYS  .GT. NDAY .OR. NDAY .GT. NDAYS+1'
        GOTO 004
	END IF

	TS70=UTS70+L/15.
	IF(TS70.GT.24.) TS70=TS70-24.
        INN=0
	IF(NDAY.GT.NDAYS) INN=1

        TS=TS70+(-1.5571*FI+109.)/60.
	IF(TS.GT.24.) TS=TS-24.
        IF(TS.LT.SUX.AND.TS.GT.SAX)THEN
        WRITE(*,*)'LSTIDS take place only at night time.'
        GOTO 004
        ENDIF
	TM1=TM
        IF(INN.EQ.1)TM1=TM+24.

        IF(TS.GT.TM1.OR.TS.LT.TM1-5.)THEN
        WRITE(*,*)'LSTIDare only if  TM-5.<TS<TM ;Here TS=',TS,'TM=',TM
        GOTO 004
	END IF


        N=0
        DO 001 M=1,2
        DO 001 K=1,3
        DO 001 J=1,2
        DO 001 I=1,5
        N=N+1
        A(I,J,K,M)=Y1(N)
        B(I,J,K,M)=Y2(N)
        C(I,J,K,M)=Y3(N)
        D(I,J,K,M)=Y4(N)
	A1(I,J,K,M)=YF1(N)
	B1(I,J,K,M)=YF2(N)
	C1(I,J,K,M)=YF3(N)
 001	D1(I,J,K,M)=0.

	DO II=1,5
	FI1=15.+10.*(II-1)
	IF(FI.GE.FI1 .AND. FI.LT.FI1+10.) GOTO 11
	END DO

 11	FI2=FI1+10.

	I=II

        IF(ICEZ.EQ.2)J=2
        IF(ICEZ.EQ.1)J=1
        IF(AE.GE.500. .AND. AE.LT.750.)K=1
        IF(AE.GE.750. .AND. AE.LT.1000.)K=2
        IF(AE.GE.1000.)K=3
        M=-1
        IF(R.LE.20.)M=1
        IF(R.GE.120.)M=2
        T=TM1-TS
        IF(M.LT.0)GOTO 003
C------ for F0F2 & HMF2 VARIATIONS ----------
	CALL CROSHM(1,TM1-TS,I,J,K,M,A1,B1,C1,D1,FI,FI1,FI2,DF0F2)
        CALL CROSHM(2,TM1-TS,I,J,K,M,A,B,C,D,FI,FI1,FI2,DHF2)
        GOTO 005
C--------------------------------------------------------

 003    CALL CROSHM(1,TM1-TS,I,J,K,1,A1,B1,C1,D1,FI,FI1,FI2,DF1)
	CALL CROSHM(1,TM1-TS,I,J,K,2,A1,B1,C1,D1,FI,FI1,FI2,DF2)
	DF0F2=DF1+(DF2-DF1)*(R-20.)/100.
C---------------------------------------
        CALL CROSHM(2,TM1-TS,I,J,K,1,A,B,C,D,FI,FI1,FI2,DH1)
        CALL CROSHM(2,TM1-TS,I,J,K,2,A,B,C,D,FI,FI1,FI2,DH2)
        DHF2=DH1+(DH2-DH1)*(R-20.)/100.
        GOTO 005
 004    DHF2=0.
        DF0F2=0.

 005    CONTINUE
        RETURN
        END


	SUBROUTINE CROSHM(IPARAM,T,I,J,K,M,A,B,C,D,FI,FI1,FI2,DHF2)
C----    ‹€‚›‰ ‹ˆ…‰›‰ ……•Ž„ „‹Ÿ hmF2, foF2 Œ…†„“ ˜ˆŽ’ ‡Ž€Œˆ
C	IPARAM=1 „‹Ÿ foF2 ¨ IPARAM=2 ¤«ï hmF2
C-------------------------------------------------------
	REAL A(5,2,3,2),B(5,2,3,2),C(5,2,3,2),D(5,2,3,2)

        DHF2A=A(I,J,K,M)*(T**B(I,J,K,M))*EXP(C(I,J,K,M)*T)+D(I,J,K,M)
	IF(FI.LE.20. .OR. FI.GE.60.)THEN
	DHF2=DHF2A
	IF(IPARAM.EQ.1 .AND. DHF2.GT.0.)DHF2=0.
	IF(IPARAM.EQ.2 .AND. DHF2.LT.0.)DHF2=0.
	GOTO 1
	END IF

	IF(FI.GE.FI1+5) THEN
	I1=I+1
	FI1N=FI1+5.
	FI2N=FI2+5.
	ELSE
	I1=I-1
	FI1N=FI1-5.
	FI2N=FI2-5.
	END IF

	DHF2B=A(I1,J,K,M)*(T**B(I1,J,K,M))*EXP(C(I1,J,K,M)*T)+
     *  D(I1,J,K,M)

	IF(FI.GE.FI1+5)THEN
	DHF2=DHF2A*(1.-(FI-FI1N)/10.)+DHF2B*(1.-(FI2N-FI)/10.)
	ELSE
	DHF2=DHF2B*(1.-(FI-FI1N)/10.)+DHF2A*(1.-(FI2N-FI)/10.)
	END IF
	IF(IPARAM.EQ.1 .AND. DHF2.GT.0.)DHF2=0.
	IF(IPARAM.EQ.2 .AND. DHF2.LT.0.)DHF2=0.
 1	CONTINUE
	RETURN
	END

        subroutine ddfh(nday,SLT,UT,lati,longi,
     *        f107m,f107,AP,dfof2n,dhmf2)
C***********************************************************************
C   THE COMPUTER PROGRAM FOR UPDATING THE IRI MODEL FOR EFFECTS OF
C   THE IONOSPHERIC STORM
C
C            Institute of Terrestrial Magnetism,Ionosphere and Radio
C            Wave Propagation, Russian Academy of Sciences,
C            142092, Troitsk, Moscow Region, Russia
C***********************************************************************
      integer  nday
      REAL AP(7)
      real UT,SLT, lati, longi, f107m, f107,dfof2n, dhmf2,f2cof
      real k0, k1, Kpo, kt, hmcof, utsec, To, denqO, denqN2,
     * fof2a, hma, Td, dendO, dendN2, fof2b, hmb, TT
      real den(4), temp(2)
      real sw9
      common/csw/sw9
      real exp, alog
      data k1,Kpo,kt,hmcof,f2cof/0.65, 2.0, 3.5, 0.02,1./
      k0 = 16./28.
      utsec = int(UT) * 3600.0
      sw9 = 1
	SAP1=AP(1)
      Ap(1) = (exp(4*Kpo/7.) - 1./(1. + 2*Kpo))*exp(1.0)
      call gts3a(nday,utsec,lati,longi,SLT,f107m,f107,
     *          Ap,den,temp)
      To = temp(1)
      denqO = den(1)
      denqN2 = den(2)
      fof2a = (denqO/(To*To*denqN2**k0))**k1
      hma = denqO*denqN2
      AP(1)=SAP1
      sw9 = -1
      call gts3a(nday,utsec,lati,longi,SLT,f107m,f107,
     *          Ap,den,temp)
      Td = temp(1)
      dendO = den(1)
      dendN2 = den(2)
      fof2b = (dendO/(Td*Td*dendN2**k0))**k1
      hmb = dendO*dendN2
      TT = (Td/To)**kt
      dfoF2n = f2cof * (fof2b - fof2a) / fof2a
      dhmF2 = hmcof * Td * alog(hmb*TT/hma)
      return
      end
      subroutine dayres(day,month,year,iday,imonth,iyear)
      integer day, month, year, iday, imonth, iyear
      integer dayinm(12)
      data dayinm/31,28,31,30,31,30,2*31,30,31,30,31/
      iday = day - 1
      if (iday.eq.0) then
          imonth = month - 1
          if (imonth.eq.0) then
              iday = 31
              imonth = 12
              iyear = year - 1
          else
              iyear = year
              iday = dayinm(imonth)
              if (imonth.eq.2) iday = iday + year/4 - (year-1)/4
          endif
      else
          imonth = month
          iyear = year
      endif
      return
      end
      subroutine nnday(day,month,year,nday)
      integer day,month,year,nday
      integer int
      nnd=31*(month-1)+day
      nday=nnd
      if(month.le.2) return
      leap=int(year/4.)-int((year-1.)/4.)
      nday=nnd-int(0.4*month+2.3)+leap
      return
      end
      subroutine gts3a(iyd,sec,glat,glong,stl,f107a,f107,ap,d,t)
c        MSIS-86/CIRA 1986 Neutral Thermosphere Model
c         A.E.Hedin 3/15/85;2/26/87 (Variable Names Shortened)
c
c     .. scalar arguments ..
      integer iyd
      real sec, glat, glong, stl, f107a, f107
c     .. arrays arguments ..
      real ap(7), d(3), t(2)
c     .. local scalars ..
      real alt, yrd, tinf, za, t0, tlb, z0, g0, s, tr12, g28, xmm,
     * db28, dd, zh28, zhm28, xmd, b28, g16, db16, zh16, b16, dm16,
     * zhm16, rl, hc16, zc16, hcc16, zcc16, rc16, g32, db32
c     .. scalars in common ..
      real sw9, tinfg, gb, rout, gsurf,re,rgas,aem
c     .. arrays in common ..
      real ptm(8), pdm(8,7), pt(150), pd(150,3), ps(150), pdl(25,2),
     * tt(15)
      common/lower5/ptm,pdm
      common/parm3/pt,pd,ps,pdl
      common/csw/sw9
      common/ttest/tinfg,gb,rout,tt
      common/parmb/gsurf,re,rgas,aem
c     .. functione references ..
      real globe5, glob5l, exp, denss, dnet, alog, ccor
c
      data alt/300./
      yrd = iyd
c       eq. a7
      tinf = ptm(1)*(1.+globe5(yrd,sec,glat,glong,stl,f107a,f107,
     $ ap,pt))*pt(1)
      za = ptm(5)*pdl(16,2)
c       eq. a9
      t0 = ptm(3)*pd(76,2)*(1.+glob5l(pd(76,2)))
c       eq. a8
      tlb = ptm(2)*(1.+glob5l(pd(26,2)))*pd(26,2)
c       eq. a10
      z0 = ptm(7)*(1.+glob5l(pd(51,2)))*pd(51,2)
c       eq. a6
      g0 = ptm(4)*ps(1)
     $ *(1.+globe5(yrd,sec,glat,glong,stl,f107a,f107,
     $ ap,ps))
c       eq. a5
      s = g0/(tinf-tlb)
c       eq. a11
      tr12 = pd(101,2)*(1.+glob5l(pd(101,2)))
      t(1) = tinf
c       eq. a18  n2
      g28 = glob5l(pd(1,2))
      yrd = iyd
      t(1) = tinf
      xmm = pdm(5,3)
c
c       **** N2 DENSITY ****
c
c       eq. a18
      db28 = pdm(1,3)*exp(g28)*pd(1,2)
c       eq. a13 - a17
      d(2)=denss(alt,db28,tinf,tlb, 28.,0.,t(2),ptm(6),s,t0,za,z0,tr12)
      dd = d(2)
c       eq. a19
      zh28 = pdm(3,3)
      zhm28 = pdm(4,3)*pdl(6,2)
      xmd = 28.-xmm
      b28 = denss(zh28,db28,tinf,tlb,xmd,-1.,tz,ptm(6),s,t0,za,z0,tr12)
c
c      **** O DENSITY ****
c
c       eq. a18
      g16 = globe5(yrd,sec,glat,glong,stl,f107a,f107,ap,pd(1,1))
      db16 =  pdm(1,2)*exp(g16)*pd(1,1)
c       eq. a13 - a17
      d(1)=denss(alt,db16,tinf,tlb, 16.,0.,t(2),ptm(6),s,t0,za,z0,tr12)
      dd = d(1)
c  Corrected from PDM(3,1) to PDM(3,2)  12/2/85
c       eq. a19
      zh16 = pdm(3,2)
      b16 = denss(zh16,db16,tinf,tlb,16-xmm,-1.,
     $  t(2),ptm(6),s,t0,za,z0,tr12)
      dm16 = denss(alt,b16,tinf,tlb,xmm,0.,t(2),ptm(6),s,t0,za,z0,tr12)
c       eq. a12
      zhm16 = zhm28
      d(1) = dnet(d(1),dm16,zhm16,xmm,16.)
c       eq. a20b
      rl = alog(b28*pdm(2,2)*abs(pdl(17,2))/b16)
c       eq. a20a
      hc16 = pdm(6,2)*pdl(4,2)
      zc16 = pdm(5,2)*pdl(3,2)
      d(1) = d(1)*ccor(alt,rl,hc16,zc16)
c       eq. a21
      hcc16 = pdm(8,2)*pdl(14,2)
      zcc16 = pdm(7,2)*pdl(13,2)
      rc16 = pdm(4,2)*pdl(15,2)
      d(1) = d(1)*ccor(alt,rc16,hcc16,zcc16)
c
c       **** O2 DENSITY ****
c
c       eq. a18
      g32 = globe5(yrd,sec,glat,glong,stl,f107a,f107,ap,pd(1,3))
      db32 = pdm(1,4)*exp(g32)*pd(1,3)
c       eq. a13 - a17
      d(3)=denss(alt,db32,tinf,tlb, 32.,0.,t(2),ptm(6),s,t0,za,z0,tr12)
      return
      end
c
      function denss(alt,dlb,tinf,tlb,xm,alpha,tz,zlb,s2,t0,za,z0,tr12)
c       Calculate Temperature and Density Profiles for MSIS models
c     .. scalar arguments ..
      real alt, dlb, tinf, tlb, xm, alpha, tz, zlb, s2, t0, za, z0, tr12
c     .. local scalars ..
      real z, zg2, tt, ta, zg0, dta, t12, zg1, dd, cc, bb, x, x2, glb,
     * gamma, densa, gamm
c     .. scalars in common ..
      real gsurf, re, rgas, aem
      common/parmb/gsurf,re,rgas,aem
c     .. functione references ..
      real amax1, exp
c
      zeta(zz,zl) = (zz-zl)*(re+zl)/(re+zz)
      denss = 1.
      z = amax1(alt,za)
c      eq. a4a
      zg2 = zeta(z,zlb)
c      eq. a1a
      tt = tinf-(tinf-tlb)*exp(-s2*zg2)
      ta = tt
      tz = tt
      denss = tz
c
      if(alt.ge.za) go to 10
c      eq. a4b
      zg0 = zeta(z0,za)
c      eq. a2b
      dta = (tinf-ta)*s2*((re+zlb)/(re+za))**2
c      eq. a3e
      t12 = t0+tr12*(ta-t0)
c      eq. a4b
      zg1 = zeta(alt,za)
c       calculate temperature below za
c      eq. a3a
      dd = 0.666666*zg0*dta/ta**2 - 3.11111*(1./ta-1./t0)+
     $ 7.11111*(1./t12-1./t0)
c      eq. a3b
      cc = zg0*dta/(2.*ta*ta) - (1./ta-1./t0) - 2.*dd
c      eq. a3c
      bb = (1./ta-1./t0) - cc - dd
c      eq. a3d
      x = (-(zg1-zg0)/zg0)
c      eq. a1b
      x2 = x*x
      tz = 1./(1./t0+bb*x2+cc*x2*x2+dd*x2*x2*x2)
      denss = tz
   10 if(xm.eq.0.) go to 50
      if(ta.gt.0. .and. tz.gt.0.) go to 20
         tt = tlb
         ta = tlb
         tz = tlb
   20 continue
c      calculate density above za
c      eq. a17a
      glb = gsurf/(1.+zlb/re)**2
c      eq. a16a
      gamma = xm*glb/(s2*rgas*tinf)
c      eq. a13, a14a, & a15
      densa = dlb*(tlb/tt)**(1.+alpha+gamma)*exp(-s2*gamma*zg2)
      denss = densa
      if(alt.ge.za) go to 50
c      calculate density below za
c      eq. a17b
      glb = gsurf/(1.+za/re)**2
c      eq. a16b
      gamm = xm*glb*zg0/rgas
c      eq. a13, a14b, & a15
      denss = densa*(ta/tz)**(1.+alpha)*
     $ exp(gamm*((x-1)/t0+bb*(x*x2-1.)/3.+cc*(x2*x2*x-1.)/5.+
     $ dd*(x2*x2*x2*x-1.)/7.))
   50 continue
      return
      end
c
      function globe5(yrd,sec,lat,long,tloc,f107a,f107,ap,p)
c       Calculate G(L) function for MSIS-86/CIRA 1986
c       Upper Thermosphere Parameters
c     .. scalar arguments ..
      real yrd, sec, lat, long, tloc, f107a, f107
c     .. arrays arguments ..
      real ap(7), p(150)
c     .. local scalars ..
      real dgtr, dr, xl, tll, dayl, p18, p32, hr, sr, p39, c, s,
     * c2, c4, s2, p14, f1, f2, t71, t72, t81, t82, p44, p45,
     * exp1, exp2
c     .. scalars in common ..
      integer iyr
      real tinf, gb, rout, sw9, ctloc, stloc, c2tloc, s2tloc, c3tloc,
     * s3tloc, day, df, dfa, apd, apdf
c     .. arrays in common ..
      real t(15), plg(9,4), apt(4)
c     .. functione references ..
      common/ttest/tinf,gb,rout,t/csw/sw9
     &      /lpoly/plg,ctloc,stloc,c2tloc,s2tloc,c3tloc,s3tloc,
     &             iyr,day,df,dfa,apd,apdf,apt
c     .. functione references ..
      real sin, cos, exp, abs
c
      data dgtr/1.74533e-2/,dr/1.72142e-2/, xl/1000./,tll/1000./
     &  dayl/-1./,p14/-1000./,p18/-1000./,p32/-1000./
     &  hr/.2618/,sr/7.2722e-5/,p39/-1000./
c
c Eq. a24d
      g0(a)=(a-4.+(p(26)-1.)*(a-4.+(exp(-abs(p(25))*(a-4.))-1.)/
     &  abs(p(25))))
c Eq. a24c
      sumex(ex)=1.+(1.-ex**19)/(1.-ex)*ex**(.5)
c Eq. a24a
      sg0(ex)=(g0(ap(2))+(g0(ap(3))*ex+g0(ap(4))*ex*ex+g0(ap(5))*ex**3
     $ +(g0(ap(6))*ex**4+g0(ap(7))*ex**12)*(1.-ex**8)/(1.-ex)))
     $ /sumex(ex)
c
      t(10) = 0.
      t(11) = 0.
      t(12) = 0.
      t(13) = 0.
   10 continue
      iyr = yrd/1000.
      day = yrd - iyr*1000.
c eq. a22 (remainder of code)
      if (xl.eq.lat)   go to 15
c Calculate legendre polynomials
      c = sin(lat*dgtr)
      s = cos(lat*dgtr)
      c2 = c*c
      c4 = c2*c2
      s2 = s*s
      plg(2,1) = c
      plg(3,1) = 0.5*(3.*c2 -1.)
      plg(4,1) = 0.5*(5.*c*c2-3.*c)
      plg(5,1) = (35.*c4 - 30.*c2 + 3.)/8.
      plg(6,1) = (63.*c2*c2*c - 70.*c2*c + 15.*c)/8.
      plg(7,1) = (11.*c*plg(6,1) - 5.*plg(5,1))/6.
      plg(2,2) = s
      plg(3,2) = 3.*c*s
      plg(4,2) = 1.5*(5.*c2-1.)*s
      plg(5,2) = 2.5*(7.*c2*c-3.*c)*s
      plg(6,2) = 1.875*(21.*c4 - 14.*c2 +1.)*s
      plg(7,2) = (11.*c*plg(6,2)-6.*plg(5,2))/5.
      plg(3,3) = 3.*s2
      plg(4,3) = 15.*s2*c
      plg(5,3) = 7.5*(7.*c2 -1.)*s2
      plg(6,3) = 3.*c*plg(5,3)-2.*plg(4,3)
      plg(7,3) = (11.*c*plg(6,3)-7.*plg(5,3))/4.
      plg(8,3) = (13.*c*plg(7,3)-8.*plg(6,3))/5.
      plg(4,4) = 15.*s2*s
      plg(5,4) = 105.*s2*s*c
      plg(6,4) = (9.*c*plg(5,4)-7.*plg(4,4))/2.
      plg(7,4) = (11.*c*plg(6,4)-8.*plg(5,4))/3.
      xl = lat
   15 continue
      if (tll.eq.tloc)   go to 16
      stloc = sin(hr*tloc)
      ctloc = cos(hr*tloc)
      s2tloc = sin(2.*hr*tloc)
      c2tloc = cos(2.*hr*tloc)
      s3tloc = sin(3.*hr*tloc)
      c3tloc = cos(3.*hr*tloc)
      tll = tloc
   16 continue
      if(day.ne.dayl.or.p(14).ne.p14) cd14=cos(dr*(day-p(14)))
      if(day.ne.dayl.or.p(14).ne.p14) c2d14=cos(dr*2*(day-p(14)))
      if(day.ne.dayl.or.p(18).ne.p18) cd18=cos(2.*dr*(day-p(18)))
      if(day.ne.dayl.or.p(32).ne.p32) cd32=cos(dr*(day-p(32)))
      if(day.ne.dayl.or.p(39).ne.p39) cd39=cos(2.*dr*(day-p(39)))
      dayl = day
      p14 = p(14)
      p18 = p(18)
      p32 = p(32)
      p39 = p(39)
c         F10.7 effect
      df = f107 - f107a
      dfa = f107a-150.
      t(1) =  p(20)*df + p(21)*df*df + p(22)*dfa
     $ + p(30)*dfa**2
      f1 = 1. + (p(48)*dfa +p(20)*df+p(21)*df*df)
      f2 = 1. + (p(50)*dfa+p(20)*df+p(21)*df*df)
c        Time independent
      t(2) =
     1  (p(2)*plg(3,1) + p(3)*plg(5,1)+p(23)*plg(7,1))
     $ +(p(15)*plg(3,1))*dfa
     2 +p(27)*plg(2,1)
c        Symmetrical annual
      t(3) =
     1 (p(19) )*cd32
c        Symmetrical semiannual
      t(4) =
     1 (p(16)+p(17)*plg(3,1))*cd18
c        Asymmetrical annual
      t(5) =  f1*
     1  (p(10)*plg(2,1) + p(11)*plg(4,1))*cd14
c        Asymmetrical semiannual
      t(6) =    p(38)*plg(2,1)*cd39
c        Diurnal
      t71 = (p(12)*plg(3,2) + p(36)*plg(2,2))*cd14
      t72 = (p(13)*plg(3,2) + p(37)*plg(2,2))*cd14
      t(7) = f2*
     1 ((p(4)*plg(2,2) + p(5)*plg(4,2) + p(28)*plg(6,2)
     2 + t71)*ctloc
     4 + (p(7)*plg(2,2) + p(8)*plg(4,2) +p(29)*plg(6,2)
     5 + t72)*stloc)
c       Semidiurnal
      t81 = (p(24)*plg(4,3))*cd14
      t82 = (p(34)*plg(4,3))*cd14
      t(8) = f2*
     1 ((p(6)*plg(3,3) + p(42)*plg(5,3) + t81)*c2tloc
     3 +(p(9)*plg(3,3) + p(43)*plg(5,3) + t82)*s2tloc)
c       Terdiurnal
      t(14) = f2*
     1 ((p(40)*plg(4,4)+(p(94)*plg(5,4)+p(47)*plg(7,4))*cd14)*
     $ s3tloc
     2 +(p(41)*plg(4,4)+(p(95)*plg(5,4)+p(49)*plg(7,4))*cd14)*
     $ c3tloc)
c       Magnetic activity based on daily ap
      if(sw9.eq.-1. .and. p(52).ne.0.) go to 30
      apd = (ap(1)-4.)
      p44 = p(44)
      p45 = p(45)
      if(p44.lt.0) p44=1.e-5
      apdf = (apd+(p45-1.)*(apd+(exp(-p44  *apd)-1.)/p44  ))
      t(9) = apdf*(p(33)+p(46)*plg(3,1)+p(35)*plg(5,1)+
     $ (p(101)*plg(2,1)+p(102)*plg(4,1)+p(103)*plg(6,1))*cd14+
     $ (p(122)*plg(2,2)+p(123)*plg(4,2)+p(124)*plg(6,2))*
     $ cos(hr*(tloc-p(125))))
      go to 40
   30 continue
      exp1 = exp(-10800.*abs(p(52))/(1.+p(139)*(45.-abs(lat))))
      if(exp1.gt..99999) exp1=.99999
      exp2 = exp(-10800.*abs(p(54)))
      if(exp2.gt..99999) exp2=.99999
      if(p(25).lt.1.e-4) p(25)=1.e-4
      apt(1) = sg0(exp1)
      apt(3) = sg0(exp2)
      t(9) = apt(1)*(p(51)+p(97)*plg(3,1)+p(55)*plg(5,1)+
     $ (p(126)*plg(2,1)+p(127)*plg(4,1)+p(128)*plg(6,1))*cd14+
     $ (p(129)*plg(2,2)+p(130)*plg(4,2)+p(131)*plg(6,2))*
     $ cos(hr*(tloc-p(132))))
  40  continue
      if(long.le.-1000.) go to 49
c       Longitudinal
      t(11) = (1.+p(90)*plg(2,1))*(1.+p(81)*dfa)*
     $((p(65)*plg(3,2)+p(66)*plg(5,2)+p(67)*plg(7,2)
     $ +p(104)*plg(2,2)+p(105)*plg(4,2)+p(106)*plg(6,2)
     $ +(p(110)*plg(2,2)+p(111)*plg(4,2)+p(112)*plg(6,2))*cd14)*
     $     cos(dgtr*long)
     $ +(p(91)*plg(3,2)+p(92)*plg(5,2)+p(93)*plg(7,2)
     $ +p(107)*plg(2,2)+p(108)*plg(4,2)+p(109)*plg(6,2)
     $ +(p(113)*plg(2,2)+p(114)*plg(4,2)+p(115)*plg(6,2))*cd14)*
     $  sin(dgtr*long))
c       Ut and mixed Ut,longitude
      t(12) = (1.+p(96)*plg(2,1))*(1.+p(82)*dfa)*
     $(1.+p(120)*plg(2,1)*cd14)*
     $((p(69)*plg(2,1)+p(70)*plg(4,1)+p(71)*plg(6,1))*
     $     cos(sr*(sec-p(72))))
      t(12) = t(12)+
     $ (p(77)*plg(4,3)+p(78)*plg(6,3)+p(79)*plg(8,3))*
     $     cos(sr*(sec-p(80))+2.*dgtr*long)*(1.+p(138)*dfa)
c       Ut,longitude magnetic activity
      if (sw9.eq.-1. .and. p(52).ne.0.) go to 45
      t(13) = apdf*(1.+p(121)*plg(2,1))*
     $((p( 61)*plg(3,2)+p( 62)*plg(5,2)+p( 63)*plg(7,2))*
     $     cos(dgtr*(long-p( 64))))
     $ +apdf*(p(116)*plg(2,2)+p(117)*plg(4,2)+p(118)*plg(6,2))*
     $     cd14*cos(dgtr*(long-p(119)))
     $ + apdf*(p( 84)*plg(2,1)+p( 85)*plg(4,1)+p( 86)*plg(6,1))*
     $     cos(sr*(sec-p( 76)))
      goto 48
   45 continue
      t(13) = apt(1)*(1.+p(133)*plg(2,1))*
     $((p(53)*plg(3,2)+p(99)*plg(5,2)+p(68)*plg(7,2))*
     $     cos(dgtr*(long-p(98))))
     $ +apt(1)*(p(134)*plg(2,2)+p(135)*plg(4,2)+p(136)*plg(6,2))*
     $     cd14*cos(dgtr*(long-p(137)))
     $ +apt(1)*(p(56)*plg(2,1)+p(57)*plg(4,1)+p(58)*plg(6,1))*
     $     cos(sr*(sec-p(59)))
   48 continue
c  parms not used: 60,83,100,140-150
   49 tinf = 0.
      if(sw9.eq.-1.) tinf=p(31)
      do 50 i = 1,14
   50 tinf = tinf + t(i)
      globe5 = tinf
      return
      end
c
      function glob5l(p)
c  limited parameter version of globe 9/2/82
c  calculate g(l) function for MSIS-86/CIRA 1986
c   lower thermosphere parameters
c     .. arrays arguments ..
      real p(150)
c     .. local scalars ..
      real dr, dayl, p7, p9, p11
c     .. arrays scalars ..
      real t(15)
c     .. scalars in common ..
      integer iyr
      real ctloc, stloc, c2tloc, s2tloc, c3tloc, s3tloc, day, df,
     * dfa, apd, apdf, sw9
c     .. arrays in common ..
      real plg(9,4), apt(4), tt
      common/lpoly/plg,ctloc,stloc,c2tloc,s2tloc,c3tloc,s3tloc,
     $ iyr,day,df,dfa,apd,apdf,apt/csw/sw9
c     .. functione references ..
      real cos
c
      data dr/1.72142e-2/,t/15*0./
      data dayl/-1./,p7/-1000./,p9/-1000./,p11/-1000./
c
      if(day.ne.dayl.or.p7.ne.p(7)) cd7 = cos(dr*(day-p(7)))
      if(day.ne.dayl.or.p9.ne.p(9)) cd9 = cos(2.*dr*(day-p(9)))
      if(day.ne.dayl.or.p11.ne.p(11)) cd11 = cos(dr*(day-p(11)))
      dayl = day
      p7 = p(7)
      p9 = p(9)
      p11 = p(11)
c
      t(1) = p(2)*dfa
      t(2) = p(4)*plg(3,1)
      t(3) = p(6)*cd7
      t(4) = (p(8) )*cd9
      t(5) = (p(10)*plg(2,1)+p(22)*plg(4,1))*cd11
      t(6) = 0.
      t(7) = (p(14)*plg(2,2)*ctloc+p(15)*plg(2,2)*stloc)
      t(8) = (p(16)*plg(3,3)+p(18)*plg(5,3)
     $     +(p(20)*plg(6,3))*cd11)*c2tloc
     $     +(p(17)*plg(3,3)+p(19)*plg(5,3)
     $     +(p(21)*plg(6,3))*cd11)*s2tloc
      t(14) = (p(12)*plg(4,4)*c3tloc
     $     +p(25)*plg(4,4)*s3tloc)
      if(sw9.eq.1)  t(9)=apdf*(p(23)+p(24)*plg(3,1))
      if(sw9.eq.-1) t(9)=(p(3)*apt(3)+p(5)*plg(3,1)*apt(3))
c       parms not used: 13
      tt = 0.
      do 50 i=1,14
   50 tt = tt+t(i)
      glob5l = tt
      return
      end
c
      function dnet(dd,dm,zhm,xmm,xm)
c       8/20/80
c       Turbopause correction for MSIS models
c       Eq. A12b
      a=zhm/(xmm-xm)
c       Eq. A12a
      ylog=a*alog(dm/dd)
      if (ylog.lt.-10.) then
         dnet=dd
         return
      endif
      if (ylog.gt. 10.) then
         dnet=dm
         return
      endif
      dnet=dd*(1.+exp(ylog))**(1/a)
      return
      end
c
      function  ccor(alt, r,h1,zh)
c        Chemistry/dissociation correction for MSIS models
c     Eq. A20a or Eq. A21
      e=(alt-zh)/h1
      if (e.gt. 70.) then
         ccor=0.
         go to 50
      endif
      if (e.lt.-70.) then
         ccor=r
         go to 50
      endif
      ex=exp(e)
      ccor=r/(1.+ex)
   50 ccor=exp(ccor)
      return
      end
c
      Block data prmsg3
c          CIRA     11-FEB-86
      common/PARMB/gsurf,Re,Rgas,aem
      common/PARM3/pt1(50),pt2(50),pt3(50),
     * pb1(50),pb2(50),pb3(50),pc1(50),pc2(50),pc3(50),
     * pd1(50),pd2(50),pd3(50),
     * ph1(50),ph2(50),ph3(50),pi1(50)
      common/LOWER5/ptm(8),pdm(8,7)
      data gsurf, Re, Rgas, aem /980.665, 6356.776, 831.4, 1.66057e-24/
c                    Temperature
      data pt1/
     *  9.96040e-01, 3.85528e-02, 3.03445e-03,-1.05531e-01,-6.07134e-03,
     * -5.16278e-04,-1.15622e-01, 2.02240e-03, 9.90156e-03,-1.27371e-01,
     * -3.02449e-02, 1.23512e-02,-5.26277e-03,-8.45398e+00, 0.00000e+00,
     *  1.42370e-02, 0.00000e+00, 1.25818e+02, 8.05486e-03, 1.64419e-03,
     * -6.21452e-06, 3.11701e-03, 0.00000e+00, 3.86578e-03, 1.32397e-01,
     *  2.13315e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00,-6.41110e-06,
     *  0.00000e+00, 3.00150e+01, 5.33297e-03, 3.89146e-03, 2.04725e-03,
     *  0.00000e+00, 0.00000e+00,-1.92645e-02, 2.75905e+00, 1.47284e-03,
     *  3.41345e-04,-1.17388e-03,-3.54589e-04, 1.13139e-01, 1.69134e-01,
     *  5.08295e-03, 3.65016e-05, 4.26385e-03, 1.15102e-04, 5.11819e-03/
      data pt2/
     *  6.09108e-03, 4.04995e-05, 1.53049e-03, 2.41470e-05, 2.30764e-03,
     *  1.55267e-03, 1.33722e-03,-1.82318e-03,-2.63007e+02, 0.00000e+00,
     *  1.37337e-03, 9.95774e-04, 0.00000e+00,-1.08983e+02, 5.62606e-03,
     *  5.94053e-03, 1.09358e-03, 0.00000e+00,-1.33410e-02,-2.43409e-02,
     * -1.35688e-02, 3.11370e+04, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     * -2.83023e+03, 8.45583e-04, 5.38706e-04, 0.00000e+00, 2.47956e+02,
     *  2.92246e-03, 0.00000e+00, 0.00000e+00, 7.47703e-05, 8.87993e-04,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     * -1.16540e-02,-4.49173e-03,-3.53189e-04,-1.73933e-04,-1.53218e-04,
     * -5.65411e-01, 7.77272e-03,-9.11784e+01, 6.45187e-04, 0.00000e+00/
      data pt3/
     * -8.37685e-04, 2.42318e-03, 4.73796e-03,-3.01801e-03,-4.23564e-03,
     * -2.48289e-03, 9.19286e-04, 2.16372e-03, 8.63968e-04, 1.89689e-03,
     *  4.15654e-03, 0.00000e+00, 1.18068e-02, 3.31190e-03, 0.00000e+00,
     *  1.20222e-03, 0.00000e+00, 0.00000e+00,-3.07246e+00, 0.00000e+00,
     *  0.00000e+00, 6.72403e-04, 1.08930e-03, 9.72278e-04, 4.68242e+00,
     * -3.15034e-04, 4.00059e-03, 5.15036e-03, 1.62989e-03, 1.08824e-03,
     *  9.95261e-04, 4.18955e+00,-3.64059e-01, 1.70182e-03, 0.00000e+00,
     *  0.00000e+00,-3.20120e+00, 0.00000e+00, 5.80206e-03, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
c                    O density
      data pb1/
     *  9.31113e-01,-1.38721e-01,-1.33457e-01,-5.29542e-02,-4.44983e-03,
     *  1.35264e-02, 5.98075e-02,-3.62880e-02,-3.12798e-02, 3.72068e-01,
     *  2.95974e-02, 1.20509e-02, 5.21995e-02,-7.78888e+00, 0.00000e+00,
     *  1.18634e-01,-2.04495e-02, 1.03280e+02, 9.82432e-02, 4.77694e-04,
     *  0.00000e+00, 2.74372e-03, 0.00000e+00, 0.00000e+00, 7.57809e-02,
     *  1.71403e-01,-1.05205e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00,-8.73348e+00,-5.81094e-03, 0.00000e+00,-8.14944e-03,
     *  0.00000e+00, 0.00000e+00, 5.17255e-02,-1.53028e+01,-3.48932e-03,
     *  9.61771e-04, 5.57732e-03,-4.54180e-04, 9.88213e-02, 9.40456e-02,
     * -3.18797e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00, 2.32122e-03/
      data pb2/
     * -6.00220e-03, 2.77654e-05,-3.22019e-03, 0.00000e+00,-3.78551e-03,
     * -3.34809e-03,-1.70668e-03, 0.00000e+00, 6.36184e+03, 0.00000e+00,
     *  1.59986e-03,-3.88204e-03,-1.64825e-03,-7.47955e+01,-1.05360e-02,
     * -9.45723e-03,-1.59824e-03,-7.06730e-04,-1.68513e-02,-1.13023e-01,
     * -6.36637e-02,-1.37709e+04, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     * -1.52368e+04,-5.86061e-03,-2.53108e-03, 0.00000e+00,-2.54837e+03,
     * -3.28988e-03, 0.00000e+00, 0.00000e+00,-2.76364e-03, 9.67923e-03,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  4.34255e-02, 1.14020e-02,-6.18447e-03, 0.00000e+00, 0.00000e+00,
     * -3.02568e-01,-3.27694e-02,-6.71589e+01,-2.28340e-03, 0.00000e+00/
      data pb3/
     *  3.06230e-03,-4.65113e-03,-9.73421e-03, 1.28326e-02, 7.88553e-03,
     *  7.97197e-03,-1.20760e-02,-7.67547e-03,-1.20755e-03,-2.98523e-02,
     * -1.26560e-02, 0.00000e+00,-5.68350e-02,-1.53039e-02, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 2.42911e-03,-4.01347e-03,-2.19074e-03, 3.11281e+00,
     *  3.23251e-03,-6.39523e-03,-6.63069e-03,-3.04403e-04,-4.01920e-03,
     * -1.18708e-03, 4.15211e+00,-2.01896e-01, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
c                    N2 density & tlb
      data pc1/
     *  1.06903e+00, 3.77113e-04, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  8.98481e-02,-2.36325e+01, 2.08180e-02, 1.39638e+02,-1.19444e-01,
     * -8.45398e+00,-3.99776e-06, 0.00000e+00, 3.66210e-03,-1.78929e-03,
     *  1.90412e-02,-3.92257e-02, 6.32343e-03, 5.48144e-03, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-2.43022e-03,
     *  9.76619e-01, 5.68478e-04, 5.82026e-03, 0.00000e+00, 6.21998e-03,
     *  0.00000e+00, 0.00000e+00, 1.07674e-02, 8.93820e+01,-1.92414e-02,
     * -8.45398e+00, 0.00000e+00, 0.00000e+00,-2.00200e-02,-1.95833e-03,
     * -9.38391e-03, 1.31480e-02,-2.60147e-03,-8.08556e-04, 5.11651e-05,
     *  2.55717e-03, 0.00000e+00, 4.66814e-03, 6.64196e-03, 0.00000e+00/
c                    Z0 & T0
      data pc2/
     *  9.98594e-01, 1.90038e-04, 0.00000e+00,-2.43825e-02, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 5.22105e-02,
     * -8.45398e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  7.67271e-03, 5.64539e-03,-2.70623e-03,-5.26454e-04, 1.37075e-03,
     *  1.33060e-03, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  9.49197e-01, 0.00000e+00, 0.00000e+00,-7.68008e-02, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00,-1.37993e-02,-1.40136e+00, 1.20481e-01,
     * -8.45398e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  9.87746e-03, 1.75330e-03,-6.88835e-04, 2.87022e-03, 0.00000e+00,
     *  0.00000e+00, 7.44513e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00/
c                    Tr
      data pc3/
     *  1.52840e-01, 0.00000e+00, 0.00000e+00, 1.16252e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-6.49190e-01,
     * -8.45398e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     * -5.84949e-02,-1.02105e-01, 2.99153e-02,-4.86227e-02, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
c                    O2 density
      data pd1/
     *  9.31402e-01, 1.37976e-01, 0.00000e+00, 3.23736e-04, 0.00000e+00,
     * -9.10906e-03, 7.07506e-02, 0.00000e+00,-5.16650e-02, 6.89755e-02,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00,-8.45398e+00, 0.00000e+00,
     *  2.81140e-02, 0.00000e+00, 7.36009e+01, 5.96604e-02, 0.00000e+00,
     *  0.00000e+00,-1.51792e-03, 0.00000e+00, 0.00000e+00, 1.32397e-01,
     *  2.13315e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 9.48758e+00, 8.84541e-03, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 1.13139e-01, 1.69134e-01,
     *  1.45192e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pd2/
     *  1.07906e-02, 2.99942e-05, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,-1.48930e-02,
     * -7.87184e-03, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     * -6.83420e-02,-4.41778e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 2.29730e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data pd3/
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
c                    S parameter
      data ph1/
     *  9.51363e-01,-4.67542e-02, 1.20260e-01, 0.00000e+00, 0.00000e+00,
     *  1.91357e-02, 0.00000e+00, 0.00000e+00, 1.25429e-03,-1.33240e-01,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00,-8.45398e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 2.52317e-03, 0.00000e+00,-9.73404e-03, 1.32397e-01,
     *  2.13315e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00,-7.18482e-04, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 7.87683e-03,-2.33698e-03, 1.13139e-01, 1.69134e-01,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data ph2/
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
      data ph3/
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00/
c                    Turbo
      data pi1/
     *  9.33804e-01, 5.47446e+00, 1.53263e-01, 9.19303e-01, 1.64109e+01,
     *  4.27083e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *  1.15897e+00, 4.71094e-01, 1.09459e+00, 5.25012e+00, 1.00000e+00,
     *  1.00000e+00, 1.03999e+00, 7.67132e-01, 1.10514e+00, 1.75636e+00,
     *  1.10845e+00, 2.33439e+00, 7.96532e-01, 4.31520e+00, 4.07300e+00,
     *  1.01885e+00, 2.39547e-01, 2.53791e-06, 8.42931e-01, 1.04192e+00,
     *  2.00202e+00, 1.00000e+00, 1.00000e+00, 1.00000e+00, 1.00000e+00/
c                    Lower boundary
      data ptm/      1.04130e+03, 3.86000e+02, 1.90000e+02, 1.66728e+01,
     *  1.15000e+02, 1.20000e+02, 9.45537e+01, 0.00000e+00/
      data pdm/
     *               2.45600e+07, 6.71072e-06, 1.00000e+02, 0.00000e+00,
     *  1.10000e+02, 1.00000e+01, 0.00000e+00, 0.00000e+00,
     *               8.59400e+10, 5.40000e-01, 1.05000e+02,-8.00000e+00,
     *  1.10000e+02, 1.00000e+01, 9.00000e+01, 2.00000e+00,
     *               2.81000e+11, 0.00000e+00, 1.05000e+02, 2.80000e+01,
     *  2.89500e+01, 0.00000e+00, 0.00000e+00, 0.00000e+00,
     *               3.30000e+10, 2.68270e-01, 1.05000e+02, 0.00000e+00,
     *  1.10000e+02, 1.00000e+01, 0.00000e+00, 0.00000e+00,
     *               1.33000e+09, 1.19615e-02, 1.05000e+02, 0.00000e+00,
     *  1.10000e+02, 1.00000e+01, 0.00000e+00, 0.00000e+00,
     *               1.76100e+05, 1.00000e+00, 9.50000e+01,-8.00000e+00,
     *  1.10000e+02, 1.00000e+01, 9.00000e+01, 2.00000e+00,
     *               1.00000e+07, 1.00000e+00, 1.05000e+02,-8.00000e+00,
     *  1.10000e+02, 1.00000e+01, 9.00000e+01, 2.00000e+00/
      end
