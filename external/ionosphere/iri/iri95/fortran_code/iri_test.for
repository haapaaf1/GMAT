      DIMENSION         outf(11,100),oar(38,100)
      LOGICAL		jf(20)
      CHARACTER         ARGV*80
      CHARACTER*4       IMZ(8),MAP,xtex
      CHARACTER*5       ITEXT(8)
      CHARACTER*8       bopt
      CHARACTER*10      iopt
      CHARACTER*27      pname
      CHARACTER*25      hname

      DATA  IMZ  /' km ','GEOD','GEOD','yyyy',' mm ',' dd ','YEAR',
     &      'L.T.'/, ITEXT/'  H  ',' LATI',
     &      ' LONG',' YEAR','MONTH',' DAY ','DAYOF',' HOUR'/

c user input of IRI input parameters
c
1	type *,'jmag(=0/1,geog/geom),lati/deg,long/deg'
	read(5,*) jm,xlat,xlon
	type *,'year(yyyy),mmdd(or -ddd),iut(=0/1,LT/UT),hour'
	read(5,*) iy,imd,iut,hour
	type *,'height/km'
	type *,'(enter 0 for list of peak heights and densities)'
        type *,'(enter -1 for plasma frequencies and profile',
     &		' parameters)'
	read(5,*) hx
	type *,'upper height [km] for TEC integration (0 for no TEC)'
	read(5,*) htec_max

	type *,'variable? (1/2/../8 for height/lat/long/year/month/',
     &		'day/day of year/hour)'
	read(5,*) ivar
	type *,'begin, end, and stepsize for the selected variable'
	read(5,*) vbeg,vend,vstp

	type *,'Options: t(rue) or f(alse)'
	type *,'Standard: t,t,t,t,f,f,t,t,t,t,t,t,t,t,t,t,t'
	type *,'Enter 0 to use standard or 1 to enter your own'
	read(5,*) jchoice
      if(jchoice.eq.0) then
	do i=1,20 
		jf(i)=.true.
		enddo
	jf(5)=.false.
	jf(6)=.false.
      else
	type *,'Compute Ne, T, Ni? (enter: t,t,t  if you want all)'
	read(5,*) jf(1),jf(2),jf(3)
	type *,'Bottomside thickness B0: t=Table-option, f=Gulyaeva {t}.'
	read(5,*) jf(4)
	type *,'foF2 model: t=CCIR, f=URSI-88 {standard:f}'
	read(5,*) jf(5)
	type *,'Ion comp. model: t=standard, f=Danilov-95 {f}' 
	read(5,*) jf(6)
	type *,'Ne Topside mod.: t=standard, f=IRI-79 topside {t}'
	read(5,*) jf(7)
	type *,'F2 peak density or foF2: t=model, f=user input.'
	read(5,*) jf(8)
	type *,'F2 peak height or M3000F2: t=model, f=user input.'
	read(5,*) jf(9)
	type *,'Te(Ne) model: t=not used, f=correlation is used. {t}'
	read(5,*) jf(10)
	type *,'LAY version: t=standard ver., f=LAY version. {t}'
	read(5,*) jf(11)
	type *,'Message output unit: t=(UNIT=6), f=(UNIT=12).'
	read(5,*) jf(12)
	type *,'F1 peak density or foF1: t=model, f=user input.'
	read(5,*) jf(13)
	type *,'F1 peak height: t=model, f=user input.'
	read(5,*) jf(14)
	type *,'E peak density or foE: t=model, f=user input.'
	read(5,*) jf(15)
	type *,'E peak height: t=model, f=user input.'
	read(5,*) jf(16)
	type *,'Sunspot index: t=from file, f=user input.'
	read(5,*) jf(17)
	type *,'UT/LT computation: t=no date change, f=ut_lt subroutine.'
	read(5,*) jf(18)
      endif

c option to enter measured values for NmF2, hmF2, NmF1, hmF1, NmE, hmE,
c N(300), N(400), N(600) if available; only for altitude profiles (ivar=1)
c
	if(ivar.eq.1) then
		if(.not.jf(8)) then
			type*,'foF2/Mhz or NmF2/m-3'
			read(5,*) oar(1,1)
	                pname='plasma frequency  (foF2/MHz'
	        	if(oar(1,1).gt.30.) 
     &				pname='electron density (NmF2/cm-3'
			endif
		if(.not.jf(9)) then
			type*,'hmF2/km or M3000F2'
			read(5,*) oar(2,1)
	                hname='Propag. factor (M(3000)F2'
        	        if(oar(2,1).gt.50.) 
     &				hname='F2 peak height   (hmF2/km'
			endif
		if(.not.jf(13)) then
			type*,'foF1/MHz or NmF1/m-3'
			read(5,*) oar(3,1)
			endif
		if(.not.jf(14)) then
			type*,'hmF1/km'
			read(5,*) oar(4,1)
			endif
		if(.not.jf(15)) then
			type*,'foE/MHz or NmE/m-3'
			read(5,*) oar(5,1)
			endif
		if(.not.jf(16)) then
			type*,'hmE/km'
			read(5,*) oar(6,1)
			endif
		if(.not.jf(10)) then
			type*,'Ne(300km),Ne(400km),Ne(600km)/m-3',
     &				' [-1 if not]'
			read(5,*) oar(15,1),oar(16,1),oar(17,1)
			endif
		endif

c input of user-specified Rz12 if so desired
c
	if(.not.jf(17)) then
		type*,'Rz12'
		read(5,*) oar(33,1)
		do i=2,100
			oar(33,i)=oar(33,1)
			enddo
		endif

      num1=(vend-vbeg)/vstp+1
      numstp=iabs(num1)
      if(numstp.GT.100) numstp=100

	map='URSI'
        if(jf(5)) map='CCIR'
        bopt='Gulyaeva'
	if(jf(4)) bopt='B0-Table'
        iopt='Danilov-95'
        if(jf(6)) iopt='IRI-86    '

        hxx=hx
	jmag=jm
	mmdd=imd
         call iri_web(jmag,jf,xlat,xlon,iy,mmdd,iut,hour,
     &          hxx,htec_max,ivar,vbeg,vend,vstp,outf,oar)

c
c input information for result page
c
        write(7,301) map
        write(7,303)
        write(7,309) bopt
        write(7,329) iopt
        if(.not.jf(8)) write(7,302) pname,oar(1,1)/1.e6
        if(.not.jf(9)) write(7,304) hname,oar(2,1)

        if(ivar.eq.1) then
                write(7,213) oar(1,1)/1.E6,oar(3,1)/1.E6,oar(5,1)/1.E6
                write(7,214) oar(2,1),oar(4,1),oar(6,1)
        else
                write(7,307)
        endif

        write(7,211) oar(23,1),oar(25,1),oar(27,1)

        if(.not.jf(17)) then
                write(7,223) oar(33,1),oar(34,1)
        else
                write(7,212) oar(33,1),oar(34,1)
        endif
        if(htec_max.gt.50.0) write(7,3914) htec_max

3914    format(/'TEC [1.E16 m-2] is obtained by numerical integration',
     &     ' in 1km steps'/'  from 50 to ',f6.1,' km.  t is the',
     &     ' percentage of TEC above the F peak.')
301     format(A4,' maps are used for the F2 peak density (NmF2)')
302     format('F2 peak ',A27,'=',F10.1,') provided by user')
303     format('CCIR maps are used for the F2 peak height (hmF2)')
304     format(A25,'=',F5.1,') provided by user')
307     format(1x/'Solar and magnetic parameter for the 1st profile',
     &          ' point:')
309     format(A8,' option is used for the bottomside thickness ',
     &          'parameter B0')
329     format(A10,' option is used for the ion composition')
211     format('Solar Zenith Angle/degree',28X,F6.1/
     &          'Dip (Magnetic Inclination)/degree',20X,
     &          F6.2/'Modip (Modified Dip)/degree',26X,F6.2)
212     format('Solar Sunspot Number (12-months running mean) Rz12',
     &          4X,F5.1/'Ionospheric-Effective Solar Index IG12',
     &          16X,F5.1)
223     format('Solar Sunspot Number (12-months running mean) Rz12',
     &          4X,F5.1,'{user provided input}'/'Ionospheric-',
     &          'Effective Solar Index IG12',16X,F5.1)
213     format('Peak Densities/cm-3: NmF2=',F9.1,'   NmF1=',F9.1,
     &          '   NmE=',F9.1)
214     format('Peak Heights/km:     hmF2=',F9.2,'   hmF1=',F9.2,
     &          '   hmE=',F9.2/)
c
c table head .......................................................
c
        agnr=7          !output unit number
        xtex=imz(ivar)
        if(jmag.gt.0.and.(ivar.eq.2.or.ivar.eq.3)) xtex='GEOM'
        if(iut.gt.0.and.ivar.eq.8) xtex='U.T.'
      PIKTAB=0
        IF(IVAR.NE.1) THEN
                IF(HX.LT.1.0) PIKTAB=1
                IF(HX.LT.0.0) PIKTAB=2
                ENDIF
        IF(PIKTAB.GT.1) THEN
          WRITE(7,8194) ITEXT(IVAR),xtex
        ELSE IF(PIKTAB.GT.0) THEN
          WRITE(7,8192) ITEXT(IVAR),xtex
        ELSE
          WRITE(7,8193) ITEXT(IVAR),xtex
        ENDIF
8192  FORMAT(/'-'/2X,A5,6X,'PEAK ALTITUDES IN KM',8X,'PEAK DEN',
     &  'SITIES IN cm-3  1E16m-2'/3X,A4,'    hmF2  hmF1   hmE   ',
     &  'hmD      NmF2   NmF1    NmE    NmD  TEC top/%')
8194  FORMAT(/'-'/2X,A5,18X,'E-VALLEY',6X,'PLASMA FREQUENCIES',
     & ' IN MHz 1E16m-2'/3X,A4,' M3000F2 B0/km  W/km Nmin/NmE ',
     &  '  foF2   foF1    foE    foD   TEC top/%')
8193  FORMAT(/'-'/1X,A5,' ELECTRON DENSITY   TEMPERATURES ',
     &  8X,'ION PERCENTAGES/%',5x,'1E16m-2'/2X,A4,' Ne/cm-3 Ne/NmF2',
     &  ' Tn/K  Ti/K  Te/K  O+  N+  H+ He+ O2+ NO+ Clust TEC t/%')

        xcor=vbeg

        do 1234 li=1,numstp

c
c special output: peak densities and altitudes
c
      IF(PIKTAB.eq.1) THEN
        if(oar(3,li).lt.1.) oar(4,li)=0.
        iyp1=int(oar(1,li)/1.e6+.5)
        iyp2=int(oar(3,li)/1.e6+.5)
        iyp3=int(oar(5,li)/1.e6+.5)
        iyp4=int(oar(7,li)/1.e6+.5)
        tec=oar(37,li)/1.e16
        if(tec.le.0.0) tec=0.0
        itopp=int(oar(38,li)+.5)
        WRITE(7,3910) XCOR,oar(2,li),oar(4,li),oar(6,li),oar(8,li),
     &    iyp1,iyp2,iyp3,iyp4,tec,itopp
3910    FORMAT(F7.1,2X,4F6.1,3X,4I7,1x,f6.2,i4)
        GOTO 1234
      ENDIF
      IF(PIKTAB.eq.2) THEN
        if(oar(3,li).lt.1.) oar(4,li)=0.
        yp1=sqrt(oar(1,li)/1.24e10)
        yp2=sqrt(oar(3,li)/1.24e10)
        yp3=sqrt(oar(5,li)/1.24e10)
        yp4=sqrt(oar(7,li)/1.24e10)
        tec=oar(37,li)/1.e16
        if(tec.le.0.0) tec=0.0
        itopp=int(oar(38,li)+.5)
        WRITE(7,3919) XCOR,oar(36,li),oar(10,li),oar(12,li)-oar(6,li),
     &    oar(11,li)/oar(5,li),yp1,yp2,yp3,yp4,tec,itopp
3919    FORMAT(F7.1,2X,f6.4,1x,f5.1,1x,f5.1,2x,f6.4,1X,2F7.2,2F7.3,
     &    1x,f6.2,i4)
        GOTO 1234
      ENDIF
c
c normal output
c
        if(ivar.eq.1) then
                oar(1,li)=oar(1,1)
                oar(37,li)=oar(37,1)
                oar(38,li)=oar(38,1)
                endif
        jne=int(outf(1,li)/1.e6+.5)
        xner=outf(1,li)/oar(1,li)
        jtn=int(outf(2,li)+.5)
        jti=int(outf(3,li)+.5)
        jte=int(outf(4,li)+.5)
        jio=INT(OUTF(5,li)+.5)
        jih=INT(OUTF(6,li)+.5)
        jihe=INT(OUTF(7,li)+.5)
        jino=INT(OUTF(8,li)+.5)
        jio2=INT(OUTF(9,li)+.5)
        jicl=INT(OUTF(10,li)+.5)
        jin=INT(OUTF(11,li)+.5)
        if(outf(1,li).lt.0) jne=-1
        if(outf(1,li).lt.0) xner=-1.
        if(outf(2,li).lt.0) jtn=-1
        if(outf(3,li).lt.0) jti=-1
        if(outf(4,li).lt.0) jte=-1
        if(outf(5,li).lt.0) jio=-1
        if(outf(6,li).lt.0) jih=-1
        if(outf(7,li).lt.0) jihe=-1
        if(outf(8,li).lt.0) jino=-1
        if(outf(9,li).lt.0) jio2=-1
        if(outf(10,li).lt.0) jicl=-1
        if(outf(11,li).lt.0) jin=-1
        tec=oar(37,li)/1.e16
        if(tec.le.0.0) tec=0.0
        itopp=int(oar(38,li)+.5)
      WRITE(7,7117) XCOR,jne,xner,jtn,jti,jte,jio,jin,jih,jihe,jino,
     &  jio2,jicl,tec,itopp
7117  FORMAT(F6.1,I8,1x,F6.3,3I6,7I4,f6.1,i4)

1234    xcor=xcor+vstp

	type *,'Enter 0 to exit or 1 to generate another profile?' 
	read(5,*) icontinue
	if (icontinue.gt.0) goto 1

	stop
	end
