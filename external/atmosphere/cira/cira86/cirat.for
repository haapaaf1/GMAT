	dimension	sh(71),p(71),h(71),z(3,71,33)
	dimension	iex(25),ih(25),tw(2,25,17),pp(21,17)
	dimension	lat(10),x(10)
	logical		press,store
	character*2	par(2)
	character*4	heipre(2)
	character*8 	filename
	character*11	paras
	character*24	param(3),parah,parap
	data	par/'CP','CH'/
	data	paras/'ZONAL MEAN '/
	data	param/'TEMPERATURE (K)         ',
     &		      'ZONAL WIND (m/s)        ',
     &		      'GEOPOTENTIAL HEIGHT (km)'/,
     &		parah/'PRESSURE (mb)           '/,
     &		parap/'GEOPOTENTIAL HEIGHT (km)'/
     &          heipre/'p/mb','h/km'/
	lu=15
	lo=6
	li=5
	loo=lo
	la=13
	index=0
	store=.false.
	montho=0
	jpho=0
	gmin=-80.
	gmax=80.
	write(lo,100)
100	format(1x///7X,'COSPAR INTERNATIONAL REFERENCE ATMOSPHERE'
     &		,' - 1986'/1X,60('-')/20X,'(0 - 120 km)'//////)

74	write(lo,*)'pressure or height coordinates (1/2)?  ->'
	read(li,*) jph
	index=0
	if(jph.gt.1) then
		param(3)=parah
		press=.false.
		imax=25
		kmax=5
		nmax=10
		linc=10
		pmin=0
		pmax=50
	else
		param(3)=parap
		press=.true.
		imax=71
		kmax=4
		nmax=8
		linc=5
		pmin=1
		pmax=10
	endif
	
88	if(press) then
	write(lo,*)'temp., zonal wind, or geopot. height (1/2/3)?  ->'
	else
	write(lo,*)'temperature, zonal wind, or pressure (1/2/3)?  ->'
	endif
	read(li,*) jtzg
	if((jtzg.eq.3).and.(.not.press)) imax=21
	if(index.gt.0) goto 54

73	write(lo,*)'month (1 to 12) ?   ->'
	read(li,*) month
	if((jph.eq.jpho).and.(month.eq.montho)) goto 278

	write(filename,103) par(jph),month+10
103	format(A2,I2,'.DAT')
2333	open(lu,file=filename,status='old',err=2322,
     &		form='unformatted')
	goto 232
2322	if(store) close(la)
	write(lo,*)'Cannot find file ',filename,', try other',
     & 		' diskette.'
	if(store) write(lo,2101)
2101	format(1x/10x,'Please remember that part of your OUTPUT.CIR file'/
     &    10x,'will remain on the presently loaded diskette.'/10x,
     &    'Make sure that it will be deleted, before you'/10x,
     &    'start the program the next time !')
	write(lo,*)'exit ------------- 0'
	write(lo,*)'continue --------- 1'
	write(lo,*)'->'
	read(li,*) newdis
	if(newdis.ne.1) goto 775
	goto 2333

232	write(lo,*)'reading table for month:',month
	write(lo,*)' '
	if(press) then	
		read(lu) sh,p,h,z
	else
		read(lu) ih,tw,pp,iex
	endif
	close(lu)
	jpho=jph
	montho=month
278	if(index.gt.0) goto 54

22	if(press) then
		write(lo,*)'pressure min and max (1.e-5 to 1100 mb) ?'
     &			,'    #',pmin,',',pmax,'#'
	else
		write(lo,*)'height min and max (0 to 120 km) ? '
     &			,'    #',pmin,',',pmax,'#'
	endif
	write(lo,*)'->'
	read(li,*) pmin,pmax
	if(index.gt.0) goto 54

72	write(lo,1800) gmin,gmax,nmax,linc
1800  	format(1x/' latitude min and max (-80 to 80) ?',15x,'#',
     &	  f4.0,',',f4.0,'#'/10x,'Only',i3,' latitude columns can ',
     &	  'be displayed;'/10x,'the latitudinal increment is chosen',
     &	  ' accordingly.'/10x,'The smallest increment is',i3,' degrees.')
	write(lo,*)'->'
	read(li,*) gmin,gmax
	n=int(abs(gmax-gmin)/linc)+1
	lstep=1
	if(n.le.nmax) goto 6778
6789		lstep=lstep+1
		nn=int(1.*n/lstep+.5)
		if(nn.gt.nmax) goto 6789
		n=nn
6778	lbeg=int((gmin+80.)/linc)+1
	if(n.gt.nmax) n=nmax
	lat(1)=-80+(lbeg-1)*linc
	lastep=lstep*linc
	do 1199 ni=2,n
1199	lat(ni)=lat(ni-1)+lastep

54	write(loo,8801) paras,param(jtzg),month
8801	format(1x//1X,A11,A24,5X,'Month:',I2)
	write(loo,1101)
	if(press) then
		write(loo,701) 
		write(loo,702) (lat(j),j=1,n)
	else
		write(loo,711) 
		write(loo,712) (lat(j),j=1,n)
	endif
701	format(1x,'Scale  Pressure  Geom.',10X,'Latitude')
702	format(1x,'Height   (mb)   Height',8(2x,i3,1x))
711	format(1x,'Height',24X,'Latitude')
712	format(1x,' (km)',4x,10(2x,i3,1x))

	i=0
55	i=i+1
	if(i.gt.imax) goto 66
	if(press) then
	  pki=p(i)
	  if(pki.gt.pmax) goto 66
	  if(pki.lt.pmin) goto 55
	else
	  pki=ih(i)
	  if(pki.gt.pmax) goto 55
	  if(pki.lt.pmin) goto 66
	endif
		jj=lbeg
		jjj=0
2288		jjj=jjj+1
		if(press) then
		  x(jjj)=z(jtzg,i,jj)
		else if(jtzg.gt.2) then
		  x(jjj)=pp(i,jj)
		else
		  x(jjj)=tw(jtzg,i,jj)
		endif
		jj=jj+lstep
		if(jjj.lt.n) goto 2288	
		if(press) goto 2001

		if(jtzg.lt.3) then
		  write(loo,302) ih(i),(x(j),j=1,n)
		else
		  write(loo,303) ih(i),iex(i),(x(j),j=1,n)
		endif
		goto 55

2001		if(jtzg.lt.3) then
		  write(loo,300) sh(i),pki,h(i),(x(j),j=1,n)
		else
		  write(loo,301) sh(i),pki,h(i),(x(j),j=1,n)
		endif

300	format(1x,f5.2,1x,1pe9.3,0pf6.1,1x,8f6.1)
301	format(1x,f5.2,1x,1pe9.3,0pf6.1,1x,8f6.2)
302	format(1x,i5,5x,10f6.1)
303	format(1x,i4,2x,'E',i2,1x,10f6.3)

	goto 55
66	if((jph.eq.2).and.(jtzg.eq.3)) write(loo,*)'- the second ',
     &	  'column shows the exponent for one row of pressure values-'
	write(loo,1101)
1101	format(1x,70('-'))

	index=index+1
1234	if(ich1.eq.2) then
	  write(lo,*)'exit ---- 0         new parameter ---- 1'
	else
	  write(lo,*)'exit ---- 0       new parameter ---- 1 '
     &		,'      store in file OUTPUT.CIR ---- 2'
	endif
	write(lo,*)'->'
	read(li,*) ich1
	loo=lo	
	goto (75,999,97) ich1+1

97	open(la,file='OUTPUT.CIR',status='new',err=2222,
     &		form='formatted')
	loo=la
	store=.true.
	goto 54

999	write(lo,*)'new height/pressure range   enter  1    #',
     &		pmin,pmax,'#'
	write(lo,*)'new latitude range          enter  2    #',
     &		gmin,gmax,'#'
	write(lo,*)'new month                   enter  3    #',
     &		month,'#'
	write(lo,*)'new parameter               enter  4    #',
     &		param(jtzg),'#'
	write(lo,*)'height or pressure          enter  5    #',
     &		heipre(jph),'#'
	write(lo,*)'->'
	read(li,*) ich
	goto (75,22,72,73,88,74) ich+1

2222	write(lo,*)'ERROR: Cannot open file OUTPUT.CIR, or running out'
     &  	,' of disk space !'
	write(lo,*)'    File cannot be stored on this device.'
	write(lo,*)'    Exit, delete old OUTPUT.CIR files and run'
     &		,' program again, or display only.'
	goto 1234

75	if(store) close(la)
775	stop
	end

