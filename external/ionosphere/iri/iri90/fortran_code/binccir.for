C
C The program BINCCIR.FOR generates the CCIR coefficient files
C in ASCII format from the binary files, either for all 12 months
C or for individually selected months.
C
C D. Bilitza, 12/23/94
C
	character	filename*12
	dimension f2(13,76,2),fm3(9,49,2)

	write(6,1000)
1000	format(1x,'which month (0 for all month)?')
	read(5,*) month
	months=month
	monthe=month
	if (month.eq.0) then
		months=1
		monthe=12
		endif
	do 1234 mmo=months,monthe,1
	mo=mmo+10
	write(6,1001) mo,mo
1001	format(1x,'Generating CCIR',I2,'.ASC from CCIR',i2,'.BIN')	
	write(filename,104) mo
104	format('ccir',i2,'.bin')
	open(10,file=filename,status='old',form='unformatted')
	read(10) f2,fm3
	write(filename,1104) mo
1104	format('ccir',i2,'.asc')
	open(11,file=filename,status='new',form='formatted')
	write(11,111) f2,fm3
111	format(1X,4E15.8)
	close(10)
	close(11)
1234	continue
	stop
	end
