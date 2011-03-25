%script for finding TRK-2-34 DSN data in 4800BBs or DSN archive files

c=2.99792458e5;
ic2=[256 1];
ic4=[ic2*256*256 ic2];

band=['UNK';'S  ';'X  ';'Ka ';'Ku ';'L  '];
rmptype=['SNAP';'STRT';'?   ';'END ';'TERM';'??  ';'??? ';'????'];
doptype=['?    ';'1-way';'2-way';'3-way';'UNK  '];

%instr=input('enter input path/filename of file ','s');

%scid=input('ID of spacecraft of interest (scid), 0 for all ');



%rampind=menu('ramped data?','no','yes')-1;
fname = 'MAP_dsnclnt_20100207.038.050000.bin'

fid=fopen(fname,'r','ieee-le.l64');
A4800=fread(fid,'uint8');
scid    = 0;
rampind = 'no';
%synchcodes (strings)

%uplink data types
ulsynch='NJPL2I00C123';

%downlink data types
dnsynch='NJPL2I00C124';

%derived data types
desynch='NJPL2I00C125';

%convert data to string to use findstr function
S4800=setstr(A4800');

%indices of starts of synch codes
indul=findstr(ulsynch,S4800);
indde=findstr(desynch,S4800);
inddn=findstr(dnsynch,S4800);

if scid>0

 %pull out data for designated spacecraft
 indulscid=find(A4800(indul+39)'==scid);
 inddescid=find(A4800(indde+39)'==scid);
 indul=indul(indulscid);
 indde=indde(inddescid);
 
end  

%find data types
indramp=find(A4800(indul+31)==9);
indramp=indul(indramp);
inddop=find(A4800(indde+31)==17);
inddop=indde(inddop);
indrng=find(A4800(indde+31)==7);
indrng=indde(indrng);
%store scid by data type for later use
scidramp=A4800(indramp+39);
sciddop=A4800(inddop+39);
scidrng=A4800(indrng+39);

%set up arrays for ramp reports
fidoramp=fopen('ramp.txt','w');
rampyear=256*A4800(indramp+32+16)+A4800(indramp+32+17);
rampday=256*A4800(indramp+32+18)+A4800(indramp+32+19);
ramptx=A4800(indramp+32+34);
uband=band(A4800(indramp+32+35)+1,:);
rtype=rmptype(A4800(indramp+32+70+32)+1,:);

%define arrays to be filled in loop
rampsec=zeros(size(rampday));
rampfreq=rampsec;
ramprate=rampsec;
ramp9=rampsec;  %ramp9 will be filled during Doppler step

for k=1:length(rampsec)

 rampsec(k)=fltiecnv(A4800(indramp(k)+32+20:indramp(k)+32+27));
 rampfreq(k)=fltiecnv(A4800(indramp(k)+32+70+16:indramp(k)+32+70+23));
 ramprate(k)=fltiecnv(A4800(indramp(k)+32+70+24:indramp(k)+32+70+31));
 %ramprate(k)=-100.0;
end 
ramptitle='YEAR  DOY  SECONDS  SCID  STA  FREQ (HZ)          RATE (HZ/SEC)  TYPE';
fprintf(fidoramp,'%s\n\n',ramptitle);
Mramp=[rampyear rampday rampsec scidramp ramptx rampfreq ramprate abs(rtype)];
fprintf(fidoramp,'%4d  %3d  %7.1f  %3d DS%02d %18.6f %18.6f %4s \n',Mramp');


fidodop=fopen('dop.txt','w');
dopyear=256*A4800(inddop+32+12)+A4800(inddop+32+13);
dopday=256*A4800(inddop+32+14)+A4800(inddop+32+15);
doptx=A4800(inddop+32+54);
rband=band(A4800(inddop+32+83)+1,:);

dtype=doptype(A4800(inddop+32+53)+1,:);
doprcvr=A4800(inddop+32+50);
doplock=A4800(inddop+32+65);
dopknum=A4800(inddop+32+104)*256^3+A4800(inddop+32+105)*256^2+A4800(inddop+32+106)*256+A4800(inddop+32+107);
dopkdenom=A4800(inddop+32+108)*256^3+A4800(inddop+32+109)*256^2+A4800(inddop+32+110)*256+A4800(inddop+32+111);

dopsec=zeros(size(dopday));
dop=dopsec;
doprate=dopsec;
doptxfreq=dopsec;
dop9=dopsec;
m=1;
ramptag=rampday(m)*86400+rampsec(m);
doptxfreq(m)=rampfreq(m);
dramp9=zeros(size(rampsec));

for k=2:length(dopsec)

 dopsec(k)=fltiecnv(A4800(inddop(k)+32+16:inddop(k)+32+23));
 dtag=dopday(k)*86400+dopsec(k);
 if m< length(rampday)
  ramptag=rampday(m+1)*86400+rampsec(m+1);
  if dtag>=ramptag
   m=m+1;
  end 
 end 
 doptxfreq(k)=rampfreq(m);
 if uband(m,1)=='S' | uband(m,1)=='L' | uband(m,1)=='U'
  txtmp=0;
 else
  txtmp=1;
 end  
 if rband(k,1)=='S' | rband(k,1)=='L' | rband(k,1)=='U'
  rtmp=0;
 else
  rtmp=1;
 end
 dopmodtmp=abs(dtype(k,1))-49;
 if dopmodtmp>2
  dopmodtmp=0;
 end 
 %setup word9
 dop9(k)=(dopmodtmp+16*rtmp+64*txtmp)*256^3;
 dramp9(m)=dop9(k);

end

dopcounthi=A4800(inddop+32+128+46)*256^3+A4800(inddop+32+128+47)*256^2;
 dopcounthi=dopcounthi+A4800(inddop+32+128+48)*256+A4800(inddop+32+128+49);
dopcountlo=A4800(inddop+32+128+50)*256^3+A4800(inddop+32+128+51)*256^2;
 dopcountlo=dopcountlo+A4800(inddop+32+128+52)*256+A4800(inddop+32+128+53);
dopcountfr=A4800(inddop+32+128+54)*256^3+A4800(inddop+32+128+55)*256^2;
 dopcountfr=dopcountfr+A4800(inddop+32+128+56)*256+A4800(inddop+32+128+57);

%following arrays are only for ascii output
hiu=floor(dopcounthi/10^6);
hil=rem(dopcounthi,10^6);
lou=floor(dopcountlo/10^6);
lol=rem(dopcountlo,10^6);
fru=floor(dopcountfr/10^6);
frl=rem(dopcountfr,10^6);

frqdiff=2^32*diff(dopcounthi)+diff(dopcountlo)+2^-32*diff(dopcountfr);
timdiff=86400*diff(dopday)+diff(dopsec);
frqdiff=frqdiff./timdiff;
frqdiff=[0;frqdiff];
timdiff=[10;timdiff];
rrate=-c*(frqdiff-dopknum.*doptxfreq./dopkdenom)./(2*dopknum.*doptxfreq./dopkdenom);
doptitle='YEAR  DOY  SECONDS SCID STA    AVE FREQ (HZ)     AVE DOPP     RR (KM/SEC)         HI           LO           FRAC    TYPE';
fprintf(fidodop,'%s\n\n',doptitle);
Mdop=[dopyear dopday dopsec sciddop doprcvr frqdiff frqdiff-240*doptxfreq/221 rrate hiu hil lou lol fru frl abs(dtype)];
fprintf(fidodop,'%4d  %3d  %7.1f %3d DS%02d  %18.6f %18.6f %12.4f %06d%06d %06d%06d %06d%06d %5s \n',Mdop');


fidorng=fopen('rng.txt','w');
rngyear=256*A4800(indrng+32+12)+A4800(indrng+32+13);
rngday=256*A4800(indrng+32+14)+A4800(indrng+32+15);
rngtx=A4800(indrng+32+54);
rnguband=band(A4800(indrng+32+55)+1,:);
if rnguband(1)=='S'
 B=2;
elseif rnguband(1)=='X'
 B=2*749/221;
end 

rngrcvr=A4800(indrng+32+50);
rng_c1=A4800(indrng+32+128+78);
rng_c2=A4800(indrng+32+128+79);
rng_chop=A4800(indrng+32+128+45);
calflg=A4800(indrng+32+128+130);
rng_amb=256^3*A4800(indrng+32+128+110)+256^2*A4800(indrng+32+128+111);
 rng_amb=rng_amb+256*A4800(indrng+32+128+112)+A4800(indrng+32+128+113);
rngsec=zeros(size(rngday));
rng_meas=rngsec;
rng_obs=rngsec;
rng_figmerit=rngsec;
rtlt=rngsec;
rng_prno=rngsec;
rng_ru=rngsec;
txfreq=rngsec;
zht=rngsec;
scdel=rngsec;
cal=rngsec;



rband=band(A4800(inddop+32+83)+1,:);
rng_cycle=rngsec;
m=1;
ramptag=rampday(m)*86400+rampsec(m);
txfreq(m)=rampfreq(m);
d2type=doptype(A4800(indrng+32+53)+1,:);
rampcalflg=zeros(size(rampsec));
rng9=rampcalflg;
rramp9=rampcalflg;

for k=2:length(rngsec)

 rngsec(k)=fltiecnv(A4800(indrng(k)+32+16:indrng(k)+32+23));
 rng_meas(k)=fltiecnv(A4800(indrng(k)+32+128+20:indrng(k)+32+128+27));
 rng_obs(k)=fltiecnv(A4800(indrng(k)+32+128+28:indrng(k)+32+128+35));
 rtlt(k)=fltiecnv(A4800(indrng(k)+32+128+58:indrng(k)+32+128+61));
 rng_prno(k)=fltiecnv(A4800(indrng(k)+32+128+62:indrng(k)+32+128+65));
 rng_figmerit(k)=fltiecnv(A4800(indrng(k)+32+128+46:indrng(k)+32+128+49));
 rng_cycle(k)=fltiecnv(A4800(indrng(k)+32+128+102:indrng(k)+32+128+109));
 zht(k)=fltiecnv(A4800(indrng(k)+32+46:indrng(k)+32+49))+fltiecnv(A4800(indrng(k)+32+76:indrng(k)+32+79));
 scdel(k)=fltiecnv(A4800(indrng(k)+32+96:indrng(k)+32+103));
 cal(k)=fltiecnv(A4800(indrng(k)+32+128+4:indrng(k)+32+128+11))+fltiecnv(A4800(indrng(k)+32+128+12:indrng(k)+32+128+19));
 if calflg(k)==1
  calfreq=rampfreq(m);
  cal_ru=B/rampfreq(m);
 else
  calfreq=1.;
  cal_ru=1.; 
 end 

 rtag=rngday(k)*86400+rngsec(k);

 txfreq(k)=rampfreq(m) + ramprate(m)*rem(rtag-ramptag,86400.0);
 rng_ru(k)=B/txfreq(k);
 if dopmodtmp>2
  dopmodtmp=0;
 end 
 if uband(m,1)=='S' | uband(m,1)=='L' | uband(m,1)=='U'
  txtmp=0;
 else
  txtmp=1;
 end  
 if rband(k,1)=='S' | rband(k,1)=='L' | rband(k,1)=='U'
  rtmp=0;
 else
  rtmp=1;
 end
 rng9(k)=(dopmodtmp+16*rtmp+64*txtmp)*256^3+rng_c1(k)*256^2+rng_c2(k)*256*8;
 rramp9(m)=rng9(k);
 rampcalflg(m)=calflg(k);	
 
 if m< length(rampday)
  ramptag=rampday(m+1)*86400+rampsec(m+1);
  if rtag>=ramptag
   m=m+1;
  end 
 end 

end

rng_obs_km=rng_obs*0.5*c.*rng_ru;
rng_amb_km=rng_amb*0.5*c.*rng_ru;
rdelta=(cal+(zht+scdel)./cal_ru)*c/2.0;

rngtitle='YEAR  DOY  SECONDS SCID STA  C1 C2 CHOP     RTLT  CALFLG  RAMB          RAMB(KM)         RNG_MEAS      RNG_OBS    DELTA       RNG_OBS_KM    FIG    PRNO';
fprintf(fidorng,'%s\n\n',rngtitle);
Mrng=[rngyear rngday rngsec scidrng rngrcvr rng_c1 rng_c2 rng_chop rtlt calflg rng_amb rng_amb_km rng_meas rng_obs rdelta rng_obs_km rng_figmerit rng_prno];
fprintf(fidorng,'%4d  %3d  %7.1f %3d DS%02d  %2d %2d %2d %10.2f  %1d  %10d %15.6f %15.3f %15.3f %9.3f %18.6f %6.1f %6.1f \n',Mrng');

%create 60-byte data
if scid>0
 menu60=menu('Create 60-byte data?','yes','no');
 if menu60==1
  fnm60=['trk34_',int2str(scid),'.b60'];
  fido60=fopen(fnm60,'w');	
  %need scid-satid and tracker ID to 60-byte index cross references
  load sqlstuf/sat_dsn_xref.txt
  load sqlstuf/sta_dsn_xref.txt
  satind=find(sat_dsn_xref(:,1)==scid);
  satid=sat_dsn_xref(satind,2);
  tx_del_km=sat_dsn_xref(satind,3);
  rtlt_glob=input('enter rtlt (integer seconds) ');
  
  %create headers from ramp data, advance time tag by rtlt 
  for k=1:length(rampsec)
   head1=[sta_dsn_xref(find(sta_dsn_xref(:,1)==ramptx(k)),2) sta_dsn_xref(find(sta_dsn_xref(:,1)==doprcvr(1)),2) rampyear(k) rampday(k)];
   fwrite(fido60,head1,'uint16');
   head2=[rampsec(k)+rtlt_glob-0.02 ramprate(k)*1.e-6 rampfreq(k)*1.e-6 0];
   fwrite(fido60,head2,'float64');
   word9=rramp9(k)+2*256^2*16+1024+256*(1-rampcalflg(k));
   %word9=word9+5*256^2+19*2048;
   head3=[word9 0 satid];
   fwrite(fido60,head3,'uint32');
   head4=[-111 0 0 rtlt_glob];
   fwrite(fido60,head4,'int16'); 
   %word9=word9-(5*256^2+19*2048);
   %and again for Doppler headers
   word9=dramp9(k)+4*256^2*16+2*256;
   fwrite(fido60,head1,'uint16');
   fwrite(fido60,head2,'float64');
   head3=[word9 0 satid];
   fwrite(fido60,head3,'uint32');
   fwrite(fido60,head4,'int16');
  end
  
  %create 60-byte records for Doppler data
  for k=1:length(dopsec)
   head1=[sta_dsn_xref(find(sta_dsn_xref(:,1)==ramptx(1)),2) sta_dsn_xref(find(sta_dsn_xref(:,1)==doprcvr(k)),2) dopyear(k) dopday(k)];
   fwrite(fido60,head1,'uint16');
   head2=[dopsec(k)  timdiff(k) 0  rrate(k)];
   fwrite(fido60,head2,'float64');
   %word9=0;
   %if uband(k,1)=='X'
   % word9=word9+4*256*3;
   %end
   %if rband(1,1)=='X'
   % word9=word9+1*256*3;
   %end
   if doplock(k)==4
    dvaltmp=1;
   else
    dvaltmp=0;
   end  
   word9=dop9(k)+dvaltmp*256^2*16*4+512;
   head3=[word9 0 satid];
   fwrite(fido60,head3,'uint32');
   head4=[12 0 0 0];
   fwrite(fido60,head4,'int16'); 
  end
  
  %create 60-byte records for range data
  for k=1:length(rngsec)
  
   if rampind>0
    head1=[sta_dsn_xref(find(sta_dsn_xref(:,1)==ramptx(1)),2) sta_dsn_xref(find(sta_dsn_xref(:,1)==rngrcvr(k)),2) rngyear(k) rngday(k)];
    fwrite(fido60,head1,'uint16');
    head2=[rngsec(k)-.02  0 txfreq(k) 0];
    fwrite(fido60,head2,'float64');
    if rng_obs(k)==-1.0
     word9=rng9(k);
    else
     word9=rng9(k)+256^2*2*16+1024+256*(1-calflg(k));
    end 
    head3=[word9 0 satid];
    fwrite(fido60,head3,'uint32');
    head4=[2^16-111 0 0 0];
    fwrite(fido60,head4,'int16'); 

   end
   
  
   head1=[sta_dsn_xref(find(sta_dsn_xref(:,1)==ramptx(1)),2) sta_dsn_xref(find(sta_dsn_xref(:,1)==rngrcvr(k)),2) rngyear(k) rngday(k)];
   fwrite(fido60,head1,'uint16');
   head2=[rngsec(k)  0 rng_obs_km(k)-tx_del_km 0];
   fwrite(fido60,head2,'float64');
   if rng_obs(k)==-1.0
    word9=rng9(k);
   else
    word9=rng9(k)+256^2*2*16+1024+256*(1-calflg(k));
   end 
   head3=[word9 0 satid];
   fwrite(fido60,head3,'uint32');
   head4=[12 0 0 0];
   fwrite(fido60,head4,'int16'); 
  end
  
  %read 60-byte data back in and sort into time order and write out again 
  fclose(fido60);
  fido60=fopen(fnm60,'r');
   A60=fread(fido60,60,'uint8');
   Am=[];
   ttag=[];
   while length(A60)==60
    Am=[Am;A60'];
    ttag=[ttag;86400*(A60(7)*256+A60(8))+fltiecnv(A60(9:16))];
    A60=fread(fido60,60,'uint8');
   end
   fclose(fido60);
   [tmp,itag]=sort(ttag);
   Am=Am(itag,:);
   Am=Am';
   fido60=fopen(fnm60,'w');
   fwrite(fido60,Am,'uint8');
   
    
    
  
 end
end
