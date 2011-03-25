function R=fltiecnv(ivec)
%function R=fltiecnv(ivec)
% bvec is vector of unsigned integers of length 4 or 8
% R is equivalent IEEE floating point value
if ivec==zeros(size(ivec))
 R=0.0;
else
 if ivec(1) < 128
  sign=1;
  ivec1=ivec(1);
 else
  sign=-1;
  ivec1=ivec(1)-128;
 end 

 if length(ivec)==4
  bitt=floor(ivec(2)/128);
  expon=(ivec1*2+bitt)-127;
  frc=((128*(1-bitt)+ivec(2))*256^2+ivec(3)*256+ivec(4))/2^23;
 else
  nib=floor(ivec(2)/16);
  expon=(ivec1*16+nib)-1023;
  frc=((16+ivec(2)-nib*16)*256.0^6+256.0.^[5:-1:0]*ivec(3:8))/2^52;
 end
 R=sign*frc*2^expon;
end