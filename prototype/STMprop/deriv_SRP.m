function [xdot,Amat] = deriv_SRP(t,x,ForceModel)

global SolarSystem jd_ref

%  Determine if the STM is being propagated
if size(x,1) == 6
    PropSTM = 0;
elseif size(x,1) == 42
    PropSTM = 1;
end

%  Useful terms for later
rv     = x(1:3,1);
rSat   = norm(rv);
rvSat  = rv;
eye3   = eye(3);
zero3  = zeros(3);
[DeltaAT] = MJD2TimeCoeff(jd_ref);
DeltaTT   = DeltaAT + 32.184;
jd_tt     = jd_ref + (DeltaTT + t)/86400;;
rvsun     = pleph(jd_tt,11,ForceModel.CentralBodyIndex,1);
sv        = rvsun - rv;
s         = norm(sv);
svhat     = sv/s;
s3        = s^3;
s5        = s^5;
[percentSun,dEpsdrvSat] = GetPercentSun(t,rvSat,rvsun);
% if percentSun < 1 && percentSun ~= 0
%     dEpsdrvSat = numjac(@GetPercentSun,t,rvSat,percentSun,1e-12*ones(1,1),[],0,[],[],rvsun);
% else
%     dEpsdrvSat = [0 0 0];
% end

%  Calculate the acceleration
pressureAtEarth = ForceModel.SolarRadiationPressure.Flux/299792458;
P  = pressureAtEarth*ForceModel.SolarRadiationPressure.Nominal_Sun^2*1/norm(sv)^2;
AU = ForceModel.SolarRadiationPressure.Nominal_Sun;
Cr = 1.8;
As = 1;
m  = 10;
xdot(1:3,1) = [0 0 0]';
xdot(4:6,1) = -percentSun*pressureAtEarth*AU^2*Cr*As/m*svhat/norm(sv)^2;

%  Calculate the STM Terms
Amat = zeros(6,6);
if PropSTM
    d1    = -pressureAtEarth*AU^2*Cr*As/m*svhat/norm(sv)^2*dEpsdrvSat;
    d2    = percentSun*pressureAtEarth*AU^2*Cr*As/m*(1/s3*eye3 - 3*sv*sv'/s5);
    Amat(4:6,1:3)  = d1 + d2;
end

function [percentSun,Jac] = GetPercentSun(t,rvSat,rvsun)

%% Compute percent sunlight
rSat             = norm(rvSat);
Force.radiusSun  = 695990;
Force.bodyRadius = 6378.1363;
a = asin(Force.radiusSun/norm(rvsun));
b = asin(Force.bodyRadius/rSat);
cosArg = -rvSat'*(rvsun)/rSat/norm(rvsun);
c = acos(cosArg);
Jac = [0 0 0];
if c > a + b
    percentSun = 1;
elseif c < abs(b - a)
    percentSun = 0;
else
    c1 = 0.5*(c*c + a*a - b*b)/c;
    c2 = sqrt(a^2 - c1^2);
    A  = a^2*acos(c1/a) + b^2*acos((c - c1)/b)-c*c2;
    percentSun = 1 - A/pi/a^2;
    
    dbdrvSat = 1/sqrt(1 - (Force.bodyRadius/rSat)^2)*Force.bodyRadius/rSat^3*rvSat';
    dcdrvSat = -1/sqrt(1 - (cosArg)^2)*...
               rvsun'/norm(rvsun)*(eye(3)/rSat - rvSat*rvSat'/rSat^3);         
    
    dAdb =  2*b*acos((c-c1)/b)   ...
        + a*b/sqrt(1 - c1^2/a^2)/c ...
        -b*c1/((a^2 - c1^2)^(1/2))...
        -(a^2 + b^2 - c^2)/sqrt(1 - ((c - c1)/b)^2)/(2*c);
  
     dAdc = c*(-a^2 +b^2 + c^2)/sqrt(a^2 - c1^2)*c1/2/c^2 ...
        -c2 ...
        -a/sqrt(1 - c1^2/a^2)*(1 - c1/c) ...
        - b^2/sqrt(1 - ((c - c1)/b)^2)*c1/b/c;
    
    Jac = (dAdb*dbdrvSat + dAdc*dcdrvSat )/pi/a^2;
    
end
