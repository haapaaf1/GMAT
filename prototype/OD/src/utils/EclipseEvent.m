function [eventValue,eventRate] = EclipseEvent(t,x,flag);

rv_sat = x(1:3,1);
r_sat  = norm(rv_sat);
r_sat = r_sat;

jd0 = 21545+2430000;
jd  = jd0 + t/86400;
rv_sun = sun(jd)'*149597870.691;
r_sun = norm(rv_sun);
Rs = 695202;
Re = 6378.1363;

alpha_p = asin( (Rs-Re)/r_sun );
r_v     = Re/alpha_p;
r_t     = r_v*cos(alpha_p);
r3      = sqrt(r_sat^2 - Re^2);
A       = (r_t + r3);
B       = (r_v + r_sat);
C       = sqrt(A^2 + B^2 - 2*A*B*cos(alpha_p));
sinbeta = sin(alpha_p)*A/C;
beta    = asin(sinbeta);
gamma   = pi - 2*beta;
Delta   = acos((-rv_sat'/r_sat)*(rv_sun/r_sun));

c1 = 0;
c1 = 0;
c2 = 1;
c3 = 2;
eventValue = Delta - gamma;
eventRate  = 150*tanh(10*eventValue) 
