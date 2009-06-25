function [time,Z,E] = RK45(frhs,tspan,z0,eventFunc,desiredValue)

%  function [tspan,Z] = RK45(frhs,Time,z0)
%
%  Runge-Kutta fourth order integrator
%
%  Variable I/O
%  UOI - > Units of Input
%  Variable Name    I/0    Units   Dim.       Description
%  frhs              I     None    None       'filename' where filename.m is m-file
%  Time              I     UOI     1x3        [t0 tf tstep]
%  z0                I     UOI     nx1        initial conditions
%  Z                 O     UOI     Nxn        Matrix of states corresponding to time vector T
%  T                 O     UOI     Nx1        Vector of times

N=length(tspan);
n=length(z0);
z0=reshape(z0,n,1);
Z=[z0 zeros(n,N-1)];
w=z0;
Z_column=[z0';zeros(N-1,n)];
for i=1:N-1
   h=tspan(i+1)-tspan(i);
   t=tspan(i);
   K1=h*feval(frhs,t,w);
   K2=h*feval(frhs,t+h/2,w+K1/2);
   K3=h*feval(frhs,t+h/2,w+K2/2);
   K4=h*feval(frhs,t+h,w+K3);
   w=w+(K1+2*K2+2*K3+K4)/6;
   time(i,1) = t;
   Z(:,i+1)=w;
   Z_column(i+1,:)=w';
   E(i,1) = feval(eventFunc,w,desiredValue);
end
tspan=tspan';
Z=Z_column;
