function [xdot] = orbitODE(t,x)

mu = 398600.4415;
rv = x(1:3,1); 
r = norm(rv);
xdot(1:3,1) = x(4:6,1);
xdot(4:6,1) = -mu*rv/norm(rv)^3 + x(4:6,1)*.03e-7;
%xdot(7,1)   = rv'*x(4:6,1);