function [xdot] = orbitODE(t,x)

mu = 398600.4415;
rv = x(1:3,1); 
r = norm(rv);
xdot(1:3,1) = x(4:6,1);
xdot(4:6,1) = -mu*rv/norm(rv)^3;