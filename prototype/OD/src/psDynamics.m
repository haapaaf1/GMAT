function [Xdot,dXdotdX,dXdotdu,dXdotdt] = psDynamics(t,X,U)

%%  Intializations
mu  = 398600.4415;
rv  = X(1:3,1);
vv  = X(4:6,1);
r   = norm(rv);
%m   = X(7,1);
Isp = 300;

%%  Compute the dynamics
Xdot(1:3,1) =  vv;
Xdot(4:6)   = -mu/norm(rv)^3*rv 
if ~isempty(U)
    Xdot(4:6) = Xdot(4:6) + + U/m;
    Xdot(7,1)   =  U/Isp;
    dXdotdX(7,7)     = -1/m^2;
end


%%  Assemble the partials w/r/t the state
dXdotdX(1:3,1:3) = zeros(3,3);
dXdotdX(4:6,1:3) = -mu/r^3*eye(3) + 3*mu*(rv*rv')/r^5;
dXdotdX(1:3,4:6) = eye(3);


%%  Assemble the partials w/r/t the state
dXdotdu = zeros(3,7);
%dXdotdu(:,4:6) = eye(3)/m;  

%%  Assemble the partials w/r/t the time
dXdotdt = zeros(1,7);




