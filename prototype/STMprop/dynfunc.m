function [Xdot] = dynfunc(t,x,dummy, ForceModel)

global SolarSystem

%  Initializations
xdot      = zeros(6,1);
A         = zeros(6,6);
phi       = vec2sqrmat(x(7:42));
xdot_PM   = zeros(6,1);
xdot_SRP  = zeros(6,1);
xdot_Drag = zeros(6,1);
xdot_NSG  = zeros(6,1);
A_PM      = zeros(6,6);
A_SRP     = zeros(6,6);
A_Drag    = zeros(6,6);
A_NSG     = zeros(6,6);

%  Call point mass model
[xdot_PM,A_PM] = deriv_pointmass(t,x,ForceModel);

switch ForceModel.SRP

    case 'On'
        
        [xdot_SRP,A_SRP] = deriv_SRP(t,x,ForceModel);
        %A_SRP = numjac('deriv_SRP',t,x(1:6),xdot_SRP(1:6),1e-13*ones(6,1),[],0,[],[],ForceModel);

end

xdot = xdot_PM + xdot_SRP;
A    = A_PM + A_SRP;
phidot   = A*phi;
Xdot  = [xdot;sqrmat2vec(phidot)];
