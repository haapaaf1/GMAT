
%  This script tests propagation using Earth as the central body.  


% --------------------------------------
%          Define the Force Model
% --------------------------------------
ForceModel.CentralBody = 'Earth';
ForceModel.PrimaryBodies = {'Earth'};
ForceModel.PointMasses = {'Earth' 'Luna' 'Sun'};
ForceModel.SRP = 'Off';
ForceModel.SolarRadiationPressure.Flux = 1367;
ForceModel.SolarRadiationPressure.Nominal_Sun = 149597870.691;

% --------------------------------------
%          Define the Spacecraft
% --------------------------------------
MMSRef.Epoch = '01 Jan 2015 00:00:00.000';
MMSRef.X = 41742753.67560511800;
MMSRef.Y = 1965148.08870323190;
MMSRef.Z = 2743813.25413492600;
MMSRef.VX = -0.08417878433;
MMSRef.VY = 8.18939924459;
MMSRef.VZ = 3.16358327771;

% --------------------------------------
%          Propagate the Spacecraft
% --------------------------------------
TOF     = 86400*365;
[t, X, Phi] = Propagate(MMSRef,ForceModel,TOF);

% --------------------------------------
%          Output
% --------------------------------------
disp('  ')
disp('-------------------------------')
disp('----Propagation Complete-------')
disp('-------------------------------')
disp('  ')
n =  size(X,1);
disp(['               X-Comp.            Y-Comp.           Z- Comp.    ']);
disp(['Position   ' num2str( X(n,1:3),12) ]);
disp(['Velocity   ' num2str( X(n,4:6),12) ]);
disp('  ')

plot3(X(:,1),X(:,2),X(:,3))
axis equal








