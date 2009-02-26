% GMAT_EphemGen - Connects with GMAT using Matlab, propagates a spacecraft
% forwards or backwards in time, and outputs an ephemeris as a Matlab array 

%--------------------------------------------------------------------------
%----------------------------  Directions  --------------------------------
%--------------------------------------------------------------------------
% 1)  Open GMAT
% 2)  Right click on the GMAT Server icon in the Resource Tree 
%     and select Start
% 3)  Configure your spacecraft settings below
% 4)  Run this script

%--------------------------------------------------------------------------
%-------------------  Generate Ephemeris from GMAT  -----------------------
%--------------------------------------------------------------------------

clc

%  Using this approach you configure settings on the spacecraft,
%  propagator, and force model. For additional control over propagation 
%  times, you can enter the initial epoch, intermediate propagation times 
% (optional), and the final epoch.

% Clear existing structures
if exist('ForceModel','var')
    clear ForceModel
end
if exist('Propagator','var')
    clear Propagator
end
if exist('Spacecraft','var')
    clear Spacecraft
end

% Basic inputs
X0 = [9000 0 3000 0 7.53 2]'; % Initial Cartesian State [X Y Z VX VY VZ] Pos in km and Vel in km/s
Times = {'01 Jan 2000 11:59:28.000','02 Jan 2000 00:59:28.000','02 Jan 2000 05:59:28.000','02 Jan 2000 08:59:28.000','02 Jan 2000 11:59:28.000'}; % Times to Start at, Propagate to, and End
TempEphemLoc = 'c:\TempEphem.txt'; % Text file name and location for the ascii storing of the ephemeris
reportFlag = 0; % Flag for outputing propagation steps in between the times specified in the Times array [ 1 = On | 0 = Off ]

% Advanced inputs
ForceModel.CentralBody = 'Earth';
ForceModel.PrimaryBodies = '{Earth}';
ForceModel.Drag = 'None';
ForceModel.SRP = 'Off';
ForceModel.ErrorControl = 'RSSStep';
ForceModel.GravityField.Earth.Degree = 4;
ForceModel.GravityField.Earth.Order = 4;
ForceModel.GravityField.Earth.PotentialFile = 'JGM2';

Propagator.Type = 'RungeKutta89';
Propagator.InitialStepSize = 0.001; % sec
Propagator.Accuracy = 1e-012;
Propagator.MinStep = 0.001; % sec
Propagator.MaxStep = 2700; % sec
Propagator.MaxStepAttempts = 50;

Spacecraft.CoordinateSystem = 'EarthMJ2000Eq';
Spacecraft.DateFormat = 'UTCGregorian'; % Must be the same format as the values in the Times array
Spacecraft.Epoch = Times{1};
Spacecraft.Cd = 2.0;
Spacecraft.Cr = 1.4;
Spacecraft.DryMass = 200; % kg
Spacecraft.DragArea = 20; % m^2
Spacecraft.SRPArea = 20; % m^2

OpenGMAT % Open a connection to GMAT

% Call Function to convert Gregorian States to Modified Julian Date States
% *** WARNING *** 
% A significant performance hit will be experienced vs. using a vector of 
% modified julian times for the Times array.
% *** WARNING *** 
if isempty(strfind(lower(Spacecraft.DateFormat),'gregorian')) == 0; % Gregorian DateFormat used
    tic
    [Times] = GMAT_EphemGenGreg2MJD(Times);
    convertTime = toc;
    disp('Conversion of Gregorian Times to Modified Julian Times,');
    disp('using GMAT as the conversion tool, took:');
    disp([num2str(convertTime),' seconds']);
end;

tic
% Call Function to send commands to GMAT
[Ephem] = GMAT_EphemGenFunc(X0, Times, Spacecraft, ForceModel, Propagator, TempEphemLoc, reportFlag);
runTime = toc;
disp(' ');
disp(['GMAT propagation completed in ',num2str(runTime)]);
