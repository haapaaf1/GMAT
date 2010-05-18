% This is the test script for defining
% user specified celestial object as a central
% body. The central body here is a user defined 
% planet. The body around the central body is a 
% user defined moon.

%---------------------------------------------------------
% Create variables needed to execute the script
%---------------------------------------------------------
Create Variable rPtoMX, rPtoMY, rPtoMZ;

%----------------------------------------
%---------- Define the Solar System motion
%----------------------------------------
GMAT SolarSystem.EphemerisSource = 'TwoBodyPropagation';

%----------------------------------------
%---------- User Defined Planet
%----------------------------------------
Create Planet newPlanet;
GMAT newPlanet.EquatorialRadius = 1162;
GMAT newPlanet.Flattening = 0;
GMAT newPlanet.Mu = 981.600887707;
GMAT newPlanet.PosVelSource = 'TwoBodyPropagation';
GMAT newPlanet.CentralBody = 'Sun';
GMAT newPlanet.RotationDataSource = 'IAUSimplified';
GMAT newPlanet.InitialEpoch = 21544.50037076827;
GMAT newPlanet.SMA = 5909627293.567856;
GMAT newPlanet.ECC = 0.2492877787191154;
GMAT newPlanet.INC = 23.4740184346088;
GMAT newPlanet.RAAN = 43.9983031044403;
GMAT newPlanet.AOP = 183.031649978597;
GMAT newPlanet.TA = 25.51366421665317;
GMAT newPlanet.OrientationEpoch = 21545;
GMAT newPlanet.SpinAxisRAConstant = 313.02;
GMAT newPlanet.SpinAxisRARate = 0;
GMAT newPlanet.SpinAxisDECConstant = 9.09;
GMAT newPlanet.SpinAxisDECRate = 0;
GMAT newPlanet.RotationConstant = 236.77;
GMAT newPlanet.RotationRate = -56.3623195;
GMAT newPlanet.TextureMapFileName = './GenericCelestialBody.jpg';
GMAT newPlanet.NutationUpdateInterval = 60;

%--------------------User Defined Moon--------------------------------

Create Moon newMoon;
GMAT newMoon.EquatorialRadius = 1162;
GMAT newMoon.Flattening = 0;
GMAT newMoon.Mu = 981.600887707;
GMAT newMoon.PosVelSource = 'TwoBodyPropagation';
GMAT newMoon.CentralBody = 'newPlanet';
GMAT newMoon.RotationDataSource = 'IAUSimplified';
GMAT newMoon.InitialEpoch = 21544.50037076827;
GMAT newMoon.SMA = 390845.8;
GMAT newMoon.ECC = 0.249;
GMAT newMoon.INC = 1.4740;
GMAT newMoon.RAAN = 21.9;
GMAT newMoon.AOP = 24.031;
GMAT newMoon.TA = 15.513;
GMAT newMoon.OrientationEpoch = 21545;
GMAT newMoon.SpinAxisRAConstant = 313.02;
GMAT newMoon.SpinAxisRARate = 0;
GMAT newMoon.SpinAxisDECConstant = 9.09;
GMAT newMoon.SpinAxisDECRate = 0;
GMAT newMoon.RotationConstant = 236.77;
GMAT newMoon.RotationRate = -56.3623195;
GMAT newMoon.TextureMapFileName = './GenericCelestialBody.jpg';

%------------------------------Create Coordinate Systems-------------------
Create CoordinateSystem EarthMJ2000Eq;
GMAT EarthMJ2000Eq.Origin = Earth;
GMAT EarthMJ2000Eq.Axes = MJ2000Eq;
GMAT EarthMJ2000Eq.UpdateInterval = 60;
GMAT EarthMJ2000Eq.OverrideOriginInterval = false;

Create CoordinateSystem newPlanetMJ2000Eq;
GMAT newPlanetMJ2000Eq.Origin = newPlanet;
GMAT newPlanetMJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem newMoonMJ2000Eq;
GMAT newPlanetMJ2000Eq.Origin = newMoon;
GMAT newPlanetMJ2000Eq.Axes   = MJ2000Eq;
%%GMAT newMoonMJ2000Eq.Origin = newMoon;
%%GMAT newMoonMJ2000Eq.Axes   = MJ2000Eq;

%------------------------------Create a spacecraft------------------------

% Create spacecraft Sat1 and define its orbit
Create Spacecraft Sat1;
GMAT Sat1.DateFormat = 'TAIModJulian';
GMAT Sat1.Epoch = 21544.50037076827;
GMAT Sat1.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat1.DisplayStateType = Cartesian;
GMAT Sat1.SMA = 11295.67681418613;
GMAT Sat1.ECC = 0.2032349944098091;
GMAT Sat1.INC = 0;
GMAT Sat1.RAAN = 0;
GMAT Sat1.AOP = 0;
GMAT Sat1.TA = 0;
GMAT Sat1.Id = 'SatId';

%----------------------------Create ForceModels----------------------

%  Define Force Model with point mass only
Create ForceModel PointMass;
GMAT PointMass.CentralBody = Earth;
GMAT PointMass.PrimaryBodies = {Earth};
GMAT PointMass.Drag = None;
GMAT PointMass.SRP = Off;
GMAT PointMass.ErrorControl = RSSStep;
GMAT PointMass.GravityField.Earth.Degree = 0;
GMAT PointMass.GravityField.Earth.Order = 0;
GMAT PointMass.GravityField.Earth.PotentialFile = 'JGM2.cof';

%--------------------------- Propagators------------------------------

Create Propagator propModel;
GMAT propModel.FM = PointMass;
GMAT propModel.Type = RungeKutta89;
GMAT propModel.InitialStepSize = 60;
GMAT propModel.Accuracy = 9.999999999999999e-12;
GMAT propModel.MinStep = 0.001;
GMAT propModel.MaxStep = 2700;
GMAT propModel.MaxStepAttempts = 50;

%------------------------------ Create Report -----------------------------
Create ReportFile dataReport;
%%GMAT dataReport.Filename = '.\output\AcceptTest\UDCB_GMAT_CentralBody.report';
GMAT dataReport.Filename = 'Bug1913-UDCB_GMAT_CentralBody.report';
GMAT dataReport.Precision = 16;
GMAT dataReport.WriteHeaders = On;
GMAT dataReport.LeftJustify = On;
GMAT dataReport.ZeroFill = Off;
GMAT dataReport.ColumnWidth = 20;

%------------------------------Begin script---------------------------------

% Propagate the satellite
Propagate propModel(Sat1) {Sat1.ElapsedSecs = 8640.0};

% Position of the user defined moon relative to the user defined planet
GMAT rPtoMX = Sat1.newPlanetMJ2000Eq.X - Sat1.newMoonMJ2000Eq.X;
GMAT rPtoMY = Sat1.newPlanetMJ2000Eq.Y - Sat1.newMoonMJ2000Eq.Y;
GMAT rPtoMZ = Sat1.newPlanetMJ2000Eq.Z - Sat1.newMoonMJ2000Eq.Z;

% Report data to file
Report dataReport rPtoMX rPtoMY rPtoMZ;
