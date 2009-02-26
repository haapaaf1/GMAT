% REVISION HISTORY
% $Id: LibrationSEM.m,v 1.6 2008/10/15 01:01:22 edove Exp $

%--------------------------------------------------------------------------
%--------------------------Create Spacecraft ------------------------------
%--------------------------------------------------------------------------

Create Spacecraft Sat1;
GMAT Sat1.DateFormat = TAIModJulian;
GMAT Sat1.Epoch = 21545.000000000;
GMAT Sat1.CoordinateSystem = EarthSunMoonL1_MJ2000Eq;
GMAT Sat1.DisplayStateType = Cartesian;
GMAT Sat1.X = 0;
GMAT Sat1.Y = 0;
GMAT Sat1.Z = 0;
GMAT Sat1.VX = 0;
GMAT Sat1.VY = 0;
GMAT Sat1.VZ = 0;
GMAT Sat1.DryMass = 850;
GMAT Sat1.Cd = 2.2;
GMAT Sat1.Cr = 1.8;
GMAT Sat1.DragArea = 15;
GMAT Sat1.SRPArea = 1;

Create Spacecraft Sat2;
GMAT Sat2.DateFormat = TAIModJulian;
GMAT Sat2.Epoch = 21545.000000000;
GMAT Sat2.CoordinateSystem = EarthSunMoonL2_MJ2000Eq;
GMAT Sat2.DisplayStateType = Cartesian;
GMAT Sat2.X = 0;
GMAT Sat2.Y = 0;
GMAT Sat2.Z = 0;
GMAT Sat2.VX = 0;
GMAT Sat2.VY = 0;
GMAT Sat2.VZ = 0;
GMAT Sat2.DryMass = 850;
GMAT Sat2.Cd = 2.2;
GMAT Sat2.Cr = 1.8;
GMAT Sat2.DragArea = 15;
GMAT Sat2.SRPArea = 1;

Create Spacecraft Sat3;
GMAT Sat3.DateFormat = TAIModJulian;
GMAT Sat3.Epoch = 21545.000000000;
GMAT Sat3.CoordinateSystem = EarthSunMoonL3_MJ2000Eq;
GMAT Sat3.DisplayStateType = Cartesian;
GMAT Sat3.X = 0;
GMAT Sat3.Y = 0;
GMAT Sat3.Z = 0;
GMAT Sat3.VX = 0;
GMAT Sat3.VY = 0;
GMAT Sat3.VZ = 0;
GMAT Sat3.DryMass = 850;
GMAT Sat3.Cd = 2.2;
GMAT Sat3.Cr = 1.8;
GMAT Sat3.DragArea = 15;
GMAT Sat3.SRPArea = 1;

Create Spacecraft Sat4;
GMAT Sat4.DateFormat = TAIModJulian;
GMAT Sat4.Epoch = 21545.000000000;
GMAT Sat4.CoordinateSystem = EarthSunMoonL4_MJ2000Eq;
GMAT Sat4.DisplayStateType = Cartesian;
GMAT Sat4.X = 0;
GMAT Sat4.Y = 0;
GMAT Sat4.Z = 0;
GMAT Sat4.VX = 0;
GMAT Sat4.VY = 0;
GMAT Sat4.VZ = 0;
GMAT Sat4.DryMass = 850;
GMAT Sat4.Cd = 2.2;
GMAT Sat4.Cr = 1.8;
GMAT Sat4.DragArea = 15;
GMAT Sat4.SRPArea = 1;

Create Spacecraft Sat5;
GMAT Sat5.DateFormat = TAIModJulian;
GMAT Sat5.Epoch = 21545.000000000;
GMAT Sat5.CoordinateSystem = EarthSunMoonL5_MJ2000Eq;
GMAT Sat5.DisplayStateType = Cartesian;
GMAT Sat5.X = 0;
GMAT Sat5.Y = 0;
GMAT Sat5.Z = 0;
GMAT Sat5.VX = 0;
GMAT Sat5.VY = 0;
GMAT Sat5.VZ = 0;
GMAT Sat5.DryMass = 850;
GMAT Sat5.Cd = 2.2;
GMAT Sat5.Cr = 1.8;
GMAT Sat5.DragArea = 15;
GMAT Sat5.SRPArea = 1;

%--------------------------------------------------------------------------
%--------------------------Create Propagators------------------------------
%--------------------------------------------------------------------------


Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Earth;
GMAT DefaultProp_ForceModel.PrimaryBodies = {Earth};
GMAT DefaultProp_ForceModel.Drag = None;
GMAT DefaultProp_ForceModel.SRP = Off;
GMAT DefaultProp_ForceModel.ErrorControl = RSSStep;
GMAT DefaultProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT DefaultProp_ForceModel.Gravity.Earth.Order = 4;
GMAT DefaultProp_ForceModel.Gravity.Earth.PotentialFile = ./files/gravity/earth/JGM2.cof;

Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 60;
GMAT DefaultProp.Accuracy = 9.9999999999999994e-012;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;

GMAT SolarSystem.UseTTForEphemeris = true;

%--------------------------------------------------------------------------
%--------------------------Create Plots and Reports------------------------
%--------------------------------------------------------------------------


% Create OpenGLPlot DefaultOpenGL;
% GMAT DefaultOpenGL.Add = {Sat1, Sat2, Earth};
% GMAT DefaultOpenGL.CoordinateSystem = EarthSunMoonL1_MJ2000Eq;
% GMAT DefaultOpenGL.ViewPointReference = EarthSunMoonL1;
% GMAT DefaultOpenGL.ViewDirection = EarthSunMoonL1;
% GMAT DefaultOpenGL.ViewScaleFactor = 1;
% GMAT DefaultOpenGL.FixedFovAngle = 45;
% GMAT DefaultOpenGL.ViewUpCoordinateSystem = EarthSunMoonL1_MJ2000Eq;
% GMAT DefaultOpenGL.ViewUpAxis = Z;
% GMAT DefaultOpenGL.CelestialPlane = Off;
% GMAT DefaultOpenGL.XYPlane = On;
% GMAT DefaultOpenGL.WireFrame = Off;
% GMAT DefaultOpenGL.SolverIterations = None;
% GMAT DefaultOpenGL.Axes = Off;
% GMAT DefaultOpenGL.Grid = Off;
% GMAT DefaultOpenGL.SunLine = Off;
% GMAT DefaultOpenGL.PerspectiveMode = Off;
% GMAT DefaultOpenGL.UseFixedFov = Off;
% GMAT DefaultOpenGL.DataCollectFrequency = 1;
% GMAT DefaultOpenGL.UpdatePlotFrequency = 50;
% GMAT DefaultOpenGL.NumPointsToRedraw = 0;
% GMAT DefaultOpenGL.ShowPlot = true;

Create ReportFile ESM_LState;
GMAT ESM_LState.Filename = ./output/SystemTest/LibrationSEM.report;
GMAT ESM_LState.Precision = 16;


%--------------------------------------------------------------------------
%--------------------------Create Libration Points-------------------------
%--------------------------------------------------------------------------
Create Barycenter EarthMoonBary;
GMAT EarthMoonBary.BodyNames = {Earth, Luna};

Create LibrationPoint EarthSunMoonL1
GMAT EarthSunMoonL1.Primary = Sun;
GMAT EarthSunMoonL1.Secondary = EarthMoonBary;
GMAT EarthSunMoonL1.Point = L1;

Create LibrationPoint EarthSunMoonL2
GMAT EarthSunMoonL2.Primary = Sun;
GMAT EarthSunMoonL2.Secondary = EarthMoonBary;
GMAT EarthSunMoonL2.Point = L2;

Create LibrationPoint EarthSunMoonL3
GMAT EarthSunMoonL3.Primary = Sun;
GMAT EarthSunMoonL3.Secondary = EarthMoonBary;
GMAT EarthSunMoonL3.Point = L3;

Create LibrationPoint EarthSunMoonL4
GMAT EarthSunMoonL4.Primary = Sun;
GMAT EarthSunMoonL4.Secondary = EarthMoonBary;
GMAT EarthSunMoonL4.Point = L4;

Create LibrationPoint EarthSunMoonL5
GMAT EarthSunMoonL5.Primary = Sun;
GMAT EarthSunMoonL5.Secondary = EarthMoonBary;
GMAT EarthSunMoonL5.Point = L5;

%--------------------------------------------------------------------------
%--------------------------Create Coordinate Systems-----------------------
%--------------------------------------------------------------------------

Create CoordinateSystem EarthSunMoonL1_MJ2000Eq;
GMAT EarthSunMoonL1_MJ2000Eq.Origin = EarthSunMoonL1;
GMAT EarthSunMoonL1_MJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem EarthSunMoonL2_MJ2000Eq;
GMAT EarthSunMoonL2_MJ2000Eq.Origin = EarthSunMoonL2;
GMAT EarthSunMoonL2_MJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem EarthSunMoonL3_MJ2000Eq;
GMAT EarthSunMoonL3_MJ2000Eq.Origin = EarthSunMoonL3;
GMAT EarthSunMoonL3_MJ2000Eq.Axes = MJ2000Eq;


Create CoordinateSystem EarthSunMoonL4_MJ2000Eq;
GMAT EarthSunMoonL4_MJ2000Eq.Origin = EarthSunMoonL4;
GMAT EarthSunMoonL4_MJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem EarthSunMoonL5_MJ2000Eq;
GMAT EarthSunMoonL5_MJ2000Eq.Origin = EarthSunMoonL5;
GMAT EarthSunMoonL5_MJ2000Eq.Axes = MJ2000Eq;

%--------------------------------------------------------------------------
%--------------------------Create Coordinate Systems-----------------------
%--------------------------------------------------------------------------

% Libration Point 1
Report ESM_LState Sat1.A1ModJulian Sat1.X Sat1.Y Sat1.Z Sat1.VX Sat1.VY Sat1.VZ;
GMAT ESM_LState.WriteHeaders = Off;

% Libration Point 2
Report ESM_LState Sat2.A1ModJulian Sat2.X Sat2.Y Sat2.Z Sat2.VX Sat2.VY Sat2.VZ;

% Libration Point 3
Report ESM_LState Sat3.A1ModJulian Sat3.X Sat3.Y Sat3.Z Sat3.VX Sat3.VY Sat3.VZ;

% Libration Point 4
Report ESM_LState Sat4.A1ModJulian Sat4.X Sat4.Y Sat4.Z Sat4.VX Sat4.VY Sat4.VZ;

% Libration Point 5
Report ESM_LState Sat5.A1ModJulian Sat5.X Sat5.Y Sat5.Z Sat5.VX Sat5.VY Sat5.VZ;
