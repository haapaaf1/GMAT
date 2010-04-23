%% $Id: LibrationTest_GMAT_SE.m,v 1.6 2007/12/26 18:49:23 edove Exp $

%--------------------------------------------------------------------------
%--------------------------Create Spacecraft ------------------------------
%--------------------------------------------------------------------------

Create Spacecraft Sat1;
GMAT Sat1.DateFormat = UTCGregorian;
GMAT Sat1.Epoch = 01 Jan 2000 11:59:28.000;
GMAT Sat1.CoordinateSystem = EarthSunL1_MJ2000Eq;
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
GMAT Sat2.DateFormat = UTCGregorian;
GMAT Sat2.Epoch = 01 Jan 2000 11:59:28.000;
GMAT Sat2.CoordinateSystem = EarthSunL2_MJ2000Eq;
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
GMAT Sat3.DateFormat = UTCGregorian;
GMAT Sat3.Epoch = 01 Jan 2000 11:59:28.000;
GMAT Sat3.CoordinateSystem = EarthSunL3_MJ2000Eq;
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
GMAT Sat4.DateFormat = UTCGregorian;
GMAT Sat4.Epoch = 01 Jan 2000 11:59:28.000;
GMAT Sat4.CoordinateSystem = EarthSunL4_MJ2000Eq;
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
GMAT Sat5.DateFormat = UTCGregorian;
GMAT Sat5.Epoch = 01 Jan 2000 11:59:28.000;
GMAT Sat5.CoordinateSystem = EarthSunL5_MJ2000Eq;
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


Create OpenGLPlot DefaultOpenGL;
GMAT DefaultOpenGL.Add = {Sat1, Sat2, Earth};
GMAT DefaultOpenGL.CoordinateSystem = EarthSunL1_MJ2000Eq;
GMAT DefaultOpenGL.ViewPointRef = EarthSunL1;
GMAT DefaultOpenGL.ViewPointVector = Vector;
GMAT DefaultOpenGL.ViewDirection = EarthSunL1;
GMAT DefaultOpenGL.ViewScaleFactor = 1;
GMAT DefaultOpenGL.FixedFovAngle = 45;
GMAT DefaultOpenGL.ViewUpCoordinateSystem = EarthSunL1_MJ2000Eq;
GMAT DefaultOpenGL.ViewUpAxis = Z;
GMAT DefaultOpenGL.CelestialPlane = Off;
GMAT DefaultOpenGL.XYPlane = On;
GMAT DefaultOpenGL.WireFrame = Off;
GMAT DefaultOpenGL.TargetStatus = Off;
GMAT DefaultOpenGL.Axes = Off;
GMAT DefaultOpenGL.Grid = Off;
GMAT DefaultOpenGL.EarthSunLines = Off;
GMAT DefaultOpenGL.PerspectiveMode = Off;
GMAT DefaultOpenGL.UseFixedFov = Off;
GMAT DefaultOpenGL.DataCollectFrequency = 1;
GMAT DefaultOpenGL.UpdatePlotFrequency = 50;
GMAT DefaultOpenGL.NumPointsToRedraw = 0;
GMAT DefaultOpenGL.ShowPlot = true;

Create ReportFile ES_LState;
GMAT ES_LState.Filename = ./output/AcceptTest/LibrationTest_GMAT_SE.report;
GMAT ES_LState.Precision = 16;

%--------------------------------------------------------------------------
%--------------------------Create Libration Points-------------------------
%--------------------------------------------------------------------------

Create LibrationPoint EarthSunL1
GMAT EarthSunL1.Primary = Sun;
GMAT EarthSunL1.Secondary = Earth;
GMAT EarthSunL1.Point = L1;

Create LibrationPoint EarthSunL2
GMAT EarthSunL2.Primary = Sun;
GMAT EarthSunL2.Secondary = Earth;
GMAT EarthSunL2.Point = L2;

Create LibrationPoint EarthSunL3
GMAT EarthSunL3.Primary = Sun;
GMAT EarthSunL3.Secondary = Earth;
GMAT EarthSunL3.Point = L3;

Create LibrationPoint EarthSunL4
GMAT EarthSunL4.Primary = Sun;
GMAT EarthSunL4.Secondary = Earth;
GMAT EarthSunL4.Point = L4;

Create LibrationPoint EarthSunL5
GMAT EarthSunL5.Primary = Sun;
GMAT EarthSunL5.Secondary = Earth;
GMAT EarthSunL5.Point = L5;
%
%%--------------------------------------------------------------------------
%%--------------------------Create Coordinate Systems-----------------------
%%--------------------------------------------------------------------------
%
Create CoordinateSystem EarthSunL1_MJ2000Eq;
GMAT EarthSunL1_MJ2000Eq.Origin = EarthSunL1;
GMAT EarthSunL1_MJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem EarthSunL2_MJ2000Eq;
GMAT EarthSunL2_MJ2000Eq.Origin = EarthSunL2;
GMAT EarthSunL2_MJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem EarthSunL3_MJ2000Eq;
GMAT EarthSunL3_MJ2000Eq.Origin = EarthSunL3;
GMAT EarthSunL3_MJ2000Eq.Axes = MJ2000Eq;


Create CoordinateSystem EarthSunL4_MJ2000Eq;
GMAT EarthSunL4_MJ2000Eq.Origin = EarthSunL4;
GMAT EarthSunL4_MJ2000Eq.Axes = MJ2000Eq;

Create CoordinateSystem EarthSunL5_MJ2000Eq;
GMAT EarthSunL5_MJ2000Eq.Origin = EarthSunL5;
GMAT EarthSunL5_MJ2000Eq.Axes = MJ2000Eq;

%%--------------------------------------------------------------------------
%%--------------------------Create Coordinate Systems-----------------------
%%--------------------------------------------------------------------------

% Libration 1
Report ES_LState Sat1.A1ModJulian Sat1.X Sat1.Y Sat1.Z Sat1.VX Sat1.VY Sat1.VZ;
GMAT ES_LState.WriteHeaders = Off;

% Libration 2
Report ES_LState Sat2.A1ModJulian Sat2.X Sat2.Y Sat2.Z Sat2.VX Sat2.VY Sat2.VZ;

% Libration 3
Report ES_LState Sat3.A1ModJulian Sat3.X Sat3.Y Sat3.Z Sat3.VX Sat3.VY Sat3.VZ;

% Libration 4
Report ES_LState Sat4.A1ModJulian Sat4.X Sat4.Y Sat4.Z Sat4.VX Sat4.VY Sat4.VZ;

% Libration 5
Report ES_LState Sat5.A1ModJulian Sat5.X Sat5.Y Sat5.Z Sat5.VX Sat5.VY Sat5.VZ;