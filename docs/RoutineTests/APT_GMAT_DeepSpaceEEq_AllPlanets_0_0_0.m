%% $Id: APT_GMAT_DeepSpaceEEq_AllPlanets_0_0_0.m,v 1.1 2008/09/26 17:03:53 ljun Exp $

%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft DeepSpace;
GMAT DeepSpace.DateFormat = UTCGregorian;
GMAT DeepSpace.Epoch = 01 Jan 2000 12:00:00.000;
GMAT DeepSpace.CoordinateSystem = EarthMJ2000Eq;
GMAT DeepSpace.DisplayStateType = Cartesian;
GMAT DeepSpace.X = 56937994.01126761;
GMAT DeepSpace.Y = -1713823.49880654;
GMAT DeepSpace.Z = 1657873.05993214;
GMAT DeepSpace.VX = 0.04510534337579;
GMAT DeepSpace.VY = 10.6143828471321;
GMAT DeepSpace.VZ = 4.73506058316136;
GMAT DeepSpace.DryMass = 1000;
GMAT DeepSpace.Cd = 2.2;
GMAT DeepSpace.Cr = 1.2;
GMAT DeepSpace.DragArea = 20;
GMAT DeepSpace.SRPArea = 20;


%----------------------------------------
%---------- ForceModels
%----------------------------------------

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PointMasses = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.ErrorControl = RSSStep;


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 0.001;
GMAT RKV89.MaxStep = 30000;
GMAT RKV89.MaxStepAttempts = 50;


%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable OutputStepSize 

%----------------------------------------
%---------- Coordinate Systems
%----------------------------------------

Create CoordinateSystem EarthMJ2000Eq;
GMAT EarthMJ2000Eq.Origin = Earth;
GMAT EarthMJ2000Eq.Axes = MJ2000Eq;
GMAT EarthMJ2000Eq.UpdateInterval = 60;
GMAT EarthMJ2000Eq.OverrideOriginInterval = false;

Create CoordinateSystem EarthMJ2000Ec;
GMAT EarthMJ2000Ec.Origin = Earth;
GMAT EarthMJ2000Ec.Axes = MJ2000Ec;
GMAT EarthMJ2000Ec.UpdateInterval = 60;
GMAT EarthMJ2000Ec.OverrideOriginInterval = false;

Create CoordinateSystem EarthFixed;
GMAT EarthFixed.Origin = Earth;
GMAT EarthFixed.Axes = BodyFixed;
GMAT EarthFixed.UpdateInterval = 60;
GMAT EarthFixed.OverrideOriginInterval = false;


%----------------------------------------
%---------- Plots and Reports
%----------------------------------------

Create ReportFile DeepSpace_Report;
GMAT DeepSpace_Report.Filename = APT_GMAT_DeepSpaceEEq_AllPlanets_0_0_0.report;
GMAT DeepSpace_Report.Precision = 16;
GMAT DeepSpace_Report.WriteHeaders = Off;
GMAT DeepSpace_Report.LeftJustify = On;
GMAT DeepSpace_Report.ZeroFill = Off;
GMAT DeepSpace_Report.ColumnWidth = 20;
GMAT DeepSpace_Report.SolverIterations = None;

Create OpenGLPlot OpenGLPlot1;
GMAT OpenGLPlot1.Add = {DeepSpace};
GMAT OpenGLPlot1.OrbitColor = [ 255 ];
GMAT OpenGLPlot1.CoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot1.ViewPointReference = Earth;
GMAT OpenGLPlot1.ViewPointVector = [ 0 0 30000];
GMAT OpenGLPlot1.ViewDirection = Earth;
GMAT OpenGLPlot1.ViewScaleFactor = 10000;
GMAT OpenGLPlot1.FixedFovAngle = 45;
GMAT OpenGLPlot1.ViewUpCoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot1.ViewUpAxis = X;
GMAT OpenGLPlot1.CelestialPlane = Off;
GMAT OpenGLPlot1.XYPlane = On;
GMAT OpenGLPlot1.WireFrame = Off;
GMAT OpenGLPlot1.SolverIterations = None;
GMAT OpenGLPlot1.Axes = On;
GMAT OpenGLPlot1.Grid = Off;
GMAT OpenGLPlot1.EarthSunLines = Off;
GMAT OpenGLPlot1.UseInitialView = On;
GMAT OpenGLPlot1.PerspectiveMode = Off;
GMAT OpenGLPlot1.UseFixedFov = Off;
GMAT OpenGLPlot1.DataCollectFrequency = 1;
GMAT OpenGLPlot1.UpdatePlotFrequency = 50;
GMAT OpenGLPlot1.NumPointsToRedraw = 0;
GMAT OpenGLPlot1.ShowPlot = true;

Create OpenGLPlot OpenGLPlot2;
GMAT OpenGLPlot2.Add = {DeepSpace};
GMAT OpenGLPlot2.OrbitColor = [ 255 ];
GMAT OpenGLPlot2.CoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot2.ViewPointReference = Earth;
GMAT OpenGLPlot2.ViewPointVector = [ 0 0 30000];
GMAT OpenGLPlot2.ViewDirection = Earth;
GMAT OpenGLPlot2.ViewScaleFactor = 5000;
GMAT OpenGLPlot2.FixedFovAngle = 45;
GMAT OpenGLPlot2.ViewUpCoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot2.ViewUpAxis = Z;
GMAT OpenGLPlot2.CelestialPlane = Off;
GMAT OpenGLPlot2.XYPlane = On;
GMAT OpenGLPlot2.WireFrame = Off;
GMAT OpenGLPlot2.SolverIterations = None;
GMAT OpenGLPlot2.Axes = On;
GMAT OpenGLPlot2.Grid = Off;
GMAT OpenGLPlot2.EarthSunLines = Off;
GMAT OpenGLPlot2.UseInitialView = On;
GMAT OpenGLPlot2.PerspectiveMode = Off;
GMAT OpenGLPlot2.UseFixedFov = Off;
GMAT OpenGLPlot2.DataCollectFrequency = 1;
GMAT OpenGLPlot2.UpdatePlotFrequency = 50;
GMAT OpenGLPlot2.NumPointsToRedraw = 0;
GMAT OpenGLPlot2.ShowPlot = true;

Create XYPlot XYPlot1;
GMAT XYPlot1.IndVar = DeepSpace.A1ModJulian;
GMAT XYPlot1.Add = {DeepSpace.EarthMJ2000Eq.X};
GMAT XYPlot1.Grid = On;
GMAT XYPlot1.SolverIterations = None;
GMAT XYPlot1.ShowPlot = true;

Create XYPlot XYPlot2;
GMAT XYPlot2.IndVar = DeepSpace.A1ModJulian;
GMAT XYPlot2.Add = {DeepSpace.EarthMJ2000Eq.Y};
GMAT XYPlot2.Grid = On;
GMAT XYPlot2.SolverIterations = None;
GMAT XYPlot2.ShowPlot = true;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------


% Propagate based on preset propagation parameters
% and current stop conditions.
% Output Report file data for each propagation set in the FOR loop
Report DeepSpace_Report DeepSpace.A1ModJulian DeepSpace.EarthMJ2000Eq.X DeepSpace.EarthMJ2000Eq.Y DeepSpace.EarthMJ2000Eq.Z DeepSpace.EarthMJ2000Eq.VX DeepSpace.EarthMJ2000Eq.VY DeepSpace.EarthMJ2000Eq.VZ 

%%%For OutputStepSize = 1:365;
For OutputStepSize = 1:1:50;
   Propagate RKV89(DeepSpace) {DeepSpace.ElapsedSecs = 86400};
   Report DeepSpace_Report DeepSpace.A1ModJulian DeepSpace.EarthMJ2000Eq.X DeepSpace.EarthMJ2000Eq.Y DeepSpace.EarthMJ2000Eq.Z DeepSpace.EarthMJ2000Eq.VX DeepSpace.EarthMJ2000Eq.VY DeepSpace.EarthMJ2000Eq.VZ 
EndFor;


