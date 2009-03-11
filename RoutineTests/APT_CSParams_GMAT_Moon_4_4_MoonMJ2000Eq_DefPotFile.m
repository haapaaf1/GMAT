
%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft MoonSC;
GMAT MoonSC.DateFormat = UTCGregorian;
GMAT MoonSC.Epoch = 01 Jun 2004 12:00:00.000;
GMAT MoonSC.CoordinateSystem = MoonMJ2000Eq;
GMAT MoonSC.DisplayStateType = Cartesian;
GMAT MoonSC.X = -1486.792117191537;
GMAT MoonSC.Y = 0;
GMAT MoonSC.Z = 1486.792117191537;
GMAT MoonSC.VX = -0.142927729144255;
GMAT MoonSC.VY = -1.631407624437537;
GMAT MoonSC.VZ = 0.142927729144255;
GMAT MoonSC.DryMass = 1000;
GMAT MoonSC.Cd = 2.2;
GMAT MoonSC.Cr = 1.2;
GMAT MoonSC.DragArea = 20;
GMAT MoonSC.SRPArea = 20;


%----------------------------------------
%---------- ForceModels
%----------------------------------------

Create ForceModel Moon2Body;
GMAT Moon2Body.CentralBody = Luna;
GMAT Moon2Body.PrimaryBodies = {Luna};
GMAT Moon2Body.Drag = None;
GMAT Moon2Body.SRP = Off;
GMAT Moon2Body.ErrorControl = RSSStep;
GMAT Moon2Body.Gravity.Luna.Degree = 4;
GMAT Moon2Body.Gravity.Luna.Order = 4;
%%GMAT Moon2Body.Gravity.Luna.PotentialFile = ;


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator RKV89;
GMAT RKV89.FM = Moon2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
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

Create CoordinateSystem MoonMJ2000Eq;
GMAT MoonMJ2000Eq.Origin = Luna;
GMAT MoonMJ2000Eq.Axes = MJ2000Eq;
GMAT MoonMJ2000Eq.UpdateInterval = 60;
GMAT MoonMJ2000Eq.OverrideOriginInterval = false;


%----------------------------------------
%---------- Plots and Reports
%----------------------------------------

Create ReportFile Moon_Report;
GMAT Moon_Report.Filename = APT_CSParams_GMAT_Moon_4_4_MoonMJ2000Eq_DefPotFile.report;
GMAT Moon_Report.Precision = 16;
GMAT Moon_Report.WriteHeaders = On;
GMAT Moon_Report.LeftJustify = On;
GMAT Moon_Report.ZeroFill = Off;
GMAT Moon_Report.ColumnWidth = 25;
GMAT Moon_Report.SolverIterations = None;

Create XYPlot XYPlot1;
GMAT XYPlot1.IndVar = MoonSC.A1ModJulian;
GMAT XYPlot1.Add = {MoonSC.MoonMJ2000Eq.X, MoonSC.MoonMJ2000Eq.Y, MoonSC.MoonMJ2000Eq.Z};
GMAT XYPlot1.Grid = On;
GMAT XYPlot1.SolverIterations = None;
GMAT XYPlot1.ShowPlot = true;

Create XYPlot XYPlot2;
GMAT XYPlot2.IndVar = MoonSC.A1ModJulian;
GMAT XYPlot2.Add = {MoonSC.MoonMJ2000Eq.HX, MoonSC.MoonMJ2000Eq.HY, MoonSC.MoonMJ2000Eq.HZ};
GMAT XYPlot2.Grid = On;
GMAT XYPlot2.SolverIterations = None;
GMAT XYPlot2.ShowPlot = true;

Create OpenGLPlot OpenGLPlot1;
GMAT OpenGLPlot1.Add = {MoonSC};
GMAT OpenGLPlot1.OrbitColor = [ 255 ];
GMAT OpenGLPlot1.CoordinateSystem = MoonMJ2000Eq;
GMAT OpenGLPlot1.ViewPointReference = Luna;
GMAT OpenGLPlot1.ViewPointVector = [ 0 0 30000];
GMAT OpenGLPlot1.ViewDirection = Luna;
GMAT OpenGLPlot1.ViewScaleFactor = 1;
GMAT OpenGLPlot1.FixedFovAngle = 45;
GMAT OpenGLPlot1.ViewUpCoordinateSystem = MoonMJ2000Eq;
GMAT OpenGLPlot1.ViewUpAxis = Z;
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
GMAT OpenGLPlot2.Add = {MoonSC};
GMAT OpenGLPlot2.OrbitColor = [ 255 ];
GMAT OpenGLPlot2.CoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot2.ViewPointReference = Earth;
GMAT OpenGLPlot2.ViewPointVector = [ 0 0 30000];
GMAT OpenGLPlot2.ViewDirection = Earth;
GMAT OpenGLPlot2.ViewScaleFactor = 15;
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


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------


% Propagate based on preset propagation parameters
% and current stop conditions.
% Output Report file data for each propagation set in the FOR loop
Report Moon_Report MoonSC.A1ModJulian MoonSC.MoonMJ2000Eq.X MoonSC.MoonMJ2000Eq.Y MoonSC.MoonMJ2000Eq.Z MoonSC.MoonMJ2000Eq.VX MoonSC.MoonMJ2000Eq.VY MoonSC.MoonMJ2000Eq.VZ MoonSC.MoonMJ2000Eq.VMAG MoonSC.MoonMJ2000Eq.RAV MoonSC.MoonMJ2000Eq.HX MoonSC.MoonMJ2000Eq.HY MoonSC.MoonMJ2000Eq.HZ MoonSC.MoonMJ2000Eq.AOP MoonSC.MoonMJ2000Eq.DEC MoonSC.MoonMJ2000Eq.DECV MoonSC.MoonMJ2000Eq.INC MoonSC.MoonMJ2000Eq.RA MoonSC.MoonMJ2000Eq.RAAN 
GMAT Moon_Report.WriteHeaders = Off;

For OutputStepSize = 1:1:50;
   Propagate RKV89(MoonSC) {MoonSC.ElapsedSecs = 600};
   Report Moon_Report MoonSC.A1ModJulian MoonSC.MoonMJ2000Eq.X MoonSC.MoonMJ2000Eq.Y MoonSC.MoonMJ2000Eq.Z MoonSC.MoonMJ2000Eq.VX MoonSC.MoonMJ2000Eq.VY MoonSC.MoonMJ2000Eq.VZ MoonSC.MoonMJ2000Eq.VMAG MoonSC.MoonMJ2000Eq.RAV MoonSC.MoonMJ2000Eq.HX MoonSC.MoonMJ2000Eq.HY MoonSC.MoonMJ2000Eq.HZ MoonSC.MoonMJ2000Eq.AOP MoonSC.MoonMJ2000Eq.DEC MoonSC.MoonMJ2000Eq.DECV MoonSC.MoonMJ2000Eq.INC MoonSC.MoonMJ2000Eq.RA MoonSC.MoonMJ2000Eq.RAAN 
EndFor;


