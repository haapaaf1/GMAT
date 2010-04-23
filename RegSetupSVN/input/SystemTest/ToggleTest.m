%$Id: ToggleTest.m,v 1.8 2008/09/05 20:46:56 edove Exp $
% 
% Script used to exercise the XY plot for data that is single 
% valued (i.e. a "function") and data that in multivalued (a
% "nonfunction"; hence the script name).

%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft Sat;
GMAT Sat.DateFormat = TAIModJulian;
GMAT Sat.Epoch = 21545;
GMAT Sat.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat.DisplayStateType = Keplerian;
GMAT Sat.SMA = 11999.99999999999;
GMAT Sat.ECC = 0.2500000000000009;
GMAT Sat.INC = 53.00000000000001;
GMAT Sat.RAAN = 60.00000000000001;
GMAT Sat.AOP = 0;
GMAT Sat.TA = 125.0000000000001;
GMAT Sat.DryMass = 850;
GMAT Sat.Cd = 2.2;
GMAT Sat.Cr = 1.8;
GMAT Sat.DragArea = 15;
GMAT Sat.SRPArea = 1;


%----------------------------------------
%---------- ForceModels
%----------------------------------------

Create ForceModel prop_ForceModel;
GMAT prop_ForceModel.CentralBody = Earth;
GMAT prop_ForceModel.PrimaryBodies = {Earth};
GMAT prop_ForceModel.Drag = None;
GMAT prop_ForceModel.SRP = Off;
GMAT prop_ForceModel.ErrorControl = RSSStep;
GMAT prop_ForceModel.Gravity.Earth.Degree = 4;
GMAT prop_ForceModel.Gravity.Earth.Order = 4;
GMAT prop_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator prop;
GMAT prop.FM = prop_ForceModel;
GMAT prop.Type = RungeKutta89;
GMAT prop.InitialStepSize = 60;
GMAT prop.Accuracy = 9.999999999999999e-012;
GMAT prop.MinStep = 0.001;
GMAT prop.MaxStep = 2700;
GMAT prop.MaxStepAttempts = 50;


%----------------------------------------
%---------- Parameters
%----------------------------------------

%Here are some variables, used to test some math functions
Create Variable XYMag;

Create Variable XZMag;

Create Variable YZMag;

Create Variable RCalc;

Create Variable I;



%----------------------------------------
%---------- Subscribers
%----------------------------------------

% 2 plots
Create XYPlot XYZT1;
GMAT XYZT1.IndVar = Sat.A1ModJulian;
GMAT XYZT1.Add = {Sat.EarthMJ2000Eq.X, Sat.EarthMJ2000Eq.Y, Sat.EarthMJ2000Eq.Z};
GMAT XYZT1.Grid = On;
GMAT XYZT1.SolverIterations = None;
GMAT XYZT1.ShowPlot = true;

Create XYPlot XYZT2;
GMAT XYZT2.IndVar = Sat.A1ModJulian;
GMAT XYZT2.Add = {Sat.EarthMJ2000Eq.X, Sat.EarthMJ2000Eq.Y, Sat.EarthMJ2000Eq.Z};
GMAT XYZT2.Grid = On;
GMAT XYZT2.SolverIterations = None;
GMAT XYZT2.ShowPlot = true;

Create OpenGLPlot OpenGLPlot1;
GMAT OpenGLPlot1.Add = {Sat, Earth};
GMAT OpenGLPlot1.CoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot1.ViewPointReference = Earth;
GMAT OpenGLPlot1.ViewDirection = Earth;
GMAT OpenGLPlot1.ViewScaleFactor = 1;
GMAT OpenGLPlot1.FixedFovAngle = 45;
GMAT OpenGLPlot1.ViewUpCoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot1.ViewUpAxis = Z;
GMAT OpenGLPlot1.CelestialPlane = Off;
GMAT OpenGLPlot1.XYPlane = On;
GMAT OpenGLPlot1.WireFrame = Off;
GMAT OpenGLPlot1.SolverIterations = None;
GMAT OpenGLPlot1.Axes = On;
GMAT OpenGLPlot1.Grid = On;
GMAT OpenGLPlot1.SunLine = On;
GMAT OpenGLPlot1.UseInitialView = On;
GMAT OpenGLPlot1.PerspectiveMode = Off;
GMAT OpenGLPlot1.UseFixedFov = Off;
GMAT OpenGLPlot1.DataCollectFrequency = 1;
GMAT OpenGLPlot1.UpdatePlotFrequency = 50;
GMAT OpenGLPlot1.NumPointsToRedraw = 0;
GMAT OpenGLPlot1.ShowPlot = true;

Create OpenGLPlot OpenGLPlot2;
GMAT OpenGLPlot2.Add = {Sat, Earth};
GMAT OpenGLPlot2.CoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot2.ViewPointReference = Earth;
GMAT OpenGLPlot2.ViewDirection = Earth;
GMAT OpenGLPlot2.ViewScaleFactor = 1;
GMAT OpenGLPlot2.FixedFovAngle = 45;
GMAT OpenGLPlot2.ViewUpCoordinateSystem = EarthMJ2000Eq;
GMAT OpenGLPlot2.ViewUpAxis = Z;
GMAT OpenGLPlot2.CelestialPlane = Off;
GMAT OpenGLPlot2.XYPlane = On;
GMAT OpenGLPlot2.WireFrame = Off;
GMAT OpenGLPlot2.SolverIterations = None;
GMAT OpenGLPlot2.Axes = On;
GMAT OpenGLPlot2.Grid = On;
GMAT OpenGLPlot2.SunLine = On;
GMAT OpenGLPlot2.UseInitialView = On;
GMAT OpenGLPlot2.PerspectiveMode = Off;
GMAT OpenGLPlot2.UseFixedFov = Off;
GMAT OpenGLPlot2.DataCollectFrequency = 1;
GMAT OpenGLPlot2.UpdatePlotFrequency = 50;
GMAT OpenGLPlot2.NumPointsToRedraw = 0;
GMAT OpenGLPlot2.ShowPlot = true;

Create ReportFile ReportFile1;
GMAT ReportFile1.Filename = './output/SystemTest/ToggleTestFile1.report';
GMAT ReportFile1.Precision = 16;
GMAT ReportFile1.Add = {Sat.A1ModJulian, Sat.EarthMJ2000Eq.X, Sat.EarthMJ2000Eq.Y, Sat.EarthMJ2000Eq.Z, Sat.EarthMJ2000Eq.VX, Sat.EarthMJ2000Eq.VY, Sat.EarthMJ2000Eq.VZ};
GMAT ReportFile1.WriteHeaders = On;
GMAT ReportFile1.LeftJustify = On;
GMAT ReportFile1.ZeroFill = Off;
GMAT ReportFile1.ColumnWidth = 20;
GMAT ReportFile1.SolverIterations = None;

Create ReportFile ReportFile2;
GMAT ReportFile2.Filename = './output/SystemTest/ToggleTestFile2.report';
GMAT ReportFile2.Precision = 16;
GMAT ReportFile2.Add = {Sat.A1ModJulian, Sat.EarthMJ2000Eq.X, Sat.EarthMJ2000Eq.Y, Sat.EarthMJ2000Eq.Z, Sat.EarthMJ2000Eq.VX, Sat.EarthMJ2000Eq.VY, Sat.EarthMJ2000Eq.VZ};
GMAT ReportFile2.WriteHeaders = On;
GMAT ReportFile2.LeftJustify = On;
GMAT ReportFile2.ZeroFill = Off;
GMAT ReportFile2.ColumnWidth = 20;
GMAT ReportFile2.SolverIterations = None;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------


Toggle XYZT1 On;
Toggle XYZT2 Off;
Toggle OpenGLPlot1 On;
Toggle OpenGLPlot2 Off;
Toggle ReportFile1 On;
Toggle ReportFile2 Off;

For I = 1:1:10;
   GMAT XYMag = sqrt( Sat.X^2 + Sat.Y^2 );
   GMAT XZMag = sqrt( Sat.X^2 + Sat.Z^2 );
   GMAT YZMag = sqrt( Sat.Y^2 + Sat.Z^2 );
   GMAT RCalc = sqrt(XYMag^2 + XZMag^2 + YZMag^2);
   Propagate prop(Sat);
EndFor;

Toggle XYZT1 Off;
Toggle XYZT2 On;
Toggle OpenGLPlot1 Off;
Toggle OpenGLPlot2 On;
Toggle ReportFile1 Off;
Toggle ReportFile2 On;

Propagate prop(Sat);
Propagate prop(Sat);