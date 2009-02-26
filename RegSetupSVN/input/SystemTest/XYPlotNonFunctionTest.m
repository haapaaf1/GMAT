%$Id: XYPlotNonFunctionTest.m,v 1.8 2008/09/05 20:46:56 edove Exp $
% 
% Script used to exercise the XY plot for data that is single 
% valued (i.e. a "function") and data that in multivalued (a
% "nonfunction"; hence the script name).


%----------------------------------------
%---------- Spacecraft
%----------------------------------------

% The spacecraft is in a somewhat eccentric orbit to make the plots more interesting.
Create Spacecraft Sat;
GMAT Sat.DateFormat = TAIModJulian;
GMAT Sat.Epoch = 21545;
GMAT Sat.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat.DisplayStateType = Keplerian;
GMAT Sat.SMA = 11999.99999999999;
GMAT Sat.ECC = 0.2500000000000001;
GMAT Sat.INC = 53.00000000000001;
GMAT Sat.RAAN = 59.99999999999999;
GMAT Sat.AOP = 359.9999991462263;
GMAT Sat.TA = 125.0000000000001;
GMAT Sat.DryMass = 850;
GMAT Sat.Cd = 2.2;
GMAT Sat.Cr = 1.8;
GMAT Sat.DragArea = 15;
GMAT Sat.SRPArea = 1;


%----------------------------------------
%---------- ForceModels
%----------------------------------------

% Just using defaults for the propagator and forces
Create ForceModel prop_ForceModel;
GMAT prop_ForceModel.CentralBody = Earth;
GMAT prop_ForceModel.PrimaryBodies = {Earth};
GMAT prop_ForceModel.Drag = None;
GMAT prop_ForceModel.SRP = Off;
GMAT prop_ForceModel.ErrorControl = RSSStep;
GMAT prop_ForceModel.Gravity.Earth.Degree = 4;
GMAT prop_ForceModel.Gravity.Earth.Order = 4;
GMAT prop_ForceModel.Gravity.Earth.PotentialFile = ./files/gravity/earth/JGM2.cof;


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
%---------- Subscribers
%----------------------------------------

% 4 plots
Create XYPlot CartesianPosV;
GMAT CartesianPosV.IndVar = Sat.EarthMJ2000Eq.VX;
GMAT CartesianPosV.Add = {Sat.EarthMJ2000Eq.X, Sat.EarthMJ2000Eq.Y, Sat.EarthMJ2000Eq.Z};
GMAT CartesianPosV.Grid = On;
GMAT CartesianPosV.SolverIterations = None;
GMAT CartesianPosV.ShowPlot = true;

Create XYPlot KeplerianRSma;
GMAT KeplerianRSma.IndVar = Sat.Earth.SMA;
GMAT KeplerianRSma.Add = {Sat.Earth.RMAG, Sat.Earth.Altitude, XYMag, XZMag, YZMag, RCalc};
GMAT KeplerianRSma.Grid = On;
GMAT KeplerianRSma.SolverIterations = None;
GMAT KeplerianRSma.ShowPlot = true;

Create XYPlot XYZT;
GMAT XYZT.IndVar = Sat.A1ModJulian;
GMAT XYZT.Add = {Sat.EarthMJ2000Eq.X, Sat.EarthMJ2000Eq.Y, Sat.EarthMJ2000Eq.Z};
GMAT XYZT.Grid = On;
GMAT XYZT.SolverIterations = None;
GMAT XYZT.ShowPlot = true;

Create XYPlot VT;
GMAT VT.IndVar = Sat.ElapsedSecs;
GMAT VT.Add = {Sat.EarthMJ2000Eq.VX, Sat.EarthMJ2000Eq.VY, Sat.EarthMJ2000Eq.VZ};
GMAT VT.Grid = On;
GMAT VT.SolverIterations = None;
GMAT VT.ShowPlot = true;

Create ReportFile rf;
GMAT rf.Filename = ./output/SystemTest/XYPlotNonFunctionTest.report;
GMAT rf.Precision = 16;
GMAT rf.Add = {Sat.UTCModJulian, RCalc, Sat.X, Sat.Y, Sat.Z};

%----------------------------------------
%---------- Mission Sequence
%----------------------------------------


For I = 1:1:10;
   GMAT XYMag = sqrt( Sat.X^2 + Sat.Y^2 );
   GMAT XZMag = sqrt( Sat.X^2 + Sat.Z^2 );
   GMAT YZMag = sqrt( Sat.Y^2 + Sat.Z^2 );
   GMAT RCalc = sqrt(XYMag^2 + XZMag^2 + YZMag^2);
   Propagate prop(Sat);
EndFor;

% This final propagate should update only the sat parameters; 
% we should see straight lines for the variables
Propagate prop(Sat) {Sat.ElapsedSecs = 12000};
