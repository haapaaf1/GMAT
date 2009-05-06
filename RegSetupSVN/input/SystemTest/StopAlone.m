%$Id: StopAlone.m,v 1.8 2008/04/18 13:42:10 lojun Exp $ 

%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft Sat;
GMAT Sat.DateFormat = TAIModJulian;
GMAT Sat.Epoch = 21545;
GMAT Sat.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat.DisplayStateType = Cartesian;
GMAT Sat.X = 7100;
GMAT Sat.Y = 0;
GMAT Sat.Z = 1300;
GMAT Sat.VX = 0;
GMAT Sat.VY = 7.35;
GMAT Sat.VZ = 1;
GMAT Sat.DryMass = 850;
GMAT Sat.Cd = 2.2;
GMAT Sat.Cr = 1.8;
GMAT Sat.DragArea = 15;
GMAT Sat.SRPArea = 1;


%----------------------------------------
%---------- ForceModels
%----------------------------------------

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Earth;
GMAT DefaultProp_ForceModel.PrimaryBodies = {Earth};
GMAT DefaultProp_ForceModel.Drag = None;
GMAT DefaultProp_ForceModel.SRP = Off;
GMAT DefaultProp_ForceModel.ErrorControl = RSSStep;
GMAT DefaultProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT DefaultProp_ForceModel.Gravity.Earth.Order = 4;
GMAT DefaultProp_ForceModel.Gravity.Earth.PotentialFile = ./files/gravity/earth/JGM2.cof;


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 60;
GMAT DefaultProp.Accuracy = 9.999999999999999e-012;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;


%----------------------------------------
%---------- Burns
%----------------------------------------

Create ImpulsiveBurn Burn1;
GMAT Burn1.Origin = Earth;
GMAT Burn1.Axes = VNB;
GMAT Burn1.VectorFormat = Cartesian;
GMAT Burn1.Element1 = 0;
GMAT Burn1.Element2 = 0;
GMAT Burn1.Element3 = 0;


%----------------------------------------
%---------- Parameters
%----------------------------------------

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
%---------- Solvers
%----------------------------------------

Create DifferentialCorrector DefaultDC;
GMAT DefaultDC.ShowProgress = true;
GMAT DefaultDC.ReportStyle = Normal;
GMAT DefaultDC.TargeterTextFile = DifferentialCorrectorDefaultDC.data;
GMAT DefaultDC.MaximumIterations = 25;
GMAT DefaultDC.UseCentralDifferences = false;


%----------------------------------------
%---------- Subscribers
%----------------------------------------

Create XYPlot XYPlot1;
GMAT XYPlot1.IndVar = Sat.A1ModJulian;
GMAT XYPlot1.Add = {Sat.EarthMJ2000Eq.X};
GMAT XYPlot1.Grid = On;
GMAT XYPlot1.SolverIterations = None;
GMAT XYPlot1.ShowPlot = true;

Create ReportFile stopReport;
GMAT stopReport.Filename = ./output/SystemTest/StopAlone.report;
GMAT stopReport.Precision = 15;
GMAT stopReport.WriteHeaders = On;
GMAT stopReport.LeftJustify = On;
GMAT stopReport.ZeroFill = Off;
GMAT stopReport.ColumnWidth = 20;
GMAT stopReport.SolverIterations = None;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------


Propagate DefaultProp(Sat) {Sat.ElapsedSecs = 8640.0};
Report stopReport Sat.ElapsedSecs Sat.SMA Sat.ECC Sat.INC Sat.RAAN ...
    Sat.AOP Sat.TA Sat.BetaAngle;
Stop
If Sat.Earth.SMA > 6378.14
   Propagate DefaultProp(Sat) {Sat.ElapsedSecs = 8640.0};
   Report stopReport Sat.ElapsedSecs Sat.SMA Sat.ECC Sat.INC Sat.RAAN ...
       Sat.AOP Sat.TA Sat.BetaAngle;
EndIf;
For I = 1:1:10;
   Propagate DefaultProp(Sat) {Sat.ElapsedSecs = 8640.0};
   Report stopReport Sat.ElapsedSecs Sat.SMA Sat.ECC Sat.INC Sat.RAAN ...
       Sat.AOP Sat.TA Sat.BetaAngle;
EndFor;
While Sat.ElapsedDays < 2
   Propagate DefaultProp(Sat) {Sat.ElapsedSecs = 8640.0};
   Report stopReport Sat.ElapsedSecs Sat.SMA Sat.ECC Sat.INC Sat.RAAN ...
       Sat.AOP Sat.TA Sat.BetaAngle;
EndWhile;
Target DefaultDC;
   Vary DefaultDC(Burn1.Element1 = 0.5, {Perturbation = 0.0001, MaxStep = 0.2, Lower = 0, Upper = 3.14159, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Propagate DefaultProp(Sat) {Sat.Earth.Apoapsis};
   Report stopReport Sat.ElapsedSecs Sat.SMA Sat.ECC Sat.INC Sat.RAAN ...
       Sat.AOP Sat.TA Sat.BetaAngle;
   Achieve DefaultDC(Sat.Earth.RMAG = 42165.0, {Tolerance = 0.1});
EndTarget;  % For targeter DefaultDC  % For targeter DefaultDC
Propagate DefaultProp(Sat) {Sat.ElapsedSecs = 8640.0};

