%$Id: MatlabObjectPassingCheck.m,v 1.10 2008/04/15 21:19:09 edove Exp $

% Global settings
GMAT SolarSystem.Ephemeris = {SLP}

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

Create ForceModel Prop_ForceModel;
GMAT Prop_ForceModel.CentralBody = Earth;
GMAT Prop_ForceModel.PrimaryBodies = {Earth};
GMAT Prop_ForceModel.Drag = Exponential;
GMAT Prop_ForceModel.SRP = Off;
GMAT Prop_ForceModel.ErrorControl = RSSStep;
GMAT Prop_ForceModel.Gravity.Earth.Degree = 4;
GMAT Prop_ForceModel.Gravity.Earth.Order = 4;
GMAT Prop_ForceModel.Gravity.Earth.PotentialFile = ./files/gravity/earth/JGM2.cof;


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator Prop;
GMAT Prop.FM = Prop_ForceModel;
GMAT Prop.Type = RungeKutta89;
GMAT Prop.InitialStepSize = 60;
GMAT Prop.Accuracy = 1e-011;
GMAT Prop.MinStep = 0.001;
GMAT Prop.MaxStep = 2700;
GMAT Prop.MaxStepAttempts = 50;


%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable RdotV;
GMAT RdotV = -999999.0;

Create Variable Energy;
GMAT Energy = 0.0;

Create Variable I;

Create Array state[1, 6];
GMAT state(1, 1) = 0;
GMAT state(1, 2) = 0;
GMAT state(1, 3) = 0;
GMAT state(1, 4) = 0;
GMAT state(1, 5) = 0;
GMAT state(1, 6) = 0;



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

Create ReportFile RVReport;
GMAT RVReport.Filename = ./output/SystemTest/MatlabObjectPassingCheck.report;
GMAT RVReport.Precision = 16;
GMAT RVReport.WriteHeaders = On;
GMAT RVReport.LeftJustify = On;
GMAT RVReport.ZeroFill = Off;
GMAT RVReport.ColumnWidth = 20;
GMAT RVReport.SolverIterations = None;


%----------------------------------------
%---------- Functions
%----------------------------------------

Create MatlabFunction CalculateApsidesFromSat;
GMAT CalculateApsidesFromSat.FunctionPath = ./input/SystemTest/;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------


For I = 1:1:1000;
   If Sat.TA > 170 & Sat.TA < 180
      Propagate Prop(Sat) {Sat.Apoapsis};
   Else
      If Sat.TA > 350
         Propagate Prop(Sat) {Sat.Periapsis};
      Else
         Propagate Prop(Sat);
      EndIf;
   EndIf;
   GMAT state(1, 1) = Sat.X;
   GMAT state(1, 2) = Sat.Y;
   GMAT state(1, 3) = Sat.Z;
   GMAT state(1, 4) = Sat.VX;
   GMAT state(1, 5) = Sat.VY;
   GMAT state(1, 6) = Sat.VZ;
   GMAT [RdotV, Energy] = CalculateApsidesFromSat(Sat);
   Report RVReport Sat.A1ModJulian Sat.Earth.Apoapsis Sat.Earth.Periapsis RdotV Sat.TA Energy Sat.Energy
EndFor;

