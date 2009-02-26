%% $Id: GMAT_Pluto1_Pluto_0_0_0.m,v 1.4 2007/07/26 19:12:29 edove Exp $

Create Spacecraft PlutoSC;
GMAT PlutoSC.DateFormat = UTCGregorian
GMAT PlutoSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT PlutoSC.CoordinateSystem = PlutoMJ2000Eq;
GMAT PlutoSC.DisplayStateType = Cartesian;
GMAT PlutoSC.X = -1067.516740143530600;
GMAT PlutoSC.Y = 0.0;
GMAT PlutoSC.Z = 1067.516740143529700;
GMAT PlutoSC.VX = -0.075474392886505;
GMAT PlutoSC.VY = -0.861480838897026;
GMAT PlutoSC.VZ = 0.075474392886505;
GMAT PlutoSC.Cd = 2.2;
GMAT PlutoSC.Cr = 1.2;
GMAT PlutoSC.DragArea = 20;
GMAT PlutoSC.SRPArea = 20;
GMAT PlutoSC.DryMass = 1000;

Create ForceModel Pluto2Body;
GMAT Pluto2Body.CentralBody = Pluto;
GMAT Pluto2Body.PointMasses = {Pluto};
GMAT Pluto2Body.Drag = None;
GMAT Pluto2Body.SRP = Off;
GMAT Pluto2Body.ErrorControl = RSSStep;

Create Propagator RKV89;
GMAT RKV89.FM = Pluto2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem PlutoMJ2000Eq;
GMAT PlutoMJ2000Eq.Origin = Pluto;
GMAT PlutoMJ2000Eq.J2000Body = Earth;
GMAT PlutoMJ2000Eq.Axes = MJ2000Eq;
GMAT PlutoMJ2000Eq.Epoch = 21545;
GMAT PlutoMJ2000Eq.UpdateInterval = 60;

Create CoordinateSystem EarthMJ2000Eq;
GMAT EarthMJ2000Eq.Origin = Earth;
GMAT EarthMJ2000Eq.J2000Body = Earth;
GMAT EarthMJ2000Eq.Axes = MJ2000Eq;
GMAT EarthMJ2000Eq.Epoch = 21545;
GMAT EarthMJ2000Eq.UpdateInterval = 60;


Create CoordinateSystem EarthMJ2000Ec;
GMAT EarthMJ2000Ec.Origin = Earth;
GMAT EarthMJ2000Ec.J2000Body = Earth;
GMAT EarthMJ2000Ec.Axes = MJ2000Ec;
GMAT EarthMJ2000Ec.Epoch = 21545;
GMAT EarthMJ2000Ec.UpdateInterval = 60;


Create CoordinateSystem EarthFixed;
GMAT EarthFixed.Origin = Earth;
GMAT EarthFixed.J2000Body = Earth;
GMAT EarthFixed.Axes = BodyFixed;
GMAT EarthFixed.Epoch = 21545;
GMAT EarthFixed.UpdateInterval = 60;

Create ReportFile Pluto_Report
GMAT Pluto_Report.Filename = ./output/AcceptTest/GMAT_Pluto1_Pluto_0_0_0.report;
GMAT Pluto_Report.Precision = 16;
GMAT Pluto_Report.WriteHeaders = Off;
GMAT Pluto_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Pluto_Report PlutoSC.A1ModJulian PlutoSC.PlutoMJ2000Eq.X PlutoSC.PlutoMJ2000Eq.Y PlutoSC.PlutoMJ2000Eq.Z PlutoSC.PlutoMJ2000Eq.VX PlutoSC.PlutoMJ2000Eq.VY PlutoSC.PlutoMJ2000Eq.VZ;
For OutputStepSize = 1:864;
	Propagate   RKV89(PlutoSC, {PlutoSC.ElapsedSecs = 300});
	Report      Pluto_Report PlutoSC.A1ModJulian PlutoSC.PlutoMJ2000Eq.X PlutoSC.PlutoMJ2000Eq.Y PlutoSC.PlutoMJ2000Eq.Z PlutoSC.PlutoMJ2000Eq.VX PlutoSC.PlutoMJ2000Eq.VY PlutoSC.PlutoMJ2000Eq.VZ;
EndFor ;
