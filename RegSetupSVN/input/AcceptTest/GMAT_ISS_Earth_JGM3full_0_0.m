


%% $Id: GMAT_ISS_Earth_JGM3full_0_0.m,v 1.1 2007/09/20 21:33:02 edove Exp $

Create Spacecraft ISS;
 GMAT ISS.J2000BodyName = Earth;
 GMAT ISS.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT ISS.DisplayStateType = Cartesian;
 GMAT ISS.CoordinateSystem = EarthMJ2000Eq;
 GMAT ISS.X = -4453.7835859999996;
 GMAT ISS.Y = -5038.2037559999999;
 GMAT ISS.Z = -426.384456;
 GMAT ISS.VX = 3.8318880000000002;
 GMAT ISS.VY = -2.8872209999999998;
 GMAT ISS.VZ = -6.0182320000000002;
 GMAT ISS.Cd = 2.2;
 GMAT ISS.Cr = 1.2;
 GMAT ISS.DragArea = 20;
 GMAT ISS.SRPArea = 20;
 GMAT ISS.DryMass = 1000;
 GMAT ISS.TotalMass = 1000;

Create ForceModel EarthJGM3;
GMAT EarthJGM3.PrimaryBodies = {Earth};
GMAT EarthJGM3.Drag = None;
GMAT EarthJGM3.SRP = Off;
GMAT EarthJGM3.Gravity.Earth.Model = ./files/gravity/earth/JGM3.cof;
GMAT EarthJGM3.Gravity.Earth.Degree = 70;
GMAT EarthJGM3.Gravity.Earth.Order = 70;
GMAT EarthJGM3.PointMasses   = {};

Create Propagator RKV89;
GMAT RKV89.FM = EarthJGM3;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile ISS_Report
GMAT ISS_Report.Filename = ./output/AcceptTest/GMAT_ISS_Earth_JGM3full_0_0.report;
GMAT ISS_Report.Precision = 16;
GMAT ISS_Report.WriteHeaders = Off;
GMAT ISS_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report ISS_Report ISS.A1ModJulian ISS.X ISS.Y ISS.Z ISS.VX ISS.VY ISS.VZ;
For OutputStepSize = 1:1440;
	Propagate   RKV89(ISS, {ISS.ElapsedSecs = 60});
	Report      ISS_Report ISS.A1ModJulian ISS.X ISS.Y ISS.Z ISS.VX ISS.VY ISS.VZ;
EndFor ;
