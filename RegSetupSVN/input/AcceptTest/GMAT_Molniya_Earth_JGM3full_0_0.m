


%% $Id: GMAT_Molniya_Earth_JGM3full_0_0.m,v 1.1 2007/09/20 21:33:02 edove Exp $

Create Spacecraft Molniya;
 GMAT Molniya.J2000BodyName = Earth;
 GMAT Molniya.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT Molniya.DisplayStateType = Cartesian;
 GMAT Molniya.CoordinateSystem = EarthMJ2000Eq;
 GMAT Molniya.X = -1529.894287;
 GMAT Molniya.Y = -2672.877357;
 GMAT Molniya.Z = -6150.115340;
 GMAT Molniya.VX = 8.7175180;
 GMAT Molniya.VY = -4.989709;
 GMAT Molniya.VZ = 0.0;
 GMAT Molniya.Cd = 2.2;
 GMAT Molniya.Cr = 1.2;
 GMAT Molniya.DragArea = 20;
 GMAT Molniya.SRPArea = 20;
 GMAT Molniya.DryMass = 1000;
 GMAT Molniya.TotalMass = 1000;

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

Create ReportFile Molniya_Report
GMAT Molniya_Report.Filename = ./output/AcceptTest/GMAT_Molniya_Earth_JGM3full_0_0.report;
GMAT Molniya_Report.Precision = 16;
GMAT Molniya_Report.WriteHeaders = Off;
GMAT Molniya_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Molniya_Report Molniya.A1ModJulian Molniya.X Molniya.Y Molniya.Z Molniya.VX Molniya.VY Molniya.VZ;
For OutputStepSize = 1:864;
	Propagate   RKV89(Molniya, {Molniya.ElapsedSecs = 300});
	Report      Molniya_Report Molniya.A1ModJulian Molniya.X Molniya.Y Molniya.Z Molniya.VX Molniya.VY Molniya.VZ;
EndFor ;