


%% $Id: GMAT_GEO_Earth_JGM3full_0_0.m,v 1.1 2007/09/20 21:33:02 edove Exp $

Create Spacecraft GEO;
 GMAT GEO.J2000BodyName = Earth;
 GMAT GEO.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT GEO.DisplayStateType = Cartesian;
 GMAT GEO.CoordinateSystem = EarthMJ2000Eq;
 GMAT GEO.X = 36607.3582560;
 GMAT GEO.Y = -20921.723703;
 GMAT GEO.Z = 0.0;
 GMAT GEO.VX = 1.52563600;
 GMAT GEO.VY = 2.66945100;
 GMAT GEO.VZ = 0.0;
 GMAT GEO.Cd = 2.2;
 GMAT GEO.Cr = 1.2;
 GMAT GEO.DragArea = 20;
 GMAT GEO.SRPArea = 20;
 GMAT GEO.DryMass = 1000;
 GMAT GEO.TotalMass = 1000;

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
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 60;
GMAT RKV89.MaxStep = 60;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile GEO_Report
GMAT GEO_Report.Filename = ./output/AcceptTest/GMAT_GEO_Earth_JGM3full_0_0.report;
GMAT GEO_Report.Precision = 16;
GMAT GEO_Report.WriteHeaders = Off;
GMAT GEO_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report GEO_Report GEO.A1ModJulian GEO.X GEO.Y GEO.Z GEO.VX GEO.VY GEO.VZ;
For OutputStepSize = 1:1008;
	Propagate   RKV89(GEO, {GEO.ElapsedSecs = 600});
	Report      GEO_Report GEO.A1ModJulian GEO.X GEO.Y GEO.Z GEO.VX GEO.VY GEO.VZ;
EndFor ;
