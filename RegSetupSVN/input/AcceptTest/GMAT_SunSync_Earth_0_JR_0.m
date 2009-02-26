


%% $Id: GMAT_SunSync_Earth_0_JR_0.m,v 1.4 2007/07/26 19:12:29 edove Exp $

Create Spacecraft SunSync;
 GMAT SunSync.J2000BodyName = Earth;
 GMAT SunSync.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT SunSync.DisplayStateType = Cartesian;
 GMAT SunSync.CoordinateSystem = EarthMJ2000Eq;
 GMAT SunSync.X = -2290.301063;
 GMAT SunSync.Y = -6379.471940;
 GMAT SunSync.Z = 0;
 GMAT SunSync.VX = -0.883923;
 GMAT SunSync.VY = 0.317338;
 GMAT SunSync.VZ = 7.610832;
 GMAT SunSync.Cd = 2.2;
 GMAT SunSync.Cr = 1.2;
 GMAT SunSync.DragArea = 20;
 GMAT SunSync.SRPArea = 20;
 GMAT SunSync.DryMass = 1000;
 GMAT SunSync.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.PrimaryBodies = {Earth};
GMAT Earth2Body.Drag = JacchiaRoberts;
GMAT Earth2Body.Drag.F107 = 150;
GMAT Earth2Body.Drag.F107A = 150;
GMAT Earth2Body.Drag.MagneticIndex = 3;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.Gravity.Earth.Model = JGM2;
GMAT Earth2Body.Gravity.Earth.Degree = 0;
GMAT Earth2Body.Gravity.Earth.Order = 0;
GMAT Earth2Body.PointMasses   = {};

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile SunSync_Report
GMAT SunSync_Report.Filename = ./output/AcceptTest/GMAT_SunSync_Earth_0_JR_0.report;
GMAT SunSync_Report.Precision = 16;
GMAT SunSync_Report.WriteHeaders = Off;
GMAT SunSync_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report SunSync_Report SunSync.A1ModJulian SunSync.X SunSync.Y SunSync.Z SunSync.VX SunSync.VY SunSync.VZ;
For OutputStepSize = 1:1440;
	Propagate   RKV89(SunSync, {SunSync.ElapsedSecs = 60});
	Report      SunSync_Report SunSync.A1ModJulian SunSync.X SunSync.Y SunSync.Z SunSync.VX SunSync.VY SunSync.VZ;
EndFor ;
