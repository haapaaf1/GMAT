


%% $Id: GMAT_SunSync_EarthSunLuna_JGM2_MSISE90_SRP.m,v 1.5 2007/08/10 21:15:03 edove Exp $

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

Create ForceModel EarthJGM2;
GMAT EarthJGM2.PrimaryBodies = {Earth};
GMAT EarthJGM2.Drag = MSISE90;
GMAT EarthJGM2.Drag.F107 = 150;
GMAT EarthJGM2.Drag.F107A = 150;
GMAT EarthJGM2.Drag.MagneticIndex = 3;
GMAT EarthJGM2.SRP = On;
GMAT EarthJGM2.SRP.Flux_Pressure = 4.53443218374393e-006;
GMAT EarthJGM2.Gravity.Earth.Model = ./files/gravity/earth/JGM2.cof;
GMAT EarthJGM2.Gravity.Earth.Degree = 20;
GMAT EarthJGM2.Gravity.Earth.Order = 20;
GMAT EarthJGM2.PointMasses   = {Sun, Luna};

Create Propagator RKV89;
GMAT RKV89.FM = EarthJGM2;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile SunSync_Report
GMAT SunSync_Report.Filename = ./output/AcceptTest/GMAT_SunSync_EarthSunLuna_JGM2_MSISE90_SRP.report;
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
