


%% $Id: GMAT_GPS_Earth_0_0_SRP.m,v 1.4 2007/07/26 19:12:28 edove Exp $

Create Spacecraft GPS;
 GMAT GPS.J2000BodyName = Earth;
 GMAT GPS.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT GPS.DisplayStateType = Cartesian;
 GMAT GPS.CoordinateSystem = EarthMJ2000Eq;
 GMAT GPS.X = 5525.33668;
 GMAT GPS.Y = -15871.18494;
 GMAT GPS.Z = -20998.992446;
 GMAT GPS.VX = 2.750341;
 GMAT GPS.VY = 2.434198;
 GMAT GPS.VZ = -1.068884;
 GMAT GPS.Cd = 2.2;
 GMAT GPS.Cr = 1.2;
 GMAT GPS.DragArea = 20;
 GMAT GPS.SRPArea = 20;
 GMAT GPS.DryMass = 1000;
 GMAT GPS.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.PrimaryBodies = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = On;
GMAT Earth2Body.SRP.Flux_Pressure = 4.53443218374393e-006;
GMAT Earth2Body.Gravity.Earth.Model = JGM2;
GMAT Earth2Body.Gravity.Earth.Degree = 0;
GMAT Earth2Body.Gravity.Earth.Order = 0;
GMAT Earth2Body.PointMasses   = {};

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 60;
GMAT RKV89.MaxStep = 60;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile GPS_Report
GMAT GPS_Report.Filename = ./output/AcceptTest/GMAT_GPS_Earth_0_0_SRP.report;
GMAT GPS_Report.Precision = 16;
GMAT GPS_Report.WriteHeaders = Off;
GMAT GPS_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report GPS_Report GPS.A1ModJulian GPS.X GPS.Y GPS.Z GPS.VX GPS.VY GPS.VZ;
For OutputStepSize = 1:1440;
	Propagate   RKV89(GPS, {GPS.ElapsedSecs = 120});
	Report      GPS_Report GPS.A1ModJulian GPS.X GPS.Y GPS.Z GPS.VX GPS.VY GPS.VZ;
EndFor ;
