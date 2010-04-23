


%% $Id: ESL_JGM2_MSISE90_SRP.m,v 1.3 2007/08/10 21:15:03 edove Exp $

Create Spacecraft GEO;
 GMAT GEO.DateFormat = UTCGregorian;
 GMAT GEO.Epoch = '01 Jun 2004 12:00:00.000';
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

Create ForceModel EarthJGM2;
GMAT EarthJGM2.PrimaryBodies = {Earth};
GMAT EarthJGM2.Drag = MSISE90;
GMAT EarthJGM2.Drag.F107 = 150;
GMAT EarthJGM2.Drag.F107A = 150;
GMAT EarthJGM2.Drag.MagneticIndex = 3;
GMAT EarthJGM2.SRP = On;
GMAT EarthJGM2.SRP.Flux_Pressure = 4.53443218374393e-006;
GMAT EarthJGM2.Gravity.Earth.Model = './files/gravity/earth/JGM2.cof';
GMAT EarthJGM2.Gravity.Earth.Degree = 20;
GMAT EarthJGM2.Gravity.Earth.Order = 20;
GMAT EarthJGM2.PointMasses   = {Sun, Luna};

Create Propagator RKV89;
GMAT RKV89.FM = EarthJGM2;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 60;
GMAT RKV89.MaxStep = 60;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile GEO_Report
GMAT GEO_Report.Filename = './output/SystemTest/ESL_JGM2_MSISE90_SRP.report';
GMAT GEO_Report.Precision = 16;
GMAT GEO_Report.WriteHeaders = Off;
GMAT GEO_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report GEO_Report GEO.A1ModJulian GEO.X GEO.Y GEO.Z GEO.VX GEO.VY GEO.VZ;
For OutputStepSize = 1:72;
	Propagate   RKV89(GEO, {GEO.ElapsedSecs = 600});
	Report      GEO_Report GEO.A1ModJulian GEO.X GEO.Y GEO.Z GEO.VX GEO.VY GEO.VZ;
EndFor ;