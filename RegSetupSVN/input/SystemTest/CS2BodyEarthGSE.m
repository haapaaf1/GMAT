%% $Id: CS2BodyEarthGSE.m,v 1.3 2008/10/14 17:58:29 edove Exp $

Create Spacecraft ISS;
 GMAT ISS.J2000BodyName = Earth;
 GMAT ISS.Epoch.UTCGregorian = '01 Jun 2004 12:00:00.000';
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

Create ForceModel Earth2Body;
GMAT Earth2Body.PrimaryBodies = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.Gravity.Earth.Model = 'JGM2.cof';
GMAT Earth2Body.Gravity.Earth.Degree = 0;
GMAT Earth2Body.Gravity.Earth.Order = 0;

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
GMAT SolarSystem.UseTTForEphemeris = true;

Create CoordinateSystem EarthGSE;
GMAT EarthGSE.Origin = Earth;
GMAT EarthGSE.Axes = GSE;
GMAT EarthGSE.UpdateInterval = 0;

Create ReportFile ISS_Report
GMAT ISS_Report.Filename = './output/SystemTest/CS2BodyEarthGSE.report';
GMAT ISS_Report.Precision = 16;
GMAT ISS_Report.WriteHeaders = On;
GMAT ISS_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      ISS_Report ISS.A1ModJulian ISS.EarthGSE.X ISS.EarthGSE.Y ISS.EarthGSE.Z ISS.EarthGSE.VX ISS.EarthGSE.VY ISS.EarthGSE.VZ ISS.EarthGSE.VMAG ISS.EarthGSE.RAV ISS.EarthGSE.HX ISS.EarthGSE.HY ISS.EarthGSE.HZ ISS.EarthGSE.AOP ISS.EarthGSE.DEC ISS.EarthGSE.DECV ISS.EarthGSE.INC ISS.EarthGSE.RA ISS.EarthGSE.RAAN;
GMAT ISS_Report.WriteHeaders = Off;
For OutputStepSize = 1:72;
	Propagate   RKV89(ISS, {ISS.ElapsedSecs = 600});
      Report      ISS_Report ISS.A1ModJulian ISS.EarthGSE.X ISS.EarthGSE.Y ISS.EarthGSE.Z ISS.EarthGSE.VX ISS.EarthGSE.VY ISS.EarthGSE.VZ ISS.EarthGSE.VMAG ISS.EarthGSE.RAV ISS.EarthGSE.HX ISS.EarthGSE.HY ISS.EarthGSE.HZ ISS.EarthGSE.AOP ISS.EarthGSE.DEC ISS.EarthGSE.DECV ISS.EarthGSE.INC ISS.EarthGSE.RA ISS.EarthGSE.RAAN;
EndFor ;
