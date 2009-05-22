%% $Id: CS2BodyEarthFixed.m,v 1.2 2007/07/26 19:13:11 edove Exp $

Create Spacecraft ISS;
 GMAT ISS.J2000BodyName = Earth;
 GMAT ISS.Epoch = '01 Jun 2004 12:00:00.000';
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
GMAT Earth2Body.PointMasses   = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 1e-7;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile ISS_Report
GMAT ISS_Report.Filename = './output/SystemTest/CS2BodyEarthFixed.report';
GMAT ISS_Report.Precision = 16;
GMAT ISS_Report.WriteHeaders = On;
GMAT ISS_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      ISS_Report ISS.A1ModJulian ISS.EarthFixed.X ISS.EarthFixed.Y ISS.EarthFixed.Z ISS.EarthFixed.VX ISS.EarthFixed.VY ISS.EarthFixed.VZ ISS.EarthFixed.VMAG ISS.EarthFixed.RAV ISS.EarthFixed.HX ISS.EarthFixed.HY ISS.EarthFixed.HZ ISS.EarthFixed.AOP ISS.EarthFixed.DEC ISS.EarthFixed.DECV ISS.EarthFixed.INC ISS.EarthFixed.RA ISS.EarthFixed.RAAN;
GMAT ISS_Report.WriteHeaders = Off;
For OutputStepSize = 1:72;
	Propagate   RKV89(ISS, {ISS.ElapsedSecs = 600});
      Report      ISS_Report ISS.A1ModJulian ISS.EarthFixed.X ISS.EarthFixed.Y ISS.EarthFixed.Z ISS.EarthFixed.VX ISS.EarthFixed.VY ISS.EarthFixed.VZ ISS.EarthFixed.VMAG ISS.EarthFixed.RAV ISS.EarthFixed.HX ISS.EarthFixed.HY ISS.EarthFixed.HZ ISS.EarthFixed.AOP ISS.EarthFixed.DEC ISS.EarthFixed.DECV ISS.EarthFixed.INC ISS.EarthFixed.RA ISS.EarthFixed.RAAN;
EndFor ;
