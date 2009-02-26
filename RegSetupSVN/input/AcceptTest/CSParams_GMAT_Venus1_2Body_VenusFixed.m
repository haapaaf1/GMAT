%% $Id: CSParams_GMAT_Venus1_2Body_VenusFixed.m,v 1.4 2007/07/26 19:12:27 edove Exp $

Create Spacecraft VenusSC;
GMAT VenusSC.DateFormat = UTCGregorian
GMAT VenusSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT VenusSC.CoordinateSystem = VenusMJ2000Eq;
GMAT VenusSC.DisplayStateType = Cartesian;
GMAT VenusSC.X = -4832.074380872521000;
GMAT VenusSC.Y = 0.0;
GMAT VenusSC.Z = 4832.074380872517400;
GMAT VenusSC.VX = -0.645356787452373;
GMAT VenusSC.VY = -7.366240195908405;
GMAT VenusSC.VZ = 0.645356787452373;
GMAT VenusSC.Cd = 2.2;
GMAT VenusSC.Cr = 1.2;
GMAT VenusSC.DragArea = 20;
GMAT VenusSC.SRPArea = 20;
GMAT VenusSC.DryMass = 1000;

Create ForceModel Venus2Body;
GMAT Venus2Body.CentralBody = Venus;
GMAT Venus2Body.PointMasses = {Venus};
GMAT Venus2Body.Drag = None;
GMAT Venus2Body.SRP = Off;
GMAT Venus2Body.ErrorControl = RSSStep;

Create Propagator RKV89;
GMAT RKV89.FM = Venus2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem VenusMJ2000Eq;
GMAT VenusMJ2000Eq.Origin = Venus;
GMAT VenusMJ2000Eq.J2000Body = Earth;
GMAT VenusMJ2000Eq.Axes = MJ2000Eq;
GMAT VenusMJ2000Eq.Epoch = 21545;
GMAT VenusMJ2000Eq.UpdateInterval = 60;

Create CoordinateSystem VenusFixed;
GMAT VenusFixed.Origin = Venus;
GMAT VenusFixed.J2000Body = Earth;
GMAT VenusFixed.Axes = BodyFixed;
GMAT VenusFixed.Epoch = 21545.000000397937;
GMAT VenusFixed.UpdateInterval = 60;

Create ReportFile Venus_Report
GMAT Venus_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Venus1_2Body_VenusFixed.report;
GMAT Venus_Report.Precision = 16;
GMAT Venus_Report.WriteHeaders = On;
GMAT Venus_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Venus_Report VenusSC.A1ModJulian VenusSC.VenusFixed.X VenusSC.VenusFixed.Y VenusSC.VenusFixed.Z VenusSC.VenusFixed.VX VenusSC.VenusFixed.VY VenusSC.VenusFixed.VZ VenusSC.VenusFixed.VMAG VenusSC.VenusFixed.RAV VenusSC.VenusFixed.HX VenusSC.VenusFixed.HY VenusSC.VenusFixed.HZ VenusSC.VenusFixed.AOP VenusSC.VenusFixed.DEC VenusSC.VenusFixed.DECV VenusSC.VenusFixed.INC VenusSC.VenusFixed.RA VenusSC.VenusFixed.RAAN;
GMAT Venus_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(VenusSC, {VenusSC.ElapsedSecs = 600});
      Report      Venus_Report VenusSC.A1ModJulian VenusSC.VenusFixed.X VenusSC.VenusFixed.Y VenusSC.VenusFixed.Z VenusSC.VenusFixed.VX VenusSC.VenusFixed.VY VenusSC.VenusFixed.VZ VenusSC.VenusFixed.VMAG VenusSC.VenusFixed.RAV VenusSC.VenusFixed.HX VenusSC.VenusFixed.HY VenusSC.VenusFixed.HZ VenusSC.VenusFixed.AOP VenusSC.VenusFixed.DEC VenusSC.VenusFixed.DECV VenusSC.VenusFixed.INC VenusSC.VenusFixed.RA VenusSC.VenusFixed.RAAN;
EndFor ;
