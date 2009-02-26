%% $Id: CbSaturn2Body.m,v 1.2 2007/07/26 19:13:11 edove Exp $

Create Spacecraft SaturnSC;
GMAT SaturnSC.DateFormat = UTCGregorian
GMAT SaturnSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT SaturnSC.CoordinateSystem = SaturnMJ2000Eq;
GMAT SaturnSC.DisplayStateType = Cartesian;
GMAT SaturnSC.X = -47577.347750129338000;
GMAT SaturnSC.Y = 0.0;
GMAT SaturnSC.Z = 47577.347750129360000;
GMAT SaturnSC.VX = -2.222652848522210;
GMAT SaturnSC.VY = -25.369834288049386;
GMAT SaturnSC.VZ = 2.222652848522210;
GMAT SaturnSC.Cd = 2.2;
GMAT SaturnSC.Cr = 1.2;
GMAT SaturnSC.DragArea = 20;
GMAT SaturnSC.SRPArea = 20;
GMAT SaturnSC.DryMass = 1000;

Create ForceModel Saturn2Body;
GMAT Saturn2Body.CentralBody = Saturn;
GMAT Saturn2Body.PointMasses = {Saturn};
GMAT Saturn2Body.Drag = None;
GMAT Saturn2Body.SRP = Off;
GMAT Saturn2Body.ErrorControl = RSSStep;


Create Propagator RKV89;
GMAT RKV89.FM = Saturn2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem SaturnMJ2000Eq;
GMAT SaturnMJ2000Eq.Origin = Saturn;
GMAT SaturnMJ2000Eq.J2000Body = Earth;
GMAT SaturnMJ2000Eq.Axes = MJ2000Eq;
GMAT SaturnMJ2000Eq.Epoch = 21545;
GMAT SaturnMJ2000Eq.UpdateInterval = 60;

Create ReportFile Saturn_Report
GMAT Saturn_Report.Filename = ./output/SystemTest/CbSaturn2Body.report;
GMAT Saturn_Report.Precision = 16;
GMAT Saturn_Report.WriteHeaders = On;
GMAT Saturn_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Saturn_Report SaturnSC.A1ModJulian SaturnSC.Saturn.Altitude SaturnSC.Saturn.BetaAngle SaturnSC.Saturn.C3Energy SaturnSC.Saturn.ECC SaturnSC.Saturn.Latitude SaturnSC.Saturn.Longitude SaturnSC.Saturn.HMAG SaturnSC.Saturn.MA SaturnSC.Saturn.MM SaturnSC.Saturn.OrbitPeriod SaturnSC.Saturn.RadApo SaturnSC.Saturn.RadPer SaturnSC.Saturn.RMAG SaturnSC.Saturn.SMA SaturnSC.Saturn.TA SaturnSC.Saturn.SemilatusRectum SaturnSC.Saturn.VelApoapsis SaturnSC.Saturn.VelPeriapsis SaturnSC.Saturn.MHA SaturnSC.Saturn.LST;
GMAT Saturn_Report.WriteHeaders = Off;
For OutputStepSize = 1:72;
	Propagate   RKV89(SaturnSC, {SaturnSC.ElapsedSecs = 600});
      Report      Saturn_Report SaturnSC.A1ModJulian SaturnSC.Saturn.Altitude SaturnSC.Saturn.BetaAngle SaturnSC.Saturn.C3Energy SaturnSC.Saturn.ECC SaturnSC.Saturn.Latitude SaturnSC.Saturn.Longitude SaturnSC.Saturn.HMAG SaturnSC.Saturn.MA SaturnSC.Saturn.MM SaturnSC.Saturn.OrbitPeriod SaturnSC.Saturn.RadApo SaturnSC.Saturn.RadPer SaturnSC.Saturn.RMAG SaturnSC.Saturn.SMA SaturnSC.Saturn.TA SaturnSC.Saturn.SemilatusRectum SaturnSC.Saturn.VelApoapsis SaturnSC.Saturn.VelPeriapsis SaturnSC.Saturn.MHA SaturnSC.Saturn.LST;
EndFor ;
