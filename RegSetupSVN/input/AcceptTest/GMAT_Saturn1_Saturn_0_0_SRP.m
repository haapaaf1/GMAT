%% $Id: GMAT_Saturn1_Saturn_0_0_SRP.m,v 1.4 2007/07/26 19:12:29 edove Exp $

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
GMAT Saturn2Body.SRP = On;
GMAT Saturn2Body.SRP.Flux_Pressure = 4.53443218374393e-006;
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

Create CoordinateSystem EarthMJ2000Eq;
GMAT EarthMJ2000Eq.Origin = Earth;
GMAT EarthMJ2000Eq.J2000Body = Earth;
GMAT EarthMJ2000Eq.Axes = MJ2000Eq;
GMAT EarthMJ2000Eq.Epoch = 21545;
GMAT EarthMJ2000Eq.UpdateInterval = 60;


Create CoordinateSystem EarthMJ2000Ec;
GMAT EarthMJ2000Ec.Origin = Earth;
GMAT EarthMJ2000Ec.J2000Body = Earth;
GMAT EarthMJ2000Ec.Axes = MJ2000Ec;
GMAT EarthMJ2000Ec.Epoch = 21545;
GMAT EarthMJ2000Ec.UpdateInterval = 60;


Create CoordinateSystem EarthFixed;
GMAT EarthFixed.Origin = Earth;
GMAT EarthFixed.J2000Body = Earth;
GMAT EarthFixed.Axes = BodyFixed;
GMAT EarthFixed.Epoch = 21545;
GMAT EarthFixed.UpdateInterval = 60;

Create ReportFile Saturn_Report
GMAT Saturn_Report.Filename = ./output/AcceptTest/GMAT_Saturn1_Saturn_0_0_SRP.report;
GMAT Saturn_Report.Precision = 16;
GMAT Saturn_Report.WriteHeaders = Off;
GMAT Saturn_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Saturn_Report SaturnSC.A1ModJulian SaturnSC.SaturnMJ2000Eq.X SaturnSC.SaturnMJ2000Eq.Y SaturnSC.SaturnMJ2000Eq.Z SaturnSC.SaturnMJ2000Eq.VX SaturnSC.SaturnMJ2000Eq.VY SaturnSC.SaturnMJ2000Eq.VZ;
For OutputStepSize = 1:864;
	Propagate   RKV89(SaturnSC, {SaturnSC.ElapsedSecs = 300});
	Report      Saturn_Report SaturnSC.A1ModJulian SaturnSC.SaturnMJ2000Eq.X SaturnSC.SaturnMJ2000Eq.Y SaturnSC.SaturnMJ2000Eq.Z SaturnSC.SaturnMJ2000Eq.VX SaturnSC.SaturnMJ2000Eq.VY SaturnSC.SaturnMJ2000Eq.VZ;
EndFor ;
