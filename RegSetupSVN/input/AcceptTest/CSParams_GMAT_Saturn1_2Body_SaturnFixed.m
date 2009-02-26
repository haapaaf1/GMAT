%% $Id: CSParams_GMAT_Saturn1_2Body_SaturnFixed.m,v 1.4 2007/07/26 19:12:27 edove Exp $

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

Create CoordinateSystem SaturnFixed;
GMAT SaturnFixed.Origin = Saturn;
GMAT SaturnFixed.J2000Body = Earth;
GMAT SaturnFixed.Axes = BodyFixed;
GMAT SaturnFixed.Epoch = 21545.000000397937;
GMAT SaturnFixed.UpdateInterval = 60;

Create ReportFile Saturn_Report
GMAT Saturn_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Saturn1_2Body_SaturnFixed.report;
GMAT Saturn_Report.Precision = 16;
GMAT Saturn_Report.WriteHeaders = On;
GMAT Saturn_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Saturn_Report SaturnSC.A1ModJulian SaturnSC.SaturnFixed.X SaturnSC.SaturnFixed.Y SaturnSC.SaturnFixed.Z SaturnSC.SaturnFixed.VX SaturnSC.SaturnFixed.VY SaturnSC.SaturnFixed.VZ SaturnSC.SaturnFixed.VMAG SaturnSC.SaturnFixed.RAV SaturnSC.SaturnFixed.HX SaturnSC.SaturnFixed.HY SaturnSC.SaturnFixed.HZ SaturnSC.SaturnFixed.AOP SaturnSC.SaturnFixed.DEC SaturnSC.SaturnFixed.DECV SaturnSC.SaturnFixed.INC SaturnSC.SaturnFixed.RA SaturnSC.SaturnFixed.RAAN;
GMAT Saturn_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(SaturnSC, {SaturnSC.ElapsedSecs = 600});
      Report      Saturn_Report SaturnSC.A1ModJulian SaturnSC.SaturnFixed.X SaturnSC.SaturnFixed.Y SaturnSC.SaturnFixed.Z SaturnSC.SaturnFixed.VX SaturnSC.SaturnFixed.VY SaturnSC.SaturnFixed.VZ SaturnSC.SaturnFixed.VMAG SaturnSC.SaturnFixed.RAV SaturnSC.SaturnFixed.HX SaturnSC.SaturnFixed.HY SaturnSC.SaturnFixed.HZ SaturnSC.SaturnFixed.AOP SaturnSC.SaturnFixed.DEC SaturnSC.SaturnFixed.DECV SaturnSC.SaturnFixed.INC SaturnSC.SaturnFixed.RA SaturnSC.SaturnFixed.RAAN;
EndFor ;
