%% $Id: CSParams_GMAT_Saturn1_2Body_SaturnMJ2000Ec.m,v 1.4 2007/07/26 19:12:27 edove Exp $

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

Create CoordinateSystem SaturnMJ2000Ec;
GMAT SaturnMJ2000Ec.Origin = Saturn;
GMAT SaturnMJ2000Ec.J2000Body = Earth;
GMAT SaturnMJ2000Ec.Axes = MJ2000Ec;
GMAT SaturnMJ2000Ec.Epoch = 21545;
GMAT SaturnMJ2000Ec.UpdateInterval = 60;

Create CoordinateSystem SaturnMJ2000Eq;
GMAT SaturnMJ2000Eq.Origin = Saturn;
GMAT SaturnMJ2000Eq.J2000Body = Earth;
GMAT SaturnMJ2000Eq.Axes = MJ2000Eq;
GMAT SaturnMJ2000Eq.Epoch = 21545;
GMAT SaturnMJ2000Eq.UpdateInterval = 60;

Create ReportFile Saturn_Report
GMAT Saturn_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Saturn1_2Body_SaturnMJ2000Ec.report;
GMAT Saturn_Report.Precision = 16;
GMAT Saturn_Report.WriteHeaders = On;
GMAT Saturn_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Saturn_Report SaturnSC.A1ModJulian SaturnSC.SaturnMJ2000Ec.X SaturnSC.SaturnMJ2000Ec.Y SaturnSC.SaturnMJ2000Ec.Z SaturnSC.SaturnMJ2000Ec.VX SaturnSC.SaturnMJ2000Ec.VY SaturnSC.SaturnMJ2000Ec.VZ SaturnSC.SaturnMJ2000Ec.VMAG SaturnSC.SaturnMJ2000Ec.RAV SaturnSC.SaturnMJ2000Ec.HX SaturnSC.SaturnMJ2000Ec.HY SaturnSC.SaturnMJ2000Ec.HZ SaturnSC.SaturnMJ2000Ec.AOP SaturnSC.SaturnMJ2000Ec.DEC SaturnSC.SaturnMJ2000Ec.DECV SaturnSC.SaturnMJ2000Ec.INC SaturnSC.SaturnMJ2000Ec.RA SaturnSC.SaturnMJ2000Ec.RAAN;
GMAT Saturn_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(SaturnSC, {SaturnSC.ElapsedSecs = 600});
      Report      Saturn_Report SaturnSC.A1ModJulian SaturnSC.SaturnMJ2000Ec.X SaturnSC.SaturnMJ2000Ec.Y SaturnSC.SaturnMJ2000Ec.Z SaturnSC.SaturnMJ2000Ec.VX SaturnSC.SaturnMJ2000Ec.VY SaturnSC.SaturnMJ2000Ec.VZ SaturnSC.SaturnMJ2000Ec.VMAG SaturnSC.SaturnMJ2000Ec.RAV SaturnSC.SaturnMJ2000Ec.HX SaturnSC.SaturnMJ2000Ec.HY SaturnSC.SaturnMJ2000Ec.HZ SaturnSC.SaturnMJ2000Ec.AOP SaturnSC.SaturnMJ2000Ec.DEC SaturnSC.SaturnMJ2000Ec.DECV SaturnSC.SaturnMJ2000Ec.INC SaturnSC.SaturnMJ2000Ec.RA SaturnSC.SaturnMJ2000Ec.RAAN;
EndFor ;
