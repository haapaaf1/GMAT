%% $Id: CSParams_GMAT_Mars1_2Body_MarsMJ2000Ec.m,v 1.4 2007/07/26 19:12:27 edove Exp $

Create Spacecraft MarsSC;
GMAT MarsSC.DateFormat = UTCGregorian
GMAT MarsSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT MarsSC.CoordinateSystem = MarsMJ2000Eq;
GMAT MarsSC.DisplayStateType = Cartesian;
GMAT MarsSC.X = -2737.481646173082000;
GMAT MarsSC.Y = 0.0;
GMAT MarsSC.Z = 2737.481646173082000;
GMAT MarsSC.VX = -0.311321695052649;
GMAT MarsSC.VY = -3.553492313930950;
GMAT MarsSC.VZ = 0.311321695052650;
GMAT MarsSC.Cd = 2.2;
GMAT MarsSC.Cr = 1.2;
GMAT MarsSC.DragArea = 20;
GMAT MarsSC.SRPArea = 20;
GMAT MarsSC.DryMass = 1000;

Create ForceModel Mars2Body;
GMAT Mars2Body.CentralBody = Mars;
GMAT Mars2Body.PointMasses = {Mars};
GMAT Mars2Body.Drag = None;
GMAT Mars2Body.SRP = Off;
GMAT Mars2Body.ErrorControl = RSSStep;


Create Propagator RKV89;
GMAT RKV89.FM = Mars2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem MarsMJ2000Ec;
GMAT MarsMJ2000Ec.Origin = Mars;
GMAT MarsMJ2000Ec.J2000Body = Earth;
GMAT MarsMJ2000Ec.Axes = MJ2000Ec;
GMAT MarsMJ2000Ec.Epoch = 21545;
GMAT MarsMJ2000Ec.UpdateInterval = 60;

Create CoordinateSystem MarsMJ2000Eq;
GMAT MarsMJ2000Eq.Origin = Mars;
GMAT MarsMJ2000Eq.J2000Body = Earth;
GMAT MarsMJ2000Eq.Axes = MJ2000Eq;
GMAT MarsMJ2000Eq.Epoch = 21545;
GMAT MarsMJ2000Eq.UpdateInterval = 60;

Create ReportFile Mars_Report
GMAT Mars_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Mars1_2Body_MarsMJ2000Ec.report;
GMAT Mars_Report.Precision = 16;
GMAT Mars_Report.WriteHeaders = On;
GMAT Mars_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Mars_Report MarsSC.A1ModJulian MarsSC.MarsMJ2000Ec.X MarsSC.MarsMJ2000Ec.Y MarsSC.MarsMJ2000Ec.Z MarsSC.MarsMJ2000Ec.VX MarsSC.MarsMJ2000Ec.VY MarsSC.MarsMJ2000Ec.VZ MarsSC.MarsMJ2000Ec.VMAG MarsSC.MarsMJ2000Ec.RAV MarsSC.MarsMJ2000Ec.HX MarsSC.MarsMJ2000Ec.HY MarsSC.MarsMJ2000Ec.HZ MarsSC.MarsMJ2000Ec.AOP MarsSC.MarsMJ2000Ec.DEC MarsSC.MarsMJ2000Ec.DECV MarsSC.MarsMJ2000Ec.INC MarsSC.MarsMJ2000Ec.RA MarsSC.MarsMJ2000Ec.RAAN;
GMAT Mars_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(MarsSC, {MarsSC.ElapsedSecs = 600});
      Report      Mars_Report MarsSC.A1ModJulian MarsSC.MarsMJ2000Ec.X MarsSC.MarsMJ2000Ec.Y MarsSC.MarsMJ2000Ec.Z MarsSC.MarsMJ2000Ec.VX MarsSC.MarsMJ2000Ec.VY MarsSC.MarsMJ2000Ec.VZ MarsSC.MarsMJ2000Ec.VMAG MarsSC.MarsMJ2000Ec.RAV MarsSC.MarsMJ2000Ec.HX MarsSC.MarsMJ2000Ec.HY MarsSC.MarsMJ2000Ec.HZ MarsSC.MarsMJ2000Ec.AOP MarsSC.MarsMJ2000Ec.DEC MarsSC.MarsMJ2000Ec.DECV MarsSC.MarsMJ2000Ec.INC MarsSC.MarsMJ2000Ec.RA MarsSC.MarsMJ2000Ec.RAAN;
EndFor ;