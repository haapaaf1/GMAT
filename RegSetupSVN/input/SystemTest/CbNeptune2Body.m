%% $Id: CbNeptune2Body.m,v 1.2 2007/07/26 19:13:11 edove Exp $

Create Spacecraft NeptuneSC;
GMAT NeptuneSC.DateFormat = UTCGregorian
GMAT NeptuneSC.Epoch = '01 Jun 2004 12:00:00.000';
GMAT NeptuneSC.CoordinateSystem = NeptuneMJ2000Eq;
GMAT NeptuneSC.DisplayStateType = Cartesian;
GMAT NeptuneSC.X = -20815.089640681723000;
GMAT NeptuneSC.Y = 0.0;
GMAT NeptuneSC.Z = 20815.089640681723000;
GMAT NeptuneSC.VX = -1.426423063858300;
GMAT NeptuneSC.VY = -16.281497481173282;
GMAT NeptuneSC.VZ = 1.426423063858303;
GMAT NeptuneSC.Cd = 2.2;
GMAT NeptuneSC.Cr = 1.2;
GMAT NeptuneSC.DragArea = 20;
GMAT NeptuneSC.SRPArea = 20;
GMAT NeptuneSC.DryMass = 1000;

Create ForceModel Neptune2Body;
GMAT Neptune2Body.CentralBody = Neptune;
GMAT Neptune2Body.PointMasses = {Neptune};
GMAT Neptune2Body.Drag = None;
GMAT Neptune2Body.SRP = Off;
GMAT Neptune2Body.ErrorControl = RSSStep;


Create Propagator RKV89;
GMAT RKV89.FM = Neptune2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem NeptuneMJ2000Eq;
GMAT NeptuneMJ2000Eq.Origin = Neptune;
GMAT NeptuneMJ2000Eq.J2000Body = Earth;
GMAT NeptuneMJ2000Eq.Axes = MJ2000Eq;
GMAT NeptuneMJ2000Eq.Epoch = 21545;
GMAT NeptuneMJ2000Eq.UpdateInterval = 60;

Create ReportFile Neptune_Report
GMAT Neptune_Report.Filename = ./output/SystemTest/CbNeptune2Body.report;
GMAT Neptune_Report.Precision = 16;
GMAT Neptune_Report.WriteHeaders = On;
GMAT Neptune_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Neptune_Report NeptuneSC.A1ModJulian NeptuneSC.Neptune.Altitude NeptuneSC.Neptune.BetaAngle NeptuneSC.Neptune.C3Energy NeptuneSC.Neptune.ECC NeptuneSC.Neptune.Latitude NeptuneSC.Neptune.Longitude NeptuneSC.Neptune.HMAG NeptuneSC.Neptune.MA NeptuneSC.Neptune.MM NeptuneSC.Neptune.OrbitPeriod NeptuneSC.Neptune.RadApo NeptuneSC.Neptune.RadPer NeptuneSC.Neptune.RMAG NeptuneSC.Neptune.SMA NeptuneSC.Neptune.TA NeptuneSC.Neptune.SemilatusRectum NeptuneSC.Neptune.VelApoapsis NeptuneSC.Neptune.VelPeriapsis NeptuneSC.Neptune.MHA NeptuneSC.Neptune.LST;
GMAT Neptune_Report.WriteHeaders = Off;
For OutputStepSize = 1:72;
	Propagate   RKV89(NeptuneSC, {NeptuneSC.ElapsedSecs = 600});
      Report      Neptune_Report NeptuneSC.A1ModJulian NeptuneSC.Neptune.Altitude NeptuneSC.Neptune.BetaAngle NeptuneSC.Neptune.C3Energy NeptuneSC.Neptune.ECC NeptuneSC.Neptune.Latitude NeptuneSC.Neptune.Longitude NeptuneSC.Neptune.HMAG NeptuneSC.Neptune.MA NeptuneSC.Neptune.MM NeptuneSC.Neptune.OrbitPeriod NeptuneSC.Neptune.RadApo NeptuneSC.Neptune.RadPer NeptuneSC.Neptune.RMAG NeptuneSC.Neptune.SMA NeptuneSC.Neptune.TA NeptuneSC.Neptune.SemilatusRectum NeptuneSC.Neptune.VelApoapsis NeptuneSC.Neptune.VelPeriapsis NeptuneSC.Neptune.MHA NeptuneSC.Neptune.LST;
EndFor ;
