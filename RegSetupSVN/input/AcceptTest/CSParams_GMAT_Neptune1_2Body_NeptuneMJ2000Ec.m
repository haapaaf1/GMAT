%% $Id: CSParams_GMAT_Neptune1_2Body_NeptuneMJ2000Ec.m,v 1.4 2007/07/26 19:12:27 edove Exp $

Create Spacecraft NeptuneSC;
GMAT NeptuneSC.DateFormat = UTCGregorian
GMAT NeptuneSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
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

Create CoordinateSystem NeptuneMJ2000Ec;
GMAT NeptuneMJ2000Ec.Origin = Neptune;
GMAT NeptuneMJ2000Ec.J2000Body = Earth;
GMAT NeptuneMJ2000Ec.Axes = MJ2000Ec;
GMAT NeptuneMJ2000Ec.Epoch = 21545;
GMAT NeptuneMJ2000Ec.UpdateInterval = 60;

Create CoordinateSystem NeptuneMJ2000Eq;
GMAT NeptuneMJ2000Eq.Origin = Neptune;
GMAT NeptuneMJ2000Eq.J2000Body = Earth;
GMAT NeptuneMJ2000Eq.Axes = MJ2000Eq;
GMAT NeptuneMJ2000Eq.Epoch = 21545;
GMAT NeptuneMJ2000Eq.UpdateInterval = 60;

Create ReportFile Neptune_Report
GMAT Neptune_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Neptune1_2Body_NeptuneMJ2000Ec.report;
GMAT Neptune_Report.Precision = 16;
GMAT Neptune_Report.WriteHeaders = On;
GMAT Neptune_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Neptune_Report NeptuneSC.A1ModJulian NeptuneSC.NeptuneMJ2000Ec.X NeptuneSC.NeptuneMJ2000Ec.Y NeptuneSC.NeptuneMJ2000Ec.Z NeptuneSC.NeptuneMJ2000Ec.VX NeptuneSC.NeptuneMJ2000Ec.VY NeptuneSC.NeptuneMJ2000Ec.VZ NeptuneSC.NeptuneMJ2000Ec.VMAG NeptuneSC.NeptuneMJ2000Ec.RAV NeptuneSC.NeptuneMJ2000Ec.HX NeptuneSC.NeptuneMJ2000Ec.HY NeptuneSC.NeptuneMJ2000Ec.HZ NeptuneSC.NeptuneMJ2000Ec.AOP NeptuneSC.NeptuneMJ2000Ec.DEC NeptuneSC.NeptuneMJ2000Ec.DECV NeptuneSC.NeptuneMJ2000Ec.INC NeptuneSC.NeptuneMJ2000Ec.RA NeptuneSC.NeptuneMJ2000Ec.RAAN;
GMAT Neptune_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(NeptuneSC, {NeptuneSC.ElapsedSecs = 600});
      Report      Neptune_Report NeptuneSC.A1ModJulian NeptuneSC.NeptuneMJ2000Ec.X NeptuneSC.NeptuneMJ2000Ec.Y NeptuneSC.NeptuneMJ2000Ec.Z NeptuneSC.NeptuneMJ2000Ec.VX NeptuneSC.NeptuneMJ2000Ec.VY NeptuneSC.NeptuneMJ2000Ec.VZ NeptuneSC.NeptuneMJ2000Ec.VMAG NeptuneSC.NeptuneMJ2000Ec.RAV NeptuneSC.NeptuneMJ2000Ec.HX NeptuneSC.NeptuneMJ2000Ec.HY NeptuneSC.NeptuneMJ2000Ec.HZ NeptuneSC.NeptuneMJ2000Ec.AOP NeptuneSC.NeptuneMJ2000Ec.DEC NeptuneSC.NeptuneMJ2000Ec.DECV NeptuneSC.NeptuneMJ2000Ec.INC NeptuneSC.NeptuneMJ2000Ec.RA NeptuneSC.NeptuneMJ2000Ec.RAAN;
EndFor ;
