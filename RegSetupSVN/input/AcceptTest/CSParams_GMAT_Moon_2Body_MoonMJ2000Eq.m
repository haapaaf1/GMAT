%% $Id: CSParams_GMAT_Moon_2Body_MoonMJ2000Eq.m,v 1.4 2007/07/26 19:12:27 edove Exp $

Create Spacecraft MoonSC;
GMAT MoonSC.DateFormat = UTCGregorian
GMAT MoonSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT MoonSC.CoordinateSystem = MoonMJ2000Eq;
GMAT MoonSC.DisplayStateType = Cartesian;
GMAT MoonSC.X = -1486.792117191545200;
GMAT MoonSC.Y = 0.0;
GMAT MoonSC.Z = 1486.792117191543000;
GMAT MoonSC.VX = -0.142927729144255;
GMAT MoonSC.VY = -1.631407624437537;
GMAT MoonSC.VZ = 0.142927729144255;
GMAT MoonSC.Cd = 2.2;
GMAT MoonSC.Cr = 1.2;
GMAT MoonSC.DragArea = 20;
GMAT MoonSC.SRPArea = 20;
GMAT MoonSC.DryMass = 1000;

Create ForceModel Moon2Body;
GMAT Moon2Body.CentralBody = Luna;
GMAT Moon2Body.PointMasses = {Luna};
GMAT Moon2Body.Drag = None;
GMAT Moon2Body.SRP = Off;
GMAT Moon2Body.ErrorControl = RSSStep;


Create Propagator RKV89;
GMAT RKV89.FM = Moon2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem MoonMJ2000Eq;
GMAT MoonMJ2000Eq.Origin = Luna;
GMAT MoonMJ2000Eq.J2000Body = Earth;
GMAT MoonMJ2000Eq.Axes = MJ2000Eq;
GMAT MoonMJ2000Eq.Epoch = 21545;
GMAT MoonMJ2000Eq.UpdateInterval = 60;

Create ReportFile Moon_Report
GMAT Moon_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Moon_2Body_MoonMJ2000Eq.report;
GMAT Moon_Report.Precision = 16;
GMAT Moon_Report.WriteHeaders = On;
GMAT Moon_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Moon_Report MoonSC.A1ModJulian MoonSC.MoonMJ2000Eq.X MoonSC.MoonMJ2000Eq.Y MoonSC.MoonMJ2000Eq.Z MoonSC.MoonMJ2000Eq.VX MoonSC.MoonMJ2000Eq.VY MoonSC.MoonMJ2000Eq.VZ MoonSC.MoonMJ2000Eq.VMAG MoonSC.MoonMJ2000Eq.RAV MoonSC.MoonMJ2000Eq.HX MoonSC.MoonMJ2000Eq.HY MoonSC.MoonMJ2000Eq.HZ MoonSC.MoonMJ2000Eq.AOP MoonSC.MoonMJ2000Eq.DEC MoonSC.MoonMJ2000Eq.DECV MoonSC.MoonMJ2000Eq.INC MoonSC.MoonMJ2000Eq.RA MoonSC.MoonMJ2000Eq.RAAN;
GMAT Moon_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(MoonSC, {MoonSC.ElapsedSecs = 600});
      Report      Moon_Report MoonSC.A1ModJulian MoonSC.MoonMJ2000Eq.X MoonSC.MoonMJ2000Eq.Y MoonSC.MoonMJ2000Eq.Z MoonSC.MoonMJ2000Eq.VX MoonSC.MoonMJ2000Eq.VY MoonSC.MoonMJ2000Eq.VZ MoonSC.MoonMJ2000Eq.VMAG MoonSC.MoonMJ2000Eq.RAV MoonSC.MoonMJ2000Eq.HX MoonSC.MoonMJ2000Eq.HY MoonSC.MoonMJ2000Eq.HZ MoonSC.MoonMJ2000Eq.AOP MoonSC.MoonMJ2000Eq.DEC MoonSC.MoonMJ2000Eq.DECV MoonSC.MoonMJ2000Eq.INC MoonSC.MoonMJ2000Eq.RA MoonSC.MoonMJ2000Eq.RAAN;
EndFor ;
