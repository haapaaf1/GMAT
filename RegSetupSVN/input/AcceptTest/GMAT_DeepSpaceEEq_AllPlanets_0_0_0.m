%% $Id: GMAT_DeepSpaceEEq_AllPlanets_0_0_0.m,v 1.5 2007/07/26 19:12:28 edove Exp $

Create Spacecraft DeepSpace;
GMAT DeepSpace.DateFormat = UTCGregorian;
GMAT DeepSpace.Epoch.UTCGregorian = 01 Jan 2000 12:00:00.000;
GMAT DeepSpace.CoordinateSystem = EarthMJ2000Eq;
GMAT DeepSpace.DisplayStateType = Cartesian;
GMAT DeepSpace.X = 56937994.01126761;
GMAT DeepSpace.Y = -1713823.4988065399;
GMAT DeepSpace.Z = 1657873.0599321402;
GMAT DeepSpace.VX = 0.045105343375790;
GMAT DeepSpace.VY = 10.614382847132099;
GMAT DeepSpace.VZ = 4.735060583161360;
GMAT DeepSpace.Cd = 2.2;
GMAT DeepSpace.Cr = 1.2;
GMAT DeepSpace.DragArea = 20;
GMAT DeepSpace.SRPArea = 20;
GMAT DeepSpace.DryMass = 1000;
GMAT DeepSpace.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PointMasses = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.ErrorControl = RSSStep;

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 0.001;
GMAT RKV89.MaxStep = 30000;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 
GMAT SolarSystem.UseTTForEphemeris = true;

Create ReportFile DeepSpace_Report
GMAT DeepSpace_Report.Filename = ./output/AcceptTest/GMAT_DeepSpaceEEq_AllPlanets_0_0_0.report;
GMAT DeepSpace_Report.Precision = 16;
GMAT DeepSpace_Report.WriteHeaders = Off;
GMAT DeepSpace_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.
% Output Report file data for each propagation set in the FOR loop

Report DeepSpace_Report DeepSpace.A1ModJulian DeepSpace.EarthMJ2000Eq.X DeepSpace.EarthMJ2000Eq.Y DeepSpace.EarthMJ2000Eq.Z DeepSpace.EarthMJ2000Eq.VX DeepSpace.EarthMJ2000Eq.VY DeepSpace.EarthMJ2000Eq.VZ;
For OutputStepSize = 1:365;
	Propagate   RKV89(DeepSpace, {DeepSpace.ElapsedSecs = 86400});
	Report      DeepSpace_Report DeepSpace.A1ModJulian DeepSpace.EarthMJ2000Eq.X DeepSpace.EarthMJ2000Eq.Y DeepSpace.EarthMJ2000Eq.Z DeepSpace.EarthMJ2000Eq.VX DeepSpace.EarthMJ2000Eq.VY DeepSpace.EarthMJ2000Eq.VZ;
EndFor ;

