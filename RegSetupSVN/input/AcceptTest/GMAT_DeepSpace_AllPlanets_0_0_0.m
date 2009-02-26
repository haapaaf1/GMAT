%% $Id: GMAT_DeepSpace_AllPlanets_0_0_0.m,v 1.5 2007/07/26 19:12:28 edove Exp $

Create Spacecraft DeepSpace;
GMAT DeepSpace.DateFormat = UTCGregorian;
GMAT DeepSpace.Epoch.UTCGregorian = 01 Jan 2000 12:00:00.000;
GMAT DeepSpace.CoordinateSystem = SunMJ2000Ec;
GMAT DeepSpace.DisplayStateType = Cartesian;
GMAT DeepSpace.X = 30043412.094803076000000;
GMAT DeepSpace.Y = 143707423.481292670000000;
GMAT DeepSpace.Z = 2198384.040184043300000;
GMAT DeepSpace.VX = -29.715920923036403;
GMAT DeepSpace.VY = 6.056690472247896;
GMAT DeepSpace.VZ = 0.123271169290614;
GMAT DeepSpace.Cd = 2.2;
GMAT DeepSpace.Cr = 1.2;
GMAT DeepSpace.DragArea = 20;
GMAT DeepSpace.SRPArea = 20;
GMAT DeepSpace.DryMass = 1000;
GMAT DeepSpace.TotalMass = 1000;

Create ForceModel Sun2Body;
GMAT Sun2Body.CentralBody = Sun;
GMAT Sun2Body.PointMasses = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT Sun2Body.Drag = None;
GMAT Sun2Body.SRP = Off;
GMAT Sun2Body.ErrorControl = RSSStep;

Create Propagator RKV89;
GMAT RKV89.FM = Sun2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 0.001;
GMAT RKV89.MaxStep = 30000;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 
GMAT SolarSystem.UseTTForEphemeris = true;

Create CoordinateSystem SunMJ2000Ec;
GMAT SunMJ2000Ec.Origin = Sun;
GMAT SunMJ2000Ec.J2000Body = Earth;
GMAT SunMJ2000Ec.Axes = MJ2000Ec;
GMAT SunMJ2000Ec.Epoch = 21545;
GMAT SunMJ2000Ec.UpdateInterval = 60;

Create ReportFile DeepSpace_Report
GMAT DeepSpace_Report.Filename = ./output/AcceptTest/GMAT_DeepSpace_AllPlanets_0_0_0.report;
GMAT DeepSpace_Report.Precision = 16;
GMAT DeepSpace_Report.WriteHeaders = Off;
GMAT DeepSpace_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report DeepSpace_Report DeepSpace.A1ModJulian DeepSpace.SunMJ2000Ec.X DeepSpace.SunMJ2000Ec.Y DeepSpace.SunMJ2000Ec.Z DeepSpace.SunMJ2000Ec.VX DeepSpace.SunMJ2000Ec.VY DeepSpace.SunMJ2000Ec.VZ;
For OutputStepSize = 1:365;
	Propagate   RKV89(DeepSpace, {DeepSpace.ElapsedSecs = 86400});
	Report      DeepSpace_Report DeepSpace.A1ModJulian DeepSpace.SunMJ2000Ec.X DeepSpace.SunMJ2000Ec.Y DeepSpace.SunMJ2000Ec.Z DeepSpace.SunMJ2000Ec.VX DeepSpace.SunMJ2000Ec.VY DeepSpace.SunMJ2000Ec.VZ;
EndFor ;

