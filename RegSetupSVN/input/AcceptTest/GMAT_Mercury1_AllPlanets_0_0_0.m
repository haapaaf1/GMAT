%% $Id: GMAT_Mercury1_AllPlanets_0_0_0.m,v 1.4 2007/07/26 19:12:28 edove Exp $

Create Spacecraft MercurySC;
GMAT MercurySC.DateFormat = UTCGregorian
GMAT MercurySC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT MercurySC.CoordinateSystem = MercuryMJ2000Eq;
GMAT MercurySC.DisplayStateType = Cartesian;
GMAT MercurySC.X = -2164.769322630887000;
GMAT MercurySC.Y = 0.0;
GMAT MercurySC.Z = 2164.769322630886100;
GMAT MercurySC.VX = -0.251096955137200;
GMAT MercurySC.VY = -2.866074270797602;
GMAT MercurySC.VZ = 0.251096955137201;
GMAT MercurySC.Cd = 2.2;
GMAT MercurySC.Cr = 1.2;
GMAT MercurySC.DragArea = 20;
GMAT MercurySC.SRPArea = 20;
GMAT MercurySC.DryMass = 1000;

Create ForceModel Mercury2Body;
GMAT Mercury2Body.CentralBody = Mercury;
GMAT Mercury2Body.PrimaryBodies = {};
GMAT Mercury2Body.PointMasses   = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT Mercury2Body.Drag = None;
GMAT Mercury2Body.SRP = Off;
GMAT Mercury2Body.ErrorControl = RSSStep;

Create Propagator RKV89;
GMAT RKV89.FM = Mercury2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem MercuryMJ2000Eq;
GMAT MercuryMJ2000Eq.Origin = Mercury;
GMAT MercuryMJ2000Eq.J2000Body = Earth;
GMAT MercuryMJ2000Eq.Axes = MJ2000Eq;
GMAT MercuryMJ2000Eq.Epoch = 21545;
GMAT MercuryMJ2000Eq.UpdateInterval = 60;

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

Create ReportFile Mercury_Report
GMAT Mercury_Report.Filename = ./output/AcceptTest/GMAT_Mercury1_AllPlanets_0_0_0.report;
GMAT Mercury_Report.Precision = 16;
GMAT Mercury_Report.WriteHeaders = Off;
GMAT Mercury_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Mercury_Report MercurySC.A1ModJulian MercurySC.MercuryMJ2000Eq.X MercurySC.MercuryMJ2000Eq.Y MercurySC.MercuryMJ2000Eq.Z MercurySC.MercuryMJ2000Eq.VX MercurySC.MercuryMJ2000Eq.VY MercurySC.MercuryMJ2000Eq.VZ;
For OutputStepSize = 1:864;
	Propagate   RKV89(MercurySC, {MercurySC.ElapsedSecs = 300});
	Report      Mercury_Report MercurySC.A1ModJulian MercurySC.MercuryMJ2000Eq.X MercurySC.MercuryMJ2000Eq.Y MercurySC.MercuryMJ2000Eq.Z MercurySC.MercuryMJ2000Eq.VX MercurySC.MercuryMJ2000Eq.VY MercurySC.MercuryMJ2000Eq.VZ;
EndFor ;
