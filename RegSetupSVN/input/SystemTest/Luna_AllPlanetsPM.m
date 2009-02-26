% REVISION HISTORY
% $Id: Luna_AllPlanetsPM.m,v 1.4 2008/10/14 17:58:40 edove Exp $

Create Spacecraft Moon;
GMAT Moon.DateFormat = UTCGregorian;
GMAT Moon.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT Moon.CoordinateSystem = MoonMJ2000Eq;
GMAT Moon.DisplayStateType = Cartesian;
GMAT Moon.X = -1486.792117191545200;
GMAT Moon.Y = 0.0;
GMAT Moon.Z = 1486.792117191543000;
GMAT Moon.VX = -0.142927729144255;
GMAT Moon.VY = -1.631407624437537;
GMAT Moon.VZ = 0.142927729144255;
GMAT Moon.Cd = 2.2;
GMAT Moon.Cr = 1.2;
GMAT Moon.DragArea = 20;
GMAT Moon.SRPArea = 20;
GMAT Moon.DryMass = 1000;

Create ForceModel Moon2Body;
GMAT Moon2Body.CentralBody = Luna;
GMAT Moon2Body.PointMasses = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
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

Create ReportFile Moon_Report
GMAT Moon_Report.Filename = ./output/SystemTest/Luna_AllPlanetsPM.report;
GMAT Moon_Report.Precision = 16;
GMAT Moon_Report.WriteHeaders = Off;
GMAT Moon_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Moon_Report Moon.A1ModJulian Moon.MoonMJ2000Eq.X Moon.MoonMJ2000Eq.Y Moon.MoonMJ2000Eq.Z Moon.MoonMJ2000Eq.VX Moon.MoonMJ2000Eq.VY Moon.MoonMJ2000Eq.VZ;
For OutputStepSize = 1:72;
	Propagate   RKV89(Moon, {Moon.ElapsedSecs = 300});
	Report      Moon_Report Moon.A1ModJulian Moon.MoonMJ2000Eq.X Moon.MoonMJ2000Eq.Y Moon.MoonMJ2000Eq.Z Moon.MoonMJ2000Eq.VX Moon.MoonMJ2000Eq.VY Moon.MoonMJ2000Eq.VZ;
EndFor ;
