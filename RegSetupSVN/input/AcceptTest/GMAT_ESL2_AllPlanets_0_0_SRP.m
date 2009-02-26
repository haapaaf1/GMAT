%% $Id: GMAT_ESL2_AllPlanets_0_0_SRP.m,v 1.5 2007/07/26 19:12:28 edove Exp $

Create Spacecraft ESL2;
GMAT ESL2.DateFormat = UTCGregorian;
GMAT ESL2.Epoch.UTCGregorian = 05 Feb 2006 17:05:48.772;
GMAT ESL2.CoordinateSystem = EarthMJ2000Eq;
GMAT ESL2.DisplayStateType = Cartesian;
GMAT ESL2.X = 1010800.968074728;
GMAT ESL2.Y = -910963.5377102628;
GMAT ESL2.Z = -295145.6311353027;
GMAT ESL2.VX = 0.2642852647102676;
GMAT ESL2.VY = 0.286744175490658;
GMAT ESL2.VZ = 0.07338744995264675;
GMAT ESL2.Cd = 2.2;
GMAT ESL2.Cr = 1.2;
GMAT ESL2.DragArea = 20;
GMAT ESL2.SRPArea = 20;
GMAT ESL2.DryMass = 1000;
GMAT ESL2.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PointMasses = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = On;
GMAT Earth2Body.SRP.Flux_Pressure = 4.53443218374393e-006;
GMAT Earth2Body.ErrorControl = RSSStep;

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 0.001;
GMAT RKV89.MaxStep = 15000;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 
GMAT SolarSystem.UseTTForEphemeris = true;

Create ReportFile ESL2_Report
GMAT ESL2_Report.Filename = ./output/AcceptTest/GMAT_ESL2_AllPlanets_0_0_SRP.report;
GMAT ESL2_Report.Precision = 16;
GMAT ESL2_Report.WriteHeaders = Off;
GMAT ESL2_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report ESL2_Report ESL2.A1ModJulian ESL2.EarthMJ2000Eq.X ESL2.EarthMJ2000Eq.Y ESL2.EarthMJ2000Eq.Z ESL2.EarthMJ2000Eq.VX ESL2.EarthMJ2000Eq.VY ESL2.EarthMJ2000Eq.VZ;
For OutputStepSize = 1:360;
	Propagate   RKV89(ESL2, {ESL2.ElapsedSecs = 43200});
	Report      ESL2_Report ESL2.A1ModJulian ESL2.EarthMJ2000Eq.X ESL2.EarthMJ2000Eq.Y ESL2.EarthMJ2000Eq.Z ESL2.EarthMJ2000Eq.VX ESL2.EarthMJ2000Eq.VY ESL2.EarthMJ2000Eq.VZ;
EndFor ;

