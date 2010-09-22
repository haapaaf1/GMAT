% Earth Moon L2 orbit

Create Spacecraft EML2;
GMAT EML2.DateFormat = UTCGregorian;
GMAT EML2.Epoch.UTCGregorian = 23 Jan 2010 00:00:03.999;
GMAT EML2.CoordinateSystem = EarthMJ2000Eq;
GMAT EML2.DisplayStateType = Cartesian;
GMAT EML2.X = 406326.22661300009;
GMAT EML2.Y = 177458.38761599999;
GMAT EML2.Z = 145838.58078999998;
GMAT EML2.VX = -0.517274673822;
GMAT EML2.VY = 0.774650366561;
GMAT EML2.VZ = 0.331416602654;
GMAT EML2.Cd = 2.2;
GMAT EML2.Cr = 1.2;
GMAT EML2.DragArea = 20;
GMAT EML2.SRPArea = 20;
GMAT EML2.DryMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PointMasses = {Sun, Luna, Earth, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.ErrorControl = RSSState;

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 1200;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 0.001;
GMAT RKV89.MaxStep = 1200;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 
GMAT SolarSystem.UseTTForEphemeris = true;

Create ReportFile EML2_Report
GMAT EML2_Report.Filename =  APT_GMAT_EML2_AllPlanets_0_0_0.report;
GMAT EML2_Report.Precision = 16;
GMAT EML2_Report.WriteHeaders = Off;
GMAT EML2_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

BeginMissionSequence;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report EML2_Report EML2.A1ModJulian EML2.EarthMJ2000Eq.X EML2.EarthMJ2000Eq.Y EML2.EarthMJ2000Eq.Z EML2.EarthMJ2000Eq.VX EML2.EarthMJ2000Eq.VY EML2.EarthMJ2000Eq.VZ;
%%%For OutputStepSize = 1:504;
For OutputStepSize = 1:100;
	Propagate   RKV89(EML2, {EML2.ElapsedSecs = 2400});
	Report      EML2_Report EML2.A1ModJulian EML2.EarthMJ2000Eq.X EML2.EarthMJ2000Eq.Y EML2.EarthMJ2000Eq.Z EML2.EarthMJ2000Eq.VX EML2.EarthMJ2000Eq.VY EML2.EarthMJ2000Eq.VZ;
EndFor ;

