%% $Id: Performance_GMAT_SunSync_AllPlanets_output.m,v 1.1 2007/08/31 17:45:29 edove Exp $

Create Spacecraft SunSync;
GMAT SunSync.J2000BodyName = Earth;
GMAT SunSync.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT SunSync.StateType = Cartesian;
GMAT SunSync.CoordinateSystem = EarthMJ2000Eq;
GMAT SunSync.X = -2290.301063;
GMAT SunSync.Y = -6379.471940;
GMAT SunSync.Z = 0;
GMAT SunSync.VX = -0.883923;
GMAT SunSync.VY = 0.317338;
GMAT SunSync.VZ = 7.610832;
GMAT SunSync.Cd = 2.2;
GMAT SunSync.Cr = 1.2;
GMAT SunSync.DragArea = 20;
GMAT SunSync.SRPArea = 20;
GMAT SunSync.DryMass = 1000;
GMAT SunSync.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PrimaryBodies = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.Gravity.Earth.Model = JGM2;
GMAT Earth2Body.Gravity.Earth.Degree = 0;
GMAT Earth2Body.Gravity.Earth.Order = 0;
GMAT Earth2Body.PointMasses   = {Sun, Luna, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = PrinceDormand78;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 0.00001;
GMAT RKV89.MaxStep = 300;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile SunSync_Report
GMAT SunSync_Report.Filename =  ./output/Performance/Performance_GMAT_SunSync_AllPlanets_output.report;
GMAT SunSync_Report.Precision = 16;
GMAT SunSync_Report.Add = {SunSync.A1ModJulian,SunSync.X,SunSync.Y,SunSync.Z,SunSync.VX,SunSync.VY,SunSync.VZ};
GMAT SunSync_Report.WriteHeaders = On;
GMAT SunSync_Report.ColumnWidth = 20;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Propagate   RKV89(SunSync, {SunSync.ElapsedDays = 30});