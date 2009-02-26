%% $Id: Performance_GMAT_GEOPM_1PM_output.m,v 1.1 2007/08/31 17:45:28 edove Exp $

Create Spacecraft GEO;
 GMAT GEO.J2000BodyName = Earth;
 GMAT GEO.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT GEO.StateType = Cartesian;
 GMAT GEO.CoordinateSystem = EarthMJ2000Eq;
 GMAT GEO.X = 36607.3582560;
 GMAT GEO.Y = -20921.723703;
 GMAT GEO.Z = 0.0;
 GMAT GEO.VX = 1.52563600;
 GMAT GEO.VY = 2.66945100;
 GMAT GEO.VZ = 0.0;
 GMAT GEO.Cd = 2.2;
 GMAT GEO.Cr = 1.2;
 GMAT GEO.DragArea = 20;
 GMAT GEO.SRPArea = 20;
 GMAT GEO.DryMass = 1000;
 GMAT GEO.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PointMasses   = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;



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

Create ReportFile GEO_Report
GMAT GEO_Report.Filename =  ./output/Performance/Performance_GMAT_GEOPM_1PM_output.report;
GMAT GEO_Report.Precision = 16;
GMAT GEO_Report.Add = {GEO.A1ModJulian,GEO.X,GEO.Y,GEO.Z,GEO.VX,GEO.VY,GEO.VZ};
GMAT GEO_Report.WriteHeaders = On;
GMAT GEO_Report.ColumnWidth = 20;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Propagate   RKV89(GEO, {GEO.ElapsedDays = 30});