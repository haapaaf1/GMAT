%% $Id: Integrator_GMAT_GEO_PD45_2Body.m,v 1.4 2007/07/26 19:12:29 edove Exp $

Create Spacecraft GEO;
 GMAT GEO.J2000BodyName = Earth;
 GMAT GEO.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT GEO.DisplayStateType = Cartesian;
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
GMAT Earth2Body.PrimaryBodies = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.Gravity.Earth.Model = JGM2;
GMAT Earth2Body.Gravity.Earth.Degree = 0;
GMAT Earth2Body.Gravity.Earth.Order = 0;
GMAT Earth2Body.PointMasses   = {};

Create Propagator PD45;
GMAT PD45.FM = Earth2Body;
GMAT PD45.Type = PrinceDormand45;
GMAT PD45.InitialStepSize = 60;
GMAT PD45.Accuracy = 1e-013;
GMAT PD45.MinStep = 60;
GMAT PD45.MaxStep = 60;
GMAT PD45.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create ReportFile GEO_Report
GMAT GEO_Report.Filename = ./output/AcceptTest/Integrator_GMAT_GEO_PD45_2Body.report;
GMAT GEO_Report.Precision = 16;
GMAT GEO_Report.WriteHeaders = On;
GMAT GEO_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report GEO_Report GEO.A1ModJulian GEO.X GEO.Y GEO.Z GEO.VX GEO.VY GEO.VZ;
GMAT GEO_Report.WriteHeaders = Off;
For OutputStepSize = 1:1008;
	Propagate   PD45(GEO, {GEO.ElapsedSecs = 600});
	Report      GEO_Report GEO.A1ModJulian GEO.X GEO.Y GEO.Z GEO.VX GEO.VY GEO.VZ;
EndFor ;
