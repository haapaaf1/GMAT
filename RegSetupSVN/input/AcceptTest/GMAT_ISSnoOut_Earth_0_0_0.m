%% $Id: GMAT_ISSnoOut_Earth_0_0_0.m,v 1.2 2007/07/26 19:12:28 edove Exp $

Create Spacecraft ISS;
 GMAT ISS.J2000BodyName = Earth;
 GMAT ISS.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
 GMAT ISS.DisplayStateType = Cartesian;
 GMAT ISS.CoordinateSystem = EarthMJ2000Eq;
 GMAT ISS.X = -4453.7835859999996;
 GMAT ISS.Y = -5038.2037559999999;
 GMAT ISS.Z = -426.384456;
 GMAT ISS.VX = 3.8318880000000002;
 GMAT ISS.VY = -2.8872209999999998;
 GMAT ISS.VZ = -6.0182320000000002;
 GMAT ISS.Cd = 2.2;
 GMAT ISS.Cr = 1.2;
 GMAT ISS.DragArea = 20;
 GMAT ISS.SRPArea = 20;
 GMAT ISS.DryMass = 1000;
 GMAT ISS.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.PrimaryBodies = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;
GMAT Earth2Body.Gravity.Earth.Model = JGM2;
GMAT Earth2Body.Gravity.Earth.Degree = 0;
GMAT Earth2Body.Gravity.Earth.Order = 0;
GMAT Earth2Body.PointMasses   = {};

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Propagate   RKV89(ISS, {ISS.ElapsedSecs = 86400});
