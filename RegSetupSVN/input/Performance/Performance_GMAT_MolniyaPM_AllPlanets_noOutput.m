%% $Id: Performance_GMAT_MolniyaPM_AllPlanets_noOutput.m,v 1.1 2007/08/31 17:45:29 edove Exp $

Create Spacecraft Molniya;
GMAT Molniya.J2000BodyName = Earth;
GMAT Molniya.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT Molniya.StateType = Cartesian;
GMAT Molniya.CoordinateSystem = EarthMJ2000Eq;
GMAT Molniya.X = -1529.894287;
GMAT Molniya.Y = -2672.877357;
GMAT Molniya.Z = -6150.115340;
GMAT Molniya.VX = 8.7175180;
GMAT Molniya.VY = -4.989709;
GMAT Molniya.VZ = 0.0;
GMAT Molniya.Cd = 2.2;
GMAT Molniya.Cr = 1.2;
GMAT Molniya.DragArea = 20;
GMAT Molniya.SRPArea = 20;
GMAT Molniya.DryMass = 1000;
GMAT Molniya.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.CentralBody = Earth;
GMAT Earth2Body.PointMasses   = {Earth, Sun, Luna, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
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

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

Propagate   RKV89(Molniya, {Molniya.ElapsedDays = 30});