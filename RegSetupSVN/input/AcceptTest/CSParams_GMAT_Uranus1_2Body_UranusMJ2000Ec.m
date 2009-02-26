%% $Id: CSParams_GMAT_Uranus1_2Body_UranusMJ2000Ec.m,v 1.4 2007/07/26 19:12:27 edove Exp $

Create Spacecraft UranusSC;
GMAT UranusSC.DateFormat = UTCGregorian
GMAT UranusSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT UranusSC.CoordinateSystem = UranusMJ2000Eq;
GMAT UranusSC.DisplayStateType = Cartesian;
GMAT UranusSC.X = -26762.258109447845000;
GMAT UranusSC.Y = 0.0;
GMAT UranusSC.Z = 26762.258109447823000;
GMAT UranusSC.VX = -1.158158360792704;
GMAT UranusSC.VY = -13.219466869135891;
GMAT UranusSC.VZ = 1.158158360792704;
GMAT UranusSC.Cd = 2.2;
GMAT UranusSC.Cr = 1.2;
GMAT UranusSC.DragArea = 20;
GMAT UranusSC.SRPArea = 20;
GMAT UranusSC.DryMass = 1000;

Create ForceModel Uranus2Body;
GMAT Uranus2Body.CentralBody = Uranus;
GMAT Uranus2Body.PointMasses = {Uranus};
GMAT Uranus2Body.Drag = None;
GMAT Uranus2Body.SRP = Off;
GMAT Uranus2Body.ErrorControl = RSSStep;


Create Propagator RKV89;
GMAT RKV89.FM = Uranus2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem UranusMJ2000Ec;
GMAT UranusMJ2000Ec.Origin = Uranus;
GMAT UranusMJ2000Ec.J2000Body = Earth;
GMAT UranusMJ2000Ec.Axes = MJ2000Ec;
GMAT UranusMJ2000Ec.Epoch = 21545;
GMAT UranusMJ2000Ec.UpdateInterval = 60;

Create CoordinateSystem UranusMJ2000Eq;
GMAT UranusMJ2000Eq.Origin = Uranus;
GMAT UranusMJ2000Eq.J2000Body = Earth;
GMAT UranusMJ2000Eq.Axes = MJ2000Eq;
GMAT UranusMJ2000Eq.Epoch = 21545;
GMAT UranusMJ2000Eq.UpdateInterval = 60;

Create ReportFile Uranus_Report
GMAT Uranus_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Uranus1_2Body_UranusMJ2000Ec.report;
GMAT Uranus_Report.Precision = 16;
GMAT Uranus_Report.WriteHeaders = On;
GMAT Uranus_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Uranus_Report UranusSC.A1ModJulian UranusSC.UranusMJ2000Ec.X UranusSC.UranusMJ2000Ec.Y UranusSC.UranusMJ2000Ec.Z UranusSC.UranusMJ2000Ec.VX UranusSC.UranusMJ2000Ec.VY UranusSC.UranusMJ2000Ec.VZ UranusSC.UranusMJ2000Ec.VMAG UranusSC.UranusMJ2000Ec.RAV UranusSC.UranusMJ2000Ec.HX UranusSC.UranusMJ2000Ec.HY UranusSC.UranusMJ2000Ec.HZ UranusSC.UranusMJ2000Ec.AOP UranusSC.UranusMJ2000Ec.DEC UranusSC.UranusMJ2000Ec.DECV UranusSC.UranusMJ2000Ec.INC UranusSC.UranusMJ2000Ec.RA UranusSC.UranusMJ2000Ec.RAAN;
GMAT Uranus_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(UranusSC, {UranusSC.ElapsedSecs = 600});
      Report      Uranus_Report UranusSC.A1ModJulian UranusSC.UranusMJ2000Ec.X UranusSC.UranusMJ2000Ec.Y UranusSC.UranusMJ2000Ec.Z UranusSC.UranusMJ2000Ec.VX UranusSC.UranusMJ2000Ec.VY UranusSC.UranusMJ2000Ec.VZ UranusSC.UranusMJ2000Ec.VMAG UranusSC.UranusMJ2000Ec.RAV UranusSC.UranusMJ2000Ec.HX UranusSC.UranusMJ2000Ec.HY UranusSC.UranusMJ2000Ec.HZ UranusSC.UranusMJ2000Ec.AOP UranusSC.UranusMJ2000Ec.DEC UranusSC.UranusMJ2000Ec.DECV UranusSC.UranusMJ2000Ec.INC UranusSC.UranusMJ2000Ec.RA UranusSC.UranusMJ2000Ec.RAAN;
EndFor ;
