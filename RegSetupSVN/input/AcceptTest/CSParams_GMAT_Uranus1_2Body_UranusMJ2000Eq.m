%% $Id: CSParams_GMAT_Uranus1_2Body_UranusMJ2000Eq.m,v 1.4 2007/07/26 19:12:27 edove Exp $

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

Create CoordinateSystem UranusMJ2000Eq;
GMAT UranusMJ2000Eq.Origin = Uranus;
GMAT UranusMJ2000Eq.J2000Body = Earth;
GMAT UranusMJ2000Eq.Axes = MJ2000Eq;
GMAT UranusMJ2000Eq.Epoch = 21545;
GMAT UranusMJ2000Eq.UpdateInterval = 60;

Create ReportFile Uranus_Report
GMAT Uranus_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Uranus1_2Body_UranusMJ2000Eq.report;
GMAT Uranus_Report.Precision = 16;
GMAT Uranus_Report.WriteHeaders = On;
GMAT Uranus_Report.ColumnWidth = 25;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Uranus_Report UranusSC.A1ModJulian UranusSC.UranusMJ2000Eq.X UranusSC.UranusMJ2000Eq.Y UranusSC.UranusMJ2000Eq.Z UranusSC.UranusMJ2000Eq.VX UranusSC.UranusMJ2000Eq.VY UranusSC.UranusMJ2000Eq.VZ UranusSC.UranusMJ2000Eq.VMAG UranusSC.UranusMJ2000Eq.RAV UranusSC.UranusMJ2000Eq.HX UranusSC.UranusMJ2000Eq.HY UranusSC.UranusMJ2000Eq.HZ UranusSC.UranusMJ2000Eq.AOP UranusSC.UranusMJ2000Eq.DEC UranusSC.UranusMJ2000Eq.DECV UranusSC.UranusMJ2000Eq.INC UranusSC.UranusMJ2000Eq.RA UranusSC.UranusMJ2000Eq.RAAN;
GMAT Uranus_Report.WriteHeaders = Off;
For OutputStepSize = 1:432;
	Propagate   RKV89(UranusSC, {UranusSC.ElapsedSecs = 600});
      Report      Uranus_Report UranusSC.A1ModJulian UranusSC.UranusMJ2000Eq.X UranusSC.UranusMJ2000Eq.Y UranusSC.UranusMJ2000Eq.Z UranusSC.UranusMJ2000Eq.VX UranusSC.UranusMJ2000Eq.VY UranusSC.UranusMJ2000Eq.VZ UranusSC.UranusMJ2000Eq.VMAG UranusSC.UranusMJ2000Eq.RAV UranusSC.UranusMJ2000Eq.HX UranusSC.UranusMJ2000Eq.HY UranusSC.UranusMJ2000Eq.HZ UranusSC.UranusMJ2000Eq.AOP UranusSC.UranusMJ2000Eq.DEC UranusSC.UranusMJ2000Eq.DECV UranusSC.UranusMJ2000Eq.INC UranusSC.UranusMJ2000Eq.RA UranusSC.UranusMJ2000Eq.RAAN;
EndFor ;
