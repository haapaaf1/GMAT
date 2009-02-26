%% $Id: CbParams_GMAT_Hyperbolic_2Body.m,v 1.4 2007/07/26 19:12:26 edove Exp $

Create Spacecraft Hyper;
GMAT Hyper.J2000BodyName = Earth;
GMAT Hyper.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT Hyper.DisplayStateType = Cartesian;
GMAT Hyper.CoordinateSystem = EarthMJ2000Eq;
GMAT Hyper.X = 12371.791482634855;
GMAT Hyper.Y = 5050.7627227610719;
GMAT Hyper.Z = 5050.762722761071;
GMAT Hyper.VX = -7.9859921512608487;
GMAT Hyper.VY = 2.44520073255755;
GMAT Hyper.VZ = 2.4452007325575495;
GMAT Hyper.Cd = 2.2;
GMAT Hyper.Cr = 1.2;
GMAT Hyper.DragArea = 20;
GMAT Hyper.SRPArea = 20;
GMAT Hyper.DryMass = 1000;
GMAT Hyper.TotalMass = 1000;

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

Create ReportFile Hyper_Report
GMAT Hyper_Report.Filename = ./output/AcceptTest/CbParams_GMAT_Hyperbolic_2Body.report;
GMAT Hyper_Report.Precision = 16;
GMAT Hyper_Report.WriteHeaders = On;
GMAT Hyper_Report.ColumnWidth = 20;

% ==== Remove once MA and Orbit Period for 
% Hyperbolic Orbits gets fixed ===============
Create Variable MeanAnom;
GMAT MeanAnom.Expression = 0.0;

Create Variable OrbPer;
GMAT OrbPer.Expression = 0.0;

Create Variable OutputStepSize;

% ============================================

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Hyper_Report Hyper.A1ModJulian Hyper.Earth.Altitude Hyper.Earth.BetaAngle Hyper.Earth.C3Energy Hyper.Earth.ECC Hyper.Earth.Latitude Hyper.Earth.Longitude Hyper.Earth.HMAG MeanAnom Hyper.Earth.MM OrbPer Hyper.Earth.RadApo Hyper.Earth.RadPer Hyper.Earth.RMAG Hyper.Earth.SMA Hyper.Earth.TA Hyper.Earth.SemilatusRectum Hyper.Earth.VelApoapsis Hyper.Earth.VelPeriapsis Hyper.Earth.MHA Hyper.Earth.LST;
GMAT Hyper_Report.WriteHeaders = Off;
For OutputStepSize = 1:144;
	Propagate   RKV89(Hyper, {Hyper.ElapsedSecs = 600});
        Report      Hyper_Report Hyper.A1ModJulian Hyper.Earth.Altitude Hyper.Earth.BetaAngle Hyper.Earth.C3Energy Hyper.Earth.ECC Hyper.Earth.Latitude Hyper.Earth.Longitude Hyper.Earth.HMAG MeanAnom Hyper.Earth.MM OrbPer Hyper.Earth.RadApo Hyper.Earth.RadPer Hyper.Earth.RMAG Hyper.Earth.SMA Hyper.Earth.TA Hyper.Earth.SemilatusRectum Hyper.Earth.VelApoapsis Hyper.Earth.VelPeriapsis Hyper.Earth.MHA Hyper.Earth.LST;
EndFor ;
