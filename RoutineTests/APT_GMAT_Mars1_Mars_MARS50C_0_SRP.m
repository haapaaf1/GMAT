% GMAT Script File
% GMAT Release Build 5.0, August 2005

Create Spacecraft MarsSC;
GMAT MarsSC.DateFormat = UTCGregorian
GMAT MarsSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT MarsSC.CoordinateSystem = MarsMJ2000Eq;
GMAT MarsSC.DisplayStateType = Keplerian;
GMAT MarsSC.AnomalyType = TA;
GMAT MarsSC.SMA = 4603;
GMAT MarsSC.ECC = 0.2;
GMAT MarsSC.INC = 45;
GMAT MarsSC.RAAN = 90;
GMAT MarsSC.AOP = 45;
GMAT MarsSC.TA = 45;
GMAT MarsSC.Cd = 2.2;
GMAT MarsSC.Cr = 1.2;
GMAT MarsSC.DragArea = 20;
GMAT MarsSC.SRPArea = 20;
GMAT MarsSC.DryMass = 1000;

Create ForceModel MARS50C;
GMAT MARS50C.CentralBody = Mars;
GMAT MARS50C.PrimaryBodies = {Mars};
GMAT MARS50C.Drag = None;
GMAT MARS50C.SRP = On;
GMAT MARS50C.SRP.Flux_Pressure = 4.53443218374393e-006;
GMAT MARS50C.ErrorControl = RSSStep;
GMAT MARS50C.Gravity.Mars.PotentialFile = Mars50c.cof;
GMAT MARS50C.Gravity.Mars.Degree = 20;
GMAT MARS50C.Gravity.Mars.Order = 20;

Create Propagator RKV89;
GMAT RKV89.FM = MARS50C;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 5;
GMAT RKV89.MaxStep = 5;
GMAT RKV89.MaxStepAttempts = 50; 
GMAT RKV89.StopIfAccuracyIsViolated = false;

GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem MarsMJ2000Eq;
GMAT MarsMJ2000Eq.Origin = Mars;
GMAT MarsMJ2000Eq.J2000Body = Earth;
GMAT MarsMJ2000Eq.Axes = MJ2000Eq;

Create ReportFile Mars_Report
GMAT Mars_Report.Filename =  APT_GMAT_Mars1_Mars_MARS50C_0_SRP.report;
GMAT Mars_Report.Precision = 16;
GMAT Mars_Report.WriteHeaders = Off;
GMAT Mars_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

BeginMissionSequence;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Mars_Report MarsSC.A1ModJulian MarsSC.MarsMJ2000Eq.X MarsSC.MarsMJ2000Eq.Y MarsSC.MarsMJ2000Eq.Z MarsSC.MarsMJ2000Eq.VX MarsSC.MarsMJ2000Eq.VY MarsSC.MarsMJ2000Eq.VZ;
%%%For OutputStepSize = 1:864;
For OutputStepSize = 1:3;
	Propagate   RKV89(MarsSC, {MarsSC.ElapsedSecs = 300});
	Report      Mars_Report MarsSC.A1ModJulian MarsSC.MarsMJ2000Eq.X MarsSC.MarsMJ2000Eq.Y MarsSC.MarsMJ2000Eq.Z MarsSC.MarsMJ2000Eq.VX MarsSC.MarsMJ2000Eq.VY MarsSC.MarsMJ2000Eq.VZ;
EndFor ;

