%% $Id: GMAT_VenusSwing_Venus_0_0_0.m,v 1.4 2007/07/26 19:12:29 edove Exp $

Create Spacecraft VenusSC;
GMAT VenusSC.DateFormat = UTCGregorian
GMAT VenusSC.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT VenusSC.CoordinateSystem = VenusMJ2000Eq;
GMAT VenusSC.DisplayStateType = Cartesian;
GMAT VenusSC.X = -5602.4935359507799;
GMAT VenusSC.Y = -287865.06438281387;
GMAT VenusSC.Z = -115233.21201014519;
GMAT VenusSC.VX = 0.26506943401667638;
GMAT VenusSC.VY = 2.6472726534159028;
GMAT VenusSC.VZ = 0.97253039185700274;
GMAT VenusSC.Cd = 2.2;
GMAT VenusSC.Cr = 1.2;
GMAT VenusSC.DragArea = 20;
GMAT VenusSC.SRPArea = 20;
GMAT VenusSC.DryMass = 1000;

Create ForceModel Venus4Body;
GMAT Venus4Body.CentralBody = Venus;
GMAT Venus4Body.PointMasses = {Venus};
GMAT Venus4Body.Drag = None;
GMAT Venus4Body.SRP = Off;
GMAT Venus4Body.ErrorControl = RSSStep;

Create Propagator VenusRKV89;
GMAT VenusRKV89.FM = Venus4Body;
GMAT VenusRKV89.Type = RungeKutta89;
GMAT VenusRKV89.InitialStepSize = 60;
GMAT VenusRKV89.Accuracy = 1e-013;
GMAT VenusRKV89.MinStep = 0.1;
GMAT VenusRKV89.MaxStep = 60;
GMAT VenusRKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create OpenGLPlot OpenGLPlot1;
GMAT OpenGLPlot1.Add = {VenusSC, Venus};
GMAT OpenGLPlot1.CoordinateSystem = VenusMJ2000Eq;
GMAT OpenGLPlot1.ViewPointRef = Venus;
GMAT OpenGLPlot1.ViewPointVector = Vector;
GMAT OpenGLPlot1.ViewDirection = Venus;
GMAT OpenGLPlot1.ViewScaleFactor = 20;
GMAT OpenGLPlot1.FixedFovAngle = 45;
GMAT OpenGLPlot1.ViewUpCoordinateSystem = VenusMJ2000Eq;
GMAT OpenGLPlot1.ViewUpAxis = X;
GMAT OpenGLPlot1.CelestialPlane = Off;
GMAT OpenGLPlot1.XYPlane = On;
GMAT OpenGLPlot1.WireFrame = Off;
GMAT OpenGLPlot1.TargetStatus = Off;
GMAT OpenGLPlot1.Axes = On;
GMAT OpenGLPlot1.Grid = Off;
GMAT OpenGLPlot1.EarthSunLines = Off;
GMAT OpenGLPlot1.UseInitialView = On;
GMAT OpenGLPlot1.PerspectiveMode = Off;
GMAT OpenGLPlot1.UseFixedFov = Off;

Create CoordinateSystem VenusMJ2000Eq;
GMAT VenusMJ2000Eq.Origin = Venus;
GMAT VenusMJ2000Eq.J2000Body = Earth;
GMAT VenusMJ2000Eq.Axes = MJ2000Eq;
GMAT VenusMJ2000Eq.Epoch = 21545;
GMAT VenusMJ2000Eq.UpdateInterval = 60;

Create ReportFile Venus_Report
GMAT Venus_Report.Filename = ./output/AcceptTest/GMAT_VenusSwing_Venus_0_0_0.report;
GMAT Venus_Report.Precision = 16;
GMAT Venus_Report.WriteHeaders = Off;
GMAT Venus_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report Venus_Report VenusSC.A1ModJulian VenusSC.VenusMJ2000Eq.X VenusSC.VenusMJ2000Eq.Y VenusSC.VenusMJ2000Eq.Z VenusSC.VenusMJ2000Eq.VX VenusSC.VenusMJ2000Eq.VY VenusSC.VenusMJ2000Eq.VZ;
For OutputStepSize = 1:432;
	Propagate   VenusRKV89(VenusSC, {VenusSC.ElapsedSecs = 300});
	Report      Venus_Report VenusSC.A1ModJulian VenusSC.VenusMJ2000Eq.X VenusSC.VenusMJ2000Eq.Y VenusSC.VenusMJ2000Eq.Z VenusSC.VenusMJ2000Eq.VX VenusSC.VenusMJ2000Eq.VY VenusSC.VenusMJ2000Eq.VZ;
EndFor ;
