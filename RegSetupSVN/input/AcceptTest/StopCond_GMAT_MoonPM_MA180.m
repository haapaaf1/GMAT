%% $Id: StopCond_GMAT_MoonPM_MA180.m,v 1.3 2007/07/26 19:12:31 edove Exp $

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = UTCGregorian;
GMAT DefaultSC.Epoch = 01 Jun 2004 12:00:00.000;
GMAT DefaultSC.CoordinateSystem = MoonMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = -1486.792117191545200;
GMAT DefaultSC.Y = 0.0;
GMAT DefaultSC.Z = 1486.792117191543000;
GMAT DefaultSC.VX = -0.142927729144255;
GMAT DefaultSC.VY = -1.631407624437537;
GMAT DefaultSC.VZ = 0.142927729144255;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Luna;
GMAT DefaultProp_ForceModel.PointMasses = {Luna};
GMAT DefaultProp_ForceModel.Drag = None;
GMAT DefaultProp_ForceModel.SRP = Off;
GMAT DefaultProp_ForceModel.ErrorControl = RSSStep;

Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 60;
GMAT DefaultProp.Accuracy = 1.0e-013;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;

Create CoordinateSystem MoonMJ2000Eq;
GMAT MoonMJ2000Eq.Origin = Luna;
GMAT MoonMJ2000Eq.J2000Body = Earth;
GMAT MoonMJ2000Eq.Axes = MJ2000Eq;
GMAT MoonMJ2000Eq.Epoch = 21545;
GMAT MoonMJ2000Eq.UpdateInterval = 60;

Create ReportFile StopCond_Report
GMAT StopCond_Report.Filename = ./output/AcceptTest/StopCond_GMAT_MoonPM_MA180.report;
GMAT StopCond_Report.Precision = 16;
GMAT StopCond_Report.WriteHeaders = On;
GMAT StopCond_Report.ColumnWidth = 20;

Propagate DefaultProp(DefaultSC, {DefaultSC.Luna.MA = 180});
Report StopCond_Report DefaultSC.A1ModJulian DefaultSC.Luna.MA;
