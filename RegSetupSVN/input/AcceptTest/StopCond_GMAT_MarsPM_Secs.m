%% $Id: StopCond_GMAT_MarsPM_Secs.m,v 1.3 2007/07/26 19:12:31 edove Exp $

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = UTCGregorian;
GMAT DefaultSC.Epoch = 01 Jun 2004 12:00:00.000;
GMAT DefaultSC.CoordinateSystem = MarsMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = -2737.481646173082000;
GMAT DefaultSC.Y = 0.0;
GMAT DefaultSC.Z = 2737.481646173082000;
GMAT DefaultSC.VX = -0.311321695052649;
GMAT DefaultSC.VY = -3.553492313930950;
GMAT DefaultSC.VZ = 0.311321695052650;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Mars;
GMAT DefaultProp_ForceModel.PointMasses = {Mars};
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

Create CoordinateSystem MarsMJ2000Eq;
GMAT MarsMJ2000Eq.Origin = Mars;
GMAT MarsMJ2000Eq.J2000Body = Earth;
GMAT MarsMJ2000Eq.Axes = MJ2000Eq;
GMAT MarsMJ2000Eq.Epoch = 21545;
GMAT MarsMJ2000Eq.UpdateInterval = 60;

Create ReportFile StopCond_Report
GMAT StopCond_Report.Filename = ./output/AcceptTest/StopCond_GMAT_MarsPM_Secs.report;
GMAT StopCond_Report.Precision = 16;
GMAT StopCond_Report.WriteHeaders = On;
GMAT StopCond_Report.ColumnWidth = 20;

Propagate DefaultProp(DefaultSC, {DefaultSC.ElapsedSecs = 3600.0});
Report StopCond_Report DefaultSC.A1ModJulian DefaultSC.ElapsedSecs;
