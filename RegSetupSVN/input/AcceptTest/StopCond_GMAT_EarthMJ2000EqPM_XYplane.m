%% $Id: StopCond_GMAT_EarthMJ2000EqPM_XYplane.m,v 1.3 2007/07/26 19:12:31 edove Exp $

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = UTCGregorian;
GMAT DefaultSC.Epoch = 01 Jun 2004 12:00:00.000;
GMAT DefaultSC.CoordinateSystem = EarthMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = -8043.9600382977915;
GMAT DefaultSC.Y = -1564.9950345568864;
GMAT DefaultSC.Z = 3750.9601677510364;
GMAT DefaultSC.VX = 0.99861303787927636;
GMAT DefaultSC.VY = -6.8834168529193462;
GMAT DefaultSC.VZ = -0.46566090709653452;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Earth;
GMAT DefaultProp_ForceModel.PointMasses = {Earth};
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

Create ReportFile StopCond_Report
GMAT StopCond_Report.Filename = ./output/AcceptTest/StopCond_GMAT_EarthMJ2000EqPM_XYplane.report;
GMAT StopCond_Report.Precision = 16;
GMAT StopCond_Report.WriteHeaders = On;
GMAT StopCond_Report.ColumnWidth = 20;

Propagate DefaultProp(DefaultSC, {DefaultSC.EarthMJ2000Eq.Z = 0.0});
Report StopCond_Report DefaultSC.A1ModJulian DefaultSC.EarthMJ2000Eq.Z;
