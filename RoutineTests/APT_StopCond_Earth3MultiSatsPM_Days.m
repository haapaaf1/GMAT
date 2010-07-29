% REVISION HISTORY
% $Id: APT_StopCond_Earth3MultiSatsPM_Days.m,v 1.1 2008/09/26 17:03:53 ljun Exp $

Create Spacecraft SC1;
GMAT SC1.DateFormat = UTCGregorian;
GMAT SC1.Epoch = 01 Jun 2004 12:00:00.000;
GMAT SC1.CoordinateSystem = EarthMJ2000Eq;
GMAT SC1.DisplayStateType = Cartesian;
GMAT SC1.X = -8043.9600382977915;
GMAT SC1.Y = -1564.9950345568864;
GMAT SC1.Z = 3750.9601677510364;
GMAT SC1.VX = 0.99861303787927636;
GMAT SC1.VY = -6.8834168529193462;
GMAT SC1.VZ = -0.46566090709653452;
GMAT SC1.DryMass = 850;
GMAT SC1.Cd = 2.2;
GMAT SC1.Cr = 1.8;
GMAT SC1.DragArea = 15;
GMAT SC1.SRPArea = 1;

Create Spacecraft SC2;
GMAT SC2.DateFormat = UTCGregorian;
GMAT SC2.Epoch = 01 Jun 2004 12:00:00.000;
GMAT SC2.CoordinateSystem = EarthMJ2000Eq;
GMAT SC2.DisplayStateType = Cartesian;
GMAT SC2.X = -8128.5430706081916;
GMAT SC2.Y = -784.67313183490523;
GMAT SC2.Z = 3790.4018834673502;
GMAT SC2.VX = 0.50121378872631495;
GMAT SC2.VY = -6.9556702908636368;
GMAT SC2.VZ = -0.23371982805596253;
GMAT SC2.DryMass = 850;
GMAT SC2.Cd = 2.2;
GMAT SC2.Cr = 1.8;
GMAT SC2.DragArea = 15;
GMAT SC2.SRPArea = 1;

Create Spacecraft SC3;
GMAT SC3.DateFormat = UTCGregorian;
GMAT SC3.Epoch = 01 Jun 2004 12:00:00.000;
GMAT SC3.CoordinateSystem = EarthMJ2000Eq;
GMAT SC3.DisplayStateType = Cartesian;
GMAT SC3.X = -8156.7700833298486;
GMAT SC3.Y = -2.9462884058947938e-012;
GMAT SC3.Z = 3803.5643556663008;
GMAT SC3.VX = 1.9945360173593636e-015;
GMAT SC3.VY = -6.9798160247164711;
GMAT SC3.VZ = -1.1293562855012126e-015;
GMAT SC3.DryMass = 850;
GMAT SC3.Cd = 2.2;
GMAT SC3.Cr = 1.8;
GMAT SC3.DragArea = 15;
GMAT SC3.SRPArea = 1;

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
%%GMAT StopCond_Report.Filename = ./output/SystemTest/StopCond_Earth3MultiSatsPM_Days.report;
GMAT StopCond_Report.Filename = APT_StopCond_Earth3MultiSatsPM_Days.report;
GMAT StopCond_Report.Precision = 16;
GMAT StopCond_Report.WriteHeaders = On;
GMAT StopCond_Report.ColumnWidth = 20;


BeginMissionSequence;

Propagate Synchronized DefaultProp(SC1, SC2, {SC1.ElapsedDays = 0.2}) DefaultProp(SC3, {SC3.ElapsedDays = 0.25});
Report StopCond_Report SC1.A1ModJulian SC1.A1ModJulian SC2.A1ModJulian SC3.A1ModJulian;

