%% $Id: EpochFormatCheck.m,v 1.3 2007/07/26 19:13:11 edove Exp $
%% GMAT System Test Script File
%
% This test script is designed to test the following elements:
%
% 1. Output of all of the available Epoch Formats
%
% The output file format follows the guidlines documented in
% the GMAT System Test Plan.
%
% External dependencies: None
%
% Output data generated should be identical from one build to 
% the next.

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = TAIModJulian;
GMAT DefaultSC.Epoch = 21545;
GMAT DefaultSC.CoordinateSystem = EarthMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = 7100;
GMAT DefaultSC.Y = 0;
GMAT DefaultSC.Z = 1300;
GMAT DefaultSC.VX = 0;
GMAT DefaultSC.VY = 7.3499999999999996;
GMAT DefaultSC.VZ = 1;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2000000000000002;
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

Create ReportFile EpochReport
GMAT EpochReport.Filename = './output/SystemTest/EpochFormatCheck.report';
GMAT EpochReport.Precision = 12;
GMAT EpochReport.ColumnWidth = 20;

Report EpochReport DefaultSC.A1ModJulian;
Propagate DefaultProp(DefaultSC, {DefaultSC.ElapsedDays = 1});
Report EpochReport DefaultSC.A1Gregorian;
Report EpochReport DefaultSC.TAIModJulian;
Report EpochReport DefaultSC.TAIGregorian;
Report EpochReport DefaultSC.TCBModJulian;
Report EpochReport DefaultSC.TCBGregorian;
Report EpochReport DefaultSC.TDBModJulian;
Report EpochReport DefaultSC.TDBGregorian;
Report EpochReport DefaultSC.TTModJulian;
Report EpochReport DefaultSC.TTGregorian;
Report EpochReport DefaultSC.UTCModJulian;
Report EpochReport DefaultSC.UTCGregorian;