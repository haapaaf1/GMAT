%% $Id: allPropagates.m,v 1.4 2007/07/26 19:13:11 edove Exp $
%% GMAT System Test Script File
%
% This test script is designed to test the following elements:
%
% 1. All variations of the Propagate Command
%
% The output file format follows the guidlines documented in
% the GMAT System Test Plan.
%
% External dependencies: None
%
% Output data generated should be identical in the case, 
% A1Modulian, and ElapsedSecs column from one build to 
% the next.

%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = A1ModJulian;
GMAT DefaultSC.Epoch = 21545;
GMAT DefaultSC.CoordinateSystem = EarthMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Keplerian;
GMAT DefaultSC.SMA = 8500;
GMAT DefaultSC.ECC = 0.02;
GMAT DefaultSC.INC = 90.0;
GMAT DefaultSC.RAAN = 307.0;
GMAT DefaultSC.AOP = 314.0;
GMAT DefaultSC.TA = 100;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;

Create Spacecraft Spacecraft1;
GMAT Spacecraft1.DateFormat = A1ModJulian;
GMAT Spacecraft1.Epoch = 21545;
GMAT Spacecraft1.CoordinateSystem = EarthMJ2000Eq;
GMAT Spacecraft1.DisplayStateType = Keplerian;
GMAT Spacecraft1.SMA = 7192;
GMAT Spacecraft1.ECC = 0.025;
GMAT Spacecraft1.INC = 30.0;
GMAT Spacecraft1.RAAN = 307.0;
GMAT Spacecraft1.AOP = 314.0;
GMAT Spacecraft1.TA = 100;
GMAT Spacecraft1.DryMass = 850;
GMAT Spacecraft1.Cd = 2.2;
GMAT Spacecraft1.Cr = 1.8;
GMAT Spacecraft1.DragArea = 15;
GMAT Spacecraft1.SRPArea = 1;

Create Spacecraft Spacecraft2;
GMAT Spacecraft2.DateFormat = A1ModJulian;
GMAT Spacecraft2.Epoch = 21545;
GMAT Spacecraft2.CoordinateSystem = EarthMJ2000Eq;
GMAT Spacecraft2.DisplayStateType = Keplerian;
GMAT Spacecraft2.SMA = 7192;
GMAT Spacecraft2.ECC = 0.025;
GMAT Spacecraft2.INC = 60.0;
GMAT Spacecraft2.RAAN = 307.0;
GMAT Spacecraft2.AOP = 314.0;
GMAT Spacecraft2.TA = 100;
GMAT Spacecraft2.DryMass = 850;
GMAT Spacecraft2.Cd = 2.2;
GMAT Spacecraft2.Cr = 1.8;
GMAT Spacecraft2.DragArea = 15;
GMAT Spacecraft2.SRPArea = 1;

Create Spacecraft Spacecraft3;
GMAT Spacecraft3.DateFormat = A1ModJulian;
GMAT Spacecraft3.Epoch = 21545;
GMAT Spacecraft3.CoordinateSystem = EarthMJ2000Eq;
GMAT Spacecraft3.DisplayStateType = Keplerian;
GMAT Spacecraft3.SMA = 7192;
GMAT Spacecraft3.ECC = 0.025;
GMAT Spacecraft3.INC = 12.85;
GMAT Spacecraft3.RAAN = 307.0;
GMAT Spacecraft3.AOP = 314.0;
GMAT Spacecraft3.TA = 100;
GMAT Spacecraft3.DryMass = 850;
GMAT Spacecraft3.Cd = 2.2;
GMAT Spacecraft3.Cr = 1.8;
GMAT Spacecraft3.DragArea = 15;
GMAT Spacecraft3.SRPArea = 1;

Create Spacecraft Spacecraft4;
GMAT Spacecraft4.DateFormat = A1ModJulian;
GMAT Spacecraft4.Epoch = 21545;
GMAT Spacecraft4.CoordinateSystem = EarthMJ2000Eq;
GMAT Spacecraft4.DisplayStateType = Keplerian;
GMAT Spacecraft4.SMA = 7191.938817629019;
GMAT Spacecraft4.ECC = 0.02454974900598053;
GMAT Spacecraft4.INC = 12.85008005658097;
GMAT Spacecraft4.RAAN = 306.6148021947984;
GMAT Spacecraft4.AOP = 314.1905515359943;
GMAT Spacecraft4.TA = 69.99999999999754;
GMAT Spacecraft4.DryMass = 850;
GMAT Spacecraft4.Cd = 2.2;
GMAT Spacecraft4.Cr = 1.8;
GMAT Spacecraft4.DragArea = 15;
GMAT Spacecraft4.SRPArea = 1;

Create Spacecraft Spacecraft5;
GMAT Spacecraft5.SMA = 9000;

Create Spacecraft Spacecraft6;
GMAT Spacecraft6.SMA = 8750;

Create Spacecraft InitialDSC;
GMAT InitialDSC = DefaultSC;

Create Spacecraft InitialSC1;
GMAT InitialSC1 = Spacecraft1;

Create Spacecraft InitialSC2;
GMAT InitialSC2 = Spacecraft2;

Create Spacecraft InitialSC3;
GMAT InitialSC3 = Spacecraft3;

Create Spacecraft InitialSC4;
GMAT InitialSC4 = Spacecraft4;

Create Spacecraft InitialSC5;
GMAT InitialSC5 = Spacecraft5;

Create Spacecraft InitialSC6;
GMAT InitialSC6 = Spacecraft6;

%----------------------------------------
%---------- Formations
%----------------------------------------

Create Formation Formation1;
GMAT Formation1.A1Epoch = 21545;
GMAT Formation1.Add = {Spacecraft1, Spacecraft2};

Create Formation Formation2;
GMAT Formation2.A1Epoch = 21545;
GMAT Formation2.Add = {Spacecraft3, Spacecraft4};


%----------------------------------------
%---------- ForceModels
%----------------------------------------

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Earth;
GMAT DefaultProp_ForceModel.PointMasses = {Earth};
GMAT DefaultProp_ForceModel.Drag = None;
GMAT DefaultProp_ForceModel.SRP = Off;
GMAT DefaultProp_ForceModel.ErrorControl = RSSStep;

%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable case;
GMAT case = 0;
Create Variable initialEpoch;
GMAT initialEpoch = DefaultSC.A1ModJulian;

%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 0.864;
GMAT DefaultProp.Accuracy = 1e-13;
GMAT DefaultProp.MinStep = 1e-15;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;

Create Propagator DefaultProp2;
GMAT DefaultProp2.FM = DefaultProp_ForceModel;
GMAT DefaultProp2.Type = RungeKutta89;
GMAT DefaultProp2.InitialStepSize = 8.64;
GMAT DefaultProp2.Accuracy = 1e-13;
GMAT DefaultProp2.MinStep = 1e-15;
GMAT DefaultProp2.MaxStep = 2700;
GMAT DefaultProp2.MaxStepAttempts = 50;

%----------------------------------------
%---------- Subscribers
%----------------------------------------

Create ReportFile ReportFile1;
GMAT ReportFile1.Filename = './output/SystemTest/allPropagates.report';
GMAT ReportFile1.Precision = 15;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------

% ====================================
% Case 1
% Forward propagate 1 SC by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(DefaultSC);
Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.ElapsedSecs;
GMAT ReportFile1.WriteHeaders = Off;

% ====================================
% Case 2
% Backward propagate 1 Formation by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate BackProp DefaultProp(Formation1);
Report ReportFile1 case Spacecraft1.A1ModJulian Spacecraft1.ElapsedSecs;
Report ReportFile1 case Spacecraft2.A1ModJulian Spacecraft2.ElapsedSecs;

% ====================================
% Case 3
% Forward propagate 2 sync formations 
% by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch + 0.01;
   GMAT Spacecraft2.A1Epoch = initialEpoch + 0.01;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate Synchronized DefaultProp(Formation1) DefaultProp2(Formation2);
Report ReportFile1 case Spacecraft1.A1ModJulian Spacecraft1.ElapsedSecs;
Report ReportFile1 case Spacecraft2.A1ModJulian Spacecraft2.ElapsedSecs;
Report ReportFile1 case Spacecraft3.A1ModJulian Spacecraft3.ElapsedSecs;
Report ReportFile1 case Spacecraft4.A1ModJulian Spacecraft4.ElapsedSecs;

% ====================================
% Case 4
% Forward sync prop 2 SC by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch + 0.01;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate Synchronized DefaultProp(Spacecraft5) DefaultProp2(Spacecraft6);
Report ReportFile1 case Spacecraft5.A1ModJulian Spacecraft5.ElapsedSecs;
Report ReportFile1 case Spacecraft6.A1ModJulian Spacecraft6.ElapsedSecs;

% ====================================
% Case 5
% Backward sync prop 2 SC by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch + 0.01;
EndScript;

Propagate Synchronized BackProp DefaultProp(Spacecraft5) DefaultProp2(Spacecraft6);
Report ReportFile1 case Spacecraft5.A1ModJulian Spacecraft5.ElapsedSecs;
Report ReportFile1 case Spacecraft6.A1ModJulian Spacecraft6.ElapsedSecs;

% ====================================
% Case 6
% Forward prop 1 formation by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(Formation1);
Report ReportFile1 case Spacecraft1.A1ModJulian Spacecraft1.ElapsedSecs;
Report ReportFile1 case Spacecraft2.A1ModJulian Spacecraft2.ElapsedSecs;

% ====================================
% Case 7
% Forward prop 2 SC by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(Spacecraft5, Spacecraft6);
Report ReportFile1 case Spacecraft5.A1ModJulian Spacecraft5.ElapsedSecs;
Report ReportFile1 case Spacecraft6.A1ModJulian Spacecraft6.ElapsedSecs;

% ====================================
% Case 8
% Forward prop 2 formation by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(Formation1, Formation2);
Report ReportFile1 case Spacecraft1.A1ModJulian Spacecraft1.ElapsedSecs;
Report ReportFile1 case Spacecraft2.A1ModJulian Spacecraft2.ElapsedSecs;
Report ReportFile1 case Spacecraft3.A1ModJulian Spacecraft3.ElapsedSecs;
Report ReportFile1 case Spacecraft4.A1ModJulian Spacecraft4.ElapsedSecs;

% ====================================
% Case 9
% Forward prop 1 formation and 1 SC
% by initial step size
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(Formation1, Spacecraft6);
Report ReportFile1 case Spacecraft1.A1ModJulian Spacecraft1.ElapsedSecs;
Report ReportFile1 case Spacecraft2.A1ModJulian Spacecraft2.ElapsedSecs;
Report ReportFile1 case Spacecraft6.A1ModJulian Spacecraft6.ElapsedSecs;

% ====================================
% Case 10
% Forward prop 1 formation by defined time
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(Formation1) {Spacecraft1.ElapsedSecs = 8640.0};
Report ReportFile1 case Spacecraft1.A1ModJulian Spacecraft1.ElapsedSecs;
Report ReportFile1 case Spacecraft2.A1ModJulian Spacecraft2.ElapsedSecs;

% ====================================
% Case 11
% Forward prop 1 SC by defined time
% or SC parameter goal
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC   = InitialDSC;
   GMAT Spacecraft1 = InitialSC1;
   GMAT Spacecraft2 = InitialSC2;
   GMAT Spacecraft3 = InitialSC3;
   GMAT Spacecraft4 = InitialSC4;
   GMAT Spacecraft5 = InitialSC5;
   GMAT Spacecraft6 = InitialSC6;
   GMAT DefaultSC.A1Epoch = initialEpoch;
   GMAT Spacecraft1.A1Epoch = initialEpoch;
   GMAT Spacecraft2.A1Epoch = initialEpoch;
   GMAT Spacecraft3.A1Epoch = initialEpoch;
   GMAT Spacecraft4.A1Epoch = initialEpoch;
   GMAT Spacecraft5.A1Epoch = initialEpoch;
   GMAT Spacecraft6.A1Epoch = initialEpoch;
EndScript;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.ECC = 2, DefaultSC.ElapsedSecs = 8640.0};
Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.ElapsedSecs;