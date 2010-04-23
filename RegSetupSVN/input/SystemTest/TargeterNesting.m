%% $Id: TargeterNesting.m,v 1.7 2008/10/14 18:29:20 edove Exp $
%% GMAT System Test Script File
%
% This test script is designed to test the following elements:
%
% 1. Nesting inside of a Targeter 
%
% The output file format follows the guidlines documented in
% the GMAT System Test Plan.
%
% External dependencies: None
%
% Output data generated should be identical from one build to 
% the next.


%----------------------------------------
%---------- Spacecrafts
%----------------------------------------

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = TAIModJulian;
GMAT DefaultSC.Epoch = 21545;
GMAT DefaultSC.CoordinateSystem = EarthMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = 7100;
GMAT DefaultSC.Y = 0;
GMAT DefaultSC.Z = 1300;
GMAT DefaultSC.VX = 0;
GMAT DefaultSC.VY = 7.35;
GMAT DefaultSC.VZ = 1;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;

Create Spacecraft BeginState;
GMAT BeginState.DateFormat = TAIModJulian;
GMAT BeginState.Epoch = 21545;
GMAT BeginState.CoordinateSystem = EarthMJ2000Eq;
GMAT BeginState.DisplayStateType = Cartesian;
GMAT BeginState.X = 7100;
GMAT BeginState.Y = 0;
GMAT BeginState.Z = 1300;
GMAT BeginState.VX = 0;
GMAT BeginState.VY = 7.35;
GMAT BeginState.VZ = 1;
GMAT BeginState.DryMass = 850;
GMAT BeginState.Cd = 2.2;
GMAT BeginState.Cr = 1.8;
GMAT BeginState.DragArea = 15;
GMAT BeginState.SRPArea = 1;

Create Spacecraft DummySat;
Create Spacecraft DummySat2;
Create Spacecraft DummySat3;
Create Spacecraft DummySat4;


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
%---------- Propagators
%----------------------------------------

Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 60;
GMAT DefaultProp.Accuracy = 9.999999999999999e-012;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;


%----------------------------------------
%---------- Burns
%----------------------------------------

Create ImpulsiveBurn DefaultIB;
GMAT DefaultIB.Origin = Earth;
GMAT DefaultIB.Axes = VNB;
GMAT DefaultIB.VectorFormat = Cartesian;
GMAT DefaultIB.Element1 = 0;
GMAT DefaultIB.Element2 = 0;
GMAT DefaultIB.Element3 = 0;

Create ImpulsiveBurn IB2;
GMAT IB2.Origin = Earth;
GMAT IB2.Axes = VNB;
GMAT IB2.VectorFormat = Cartesian;
GMAT IB2.Element1 = 0;
GMAT IB2.Element2 = 0;
GMAT IB2.Element3 = 0;

%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable StartEpoch;
Create Variable var;
Create Variable dummy;
Create Variable case;
Create Variable I;

%----------------------------------------
%---------- Solvers
%----------------------------------------

Create DifferentialCorrector DC1;
GMAT DC1.ShowProgress = true;
GMAT DC1.ReportStyle = Normal;
GMAT DC1.TargeterTextFile = 'DifferentialCorrectorDC1.data';
GMAT DC1.MaximumIterations = 25;
GMAT DC1.UseCentralDifferences = false;

Create DifferentialCorrector DC2;
GMAT DC2.ShowProgress = true;
GMAT DC2.ReportStyle = Normal;
GMAT DC2.TargeterTextFile = 'DifferentialCorrectorDC2.data';
GMAT DC2.MaximumIterations = 25;
GMAT DC2.UseCentralDifferences = false;


%----------------------------------------
%---------- Plot(s)/Report(s)
%----------------------------------------

Create ReportFile ReportFile1;
GMAT ReportFile1.Filename = './output/SystemTest/TargeterNesting.report';
GMAT ReportFile1.Precision = 15;
GMAT ReportFile1.WriteHeaders = On;
GMAT ReportFile1.LeftJustify = On;
GMAT ReportFile1.ZeroFill = Off;
GMAT ReportFile1.ColumnWidth = 20;
GMAT ReportFile1.SolverIterations = None;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------

BeginScript
   GMAT BeginState = DefaultSC;
   GMAT StartEpoch = DefaultSC.A1ModJulian;

   GMAT dummy = 2;
   GMAT case  = 0;
   GMAT var   = 2;
EndScript;

% ====================================
% Case 1
% Targeter sequence with a 
% BeginScript/EndScript nested inside
% ====================================
GMAT case = case + 1;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   BeginScript;
      Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
      Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
   EndScript;
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
GMAT ReportFile1.WriteHeaders = Off;

% ====================================
% Case 2
% Targeter sequence with an 
% If and IfElse nested inside
% ====================================
GMAT case = case + 1;

GMAT DefaultSC = BeginState;
GMAT StartEpoch = DefaultSC.A1ModJulian;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   If 2 > 1
   Else;
      Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
   EndIf;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   If 2 < 1
      Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
   EndIf;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;

% ====================================
% Case 3
% Targeter sequence with a 
% For nested inside
% ====================================
GMAT case = case + 1;

GMAT DefaultSC = BeginState;
GMAT StartEpoch = DefaultSC.A1ModJulian;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;

   For I = 1:2
      var = I;
   EndFor;

   Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;

% ====================================
% Case 4
% Targeter sequence with a 
% While nested inside
% ====================================
GMAT case = case + 1;
GMAT var = 0;

GMAT DefaultSC = BeginState;
GMAT StartEpoch = DefaultSC.A1ModJulian;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   While var < 2
      var = var + 1;
   EndWhile; 
   Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;

% ====================================
% Case 5
% Targeter sequence with another 
% Targeter nested inside
% ====================================
GMAT case = case + 1;

GMAT DefaultSC = BeginState;
GMAT StartEpoch = DefaultSC.A1ModJulian;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Target DC1;
      Vary DC1(IB2.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver IB2(DummySat);
      Propagate DefaultProp(DummySat) {DummySat.Earth.TA = 0};
      Achieve DC1(DummySat.Earth.ECC = 0.03, {Tolerance = 1e-006});
   EndTarget;  % For targeter DC1
   Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA DummySat.Earth.ECC;

% ====================================
% Case 6
% Targeter nested inside an If
% ====================================
GMAT case = case + 1;

If 2 > 1;
   GMAT DefaultSC = BeginState;
   GMAT StartEpoch = DefaultSC.A1ModJulian;

   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Target DC1;
      Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
      Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
   EndTarget;  % For targeter DC1
   Target DC2;
      While var < 2
         GMAT var = var + 1;
      EndWhile; 
      Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
      Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
   EndTarget;  % For targeter DC2
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
EndIf;

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;

% ====================================
% Case 7
% Targeter nested inside a For
% ====================================
GMAT case = case + 1;

For I = 1:2;
   GMAT DefaultSC = BeginState;
   GMAT StartEpoch = DefaultSC.A1ModJulian;

   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Target DC1;
      Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
      Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
   EndTarget;  % For targeter DC1
   Target DC2;
      Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
      Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
   EndTarget;  % For targeter DC2
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
EndFor;

Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;

% ====================================
% Case 8
% Targeter nested inside a While
% ====================================
GMAT case = case + 1;
GMAT var = 0;

GMAT DefaultSC = BeginState;
GMAT StartEpoch = DefaultSC.A1ModJulian;
While var < 2
   GMAT DefaultSC = BeginState;
   GMAT StartEpoch = DefaultSC.A1ModJulian;

   GMAT var = var + 1;
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Target DC1;
      Vary DC1(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
      Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
   EndTarget;  % For targeter DC1
   Target DC2;
      Vary DC2(DefaultIB.Element1 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
      Maneuver DefaultIB(DefaultSC);
      Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
      Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
   EndTarget;  % For targeter DC2
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

   Report ReportFile1 case var DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndWhile;