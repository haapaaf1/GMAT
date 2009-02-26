%% $Id: TargeterCheck.m,v 1.8 2008/10/14 18:29:20 edove Exp $
%% GMAT System Test Script File
%
% This test script is designed to test the following elements:
%
% 1. The use of numbers, parameters, arrays, and variables 
%   inside of the Target Command's Achieve and Vary Commands. 
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


%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable StartEpoch;
Create Variable case;
Create Array array[11,1];
Create Array arrayMod[11,1];
Create Variable var1;
Create Variable var2;
Create Variable var3;
Create Variable var4;
Create Variable var5;
Create Variable var6;
Create Variable var7;
Create Variable var8;
Create Variable var9;
Create Variable var10;
Create Variable var11;
Create Variable varMod3;
Create Variable varMod10;
Create Variable varMod11;

%----------------------------------------
%---------- Coordinate Systems
%----------------------------------------

Create CoordinateSystem EarthMJ2000Eq;
GMAT EarthMJ2000Eq.Origin = Earth;
GMAT EarthMJ2000Eq.Axes = MJ2000Eq;
GMAT EarthMJ2000Eq.UpdateInterval = 60;
GMAT EarthMJ2000Eq.OverrideOriginInterval = false;

Create CoordinateSystem EarthMJ2000Ec;
GMAT EarthMJ2000Ec.Origin = Earth;
GMAT EarthMJ2000Ec.Axes = MJ2000Ec;
GMAT EarthMJ2000Ec.UpdateInterval = 60;
GMAT EarthMJ2000Ec.OverrideOriginInterval = false;

Create CoordinateSystem EarthFixed;
GMAT EarthFixed.Origin = Earth;
GMAT EarthFixed.Axes = BodyFixed;
GMAT EarthFixed.UpdateInterval = 60;
GMAT EarthFixed.OverrideOriginInterval = false;


%----------------------------------------
%---------- Solvers
%----------------------------------------

Create DifferentialCorrector DC1;
GMAT DC1.ShowProgress = true;
GMAT DC1.ReportStyle = Normal;
GMAT DC1.TargeterTextFile = DifferentialCorrectorDC1.data;
GMAT DC1.MaximumIterations = 25;
GMAT DC1.UseCentralDifferences = false;

Create DifferentialCorrector DC2;
GMAT DC2.ShowProgress = true;
GMAT DC2.ReportStyle = Normal;
GMAT DC2.TargeterTextFile = DifferentialCorrectorDC2.data;
GMAT DC2.MaximumIterations = 25;
GMAT DC2.UseCentralDifferences = false;


%----------------------------------------
%---------- Plot/Report
%----------------------------------------

% Create OpenGLPlot OpenGLPlot1;
% GMAT OpenGLPlot1.Add = {DefaultSC, Earth};
% GMAT OpenGLPlot1.ViewScaleFactor = 5;
% GMAT OpenGLPlot1.ViewUpAxis = X;

Create ReportFile ReportFile1;
GMAT ReportFile1.Filename = ./output/SystemTest/TargeterCheck.report;
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

   GMAT case  = 0;

   GMAT var1  = 0.00001;
   GMAT var2  = 1e-006;
   GMAT var3  = 0.5;
   GMAT var4  = 1e-005;
   GMAT var5  = 50;
   GMAT var6  = -5;
   GMAT var7  = 5;
   GMAT var8  = 0;
   GMAT var9  = 1;
   GMAT var10 = 23922;
   GMAT var11 = 0.02;

   GMAT array(1,1)  = 0.00001;
   GMAT array(2,1)  = 1e-006;
   GMAT array(3,1)  = 0.5;
   GMAT array(4,1)  = 1e-005;
   GMAT array(5,1)  = 50;
   GMAT array(6,1)  = -5;
   GMAT array(7,1)  = 5;
   GMAT array(8,1)  = 0;
   GMAT array(9,1)  = 1;
   GMAT array(10,1) = 23922;
   GMAT array(11,1) = 0.02;

   GMAT DummySat.X  = 23922;
   GMAT DummySat.Y  = 0.02;
   GMAT DummySat.Z  = 0.0001;
   GMAT DummySat.VX = 1e-006;
   GMAT DummySat.VY  = 0.5;
   GMAT DummySat.VZ  = 1e-005;
   GMAT DummySat2.X  = 50;
   GMAT DummySat2.Y  = -5;
   GMAT DummySat2.Z  = 5;
   GMAT DummySat2.VX  = 0;
   GMAT DummySat2.VY  = 1;
EndScript;

% ====================================
% Case 1
% Targeter sequence using numbers only
% ====================================
GMAT case = case + 1;

BeginScript;
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
GMAT ReportFile1.WriteHeaders = Off;
EndScript;

% ====================================
% Case 2
% Targeter sequence using Variables on 
% RHS of ='s in Achieve statement
% ====================================
GMAT case = case + 1;

BeginScript
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = var10, {Tolerance = var1});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = var11, {Tolerance = var2});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 3
% Targeter sequence using Variables on 
% RHS of ='s in Achieve and Vary
% ====================================
GMAT case = case + 1;

BeginScript
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = var3, {Perturbation = var4, MaxStep = var5, Lower = var6, Upper = var7, AdditiveScaleFactor = var8, MultiplicativeScaleFactor = var9});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = var10, {Tolerance = var1});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = var3, {Perturbation = var4, MaxStep = var5, Lower = var6, Upper = var7, AdditiveScaleFactor = var8, MultiplicativeScaleFactor = var9});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = var11, {Tolerance = var2});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 4
% Targeter sequence using Arrays on 
% RHS of ='s in Achieve statement
% ====================================
GMAT case = case + 1;

BeginScript
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = array(10,1), {Tolerance = array(1,1)});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = array(11,1), {Tolerance = array(2,1)});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 5
% Targeter sequence using Arrays on 
% RHS of ='s in Achieve and Vary
% ====================================
GMAT case = case + 1;

BeginScript
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = array(3,1), {Perturbation = array(4,1), MaxStep = array(5,1), Lower = array(6,1), Upper = array(7,1), AdditiveScaleFactor = array(8,1), MultiplicativeScaleFactor = array(9,1)});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = array(10,1), {Tolerance = array(1,1)});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = array(3,1), {Perturbation = array(4,1), MaxStep = array(5,1), Lower = array(6,1), Upper = array(7,1), AdditiveScaleFactor = array(8,1), MultiplicativeScaleFactor = array(9,1)});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = array(11,1), {Tolerance = array(2,1)});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 6
% Targeter sequence using SC Parameters 
% on RHS of ='s in Achieve
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = DummySat.X, {Tolerance = DummySat.Z});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = DummySat.Y, {Tolerance = DummySat.VX});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 7
% Targeter sequence using SC Parameters 
% on RHS of ='s in Achieve and Vary
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(DefaultIB.V = DummySat.VY, {Perturbation = DummySat.VZ, MaxStep = DummySat2.X, Lower = DummySat2.Y, Upper = DummySat2.Z, AdditiveScaleFactor = DummySat2.VX, MultiplicativeScaleFactor = DummySat2.VY});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   Achieve DC1(DefaultSC.Earth.RadApo = DummySat.X, {Tolerance = DummySat.Z});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(DefaultIB.V = DummySat.VY, {Perturbation = DummySat.VZ, MaxStep = DummySat2.X, Lower = DummySat2.Y, Upper = DummySat2.Z, AdditiveScaleFactor = DummySat2.VX, MultiplicativeScaleFactor = DummySat2.VY});
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   Achieve DC2(DefaultSC.Earth.ECC = DummySat.Y, {Tolerance = DummySat.VX});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 8
% Targeter sequence 
% Achieve/Vary => Variable = Number
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(varMod3 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT varMod10 = DefaultSC.Earth.RadApo;
   Achieve DC1(varMod10 = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(varMod3 = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT varMod11 = DefaultSC.Earth.ECC;
   Achieve DC2(varMod11 = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 9
% Targeter sequence 
% Achieve/Vary => Variable = Parameter
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(varMod3 = DummySat.VY, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT varMod10 = DefaultSC.Earth.RadApo;
   Achieve DC1(varMod10 = DummySat.X, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(varMod3 = DummySat.VY, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT varMod11 = DefaultSC.Earth.ECC;
   Achieve DC2(varMod11 = DummySat.Y, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 10
% Targeter sequence 
% Achieve/Vary => Variable = Variable
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(varMod3 = var3, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT varMod10 = DefaultSC.Earth.RadApo;
   Achieve DC1(varMod10 = var10, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(varMod3 = var3, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT varMod11 = DefaultSC.Earth.ECC;
   Achieve DC2(varMod11 = var11, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 11
% Targeter sequence 
% Achieve/Vary => Variable = Array
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(varMod3 = array(3,1), {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT varMod10 = DefaultSC.Earth.RadApo;
   Achieve DC1(varMod10 = array(10,1), {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(varMod3 = array(3,1), {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = varMod3;
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT varMod11 = DefaultSC.Earth.ECC;
   Achieve DC2(varMod11 = array(11,1), {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;


% ====================================
% Case 12
% Targeter sequence 
% Achieve/Vary => Array = Number
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(arrayMod(3,1) = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT arrayMod(10,1) = DefaultSC.Earth.RadApo;
   Achieve DC1(arrayMod(10,1) = 23922, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(arrayMod(3,1) = 0.5, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT arrayMod(11,1) = DefaultSC.Earth.ECC;
   Achieve DC2(arrayMod(11,1) = 0.02, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 13
% Targeter sequence 
% Achieve/Vary => Array = Parameter
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(arrayMod(3,1) = DummySat.VY, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT arrayMod(10,1) = DefaultSC.Earth.RadApo;
   Achieve DC1(arrayMod(10,1) = DummySat.X, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(arrayMod(3,1) = DummySat.VY, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT arrayMod(11,1) = DefaultSC.Earth.ECC;
   Achieve DC2(arrayMod(11,1) = DummySat.Y, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 14
% Targeter sequence 
% Achieve/Vary => Array = Variable
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(arrayMod(3,1) = var3, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT arrayMod(10,1) = DefaultSC.Earth.RadApo;
   Achieve DC1(arrayMod(10,1) = var10, {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(arrayMod(3,1) = var3, {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT arrayMod(11,1) = DefaultSC.Earth.ECC;
   Achieve DC2(arrayMod(11,1) = var11, {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;

% ====================================
% Case 15
% Targeter sequence 
% Achieve/Vary => Array = Array
% ====================================
GMAT case = case + 1;

BeginScript;
   GMAT DefaultSC = BeginState;
   GMAT DefaultSC.A1Epoch = StartEpoch;

Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
Target DC1;
   Vary DC1(arrayMod(3,1) = array(3,1), {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};
   GMAT arrayMod(10,1) = DefaultSC.Earth.RadApo;
   Achieve DC1(arrayMod(10,1) = array(10,1), {Tolerance = 0.00001});
EndTarget;  % For targeter DC1
Target DC2;
   Vary DC2(arrayMod(3,1) = array(3,1), {Perturbation = 1e-005, MaxStep = 50, Lower = -5, Upper = 5, AdditiveScaleFactor = 0, MultiplicativeScaleFactor = 1});
   GMAT DefaultIB.V = arrayMod(3,1);
   Maneuver DefaultIB(DefaultSC);
   Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 0};
   GMAT arrayMod(11,1) = DefaultSC.Earth.ECC;
   Achieve DC2(arrayMod(11,1) = array(11,1), {Tolerance = 1e-006});
EndTarget;  % For targeter DC2
Propagate DefaultProp(DefaultSC) {DefaultSC.Earth.TA = 180};

Report ReportFile1 case DefaultSC.A1ModJulian DefaultSC.Earth.ECC DefaultSC.Earth.RadApo DefaultSC.Earth.TA;
EndScript;