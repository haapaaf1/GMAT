% REVISION HISTORY
% $Id: If22.m,v 1.2 2007/07/26 19:13:11 edove Exp $

Create Spacecraft SC1;
GMAT SC1.DateFormat = TAIModJulian;
GMAT SC1.Epoch = 21545;
GMAT SC1.CoordinateSystem = EarthMJ2000Eq;
GMAT SC1.DisplayStateType = Keplerian;
GMAT SC1.SMA = 7191.9388176290076;
GMAT SC1.ECC = 0.024549749005972517;
GMAT SC1.INC = 12.850080056580971;
GMAT SC1.RAAN = 306.61480219479836;
GMAT SC1.AOP = 314.19055153599078;
GMAT SC1.TA = 99.887749332049225;
GMAT SC1.DryMass = 850;
GMAT SC1.Cd = 2.2;
GMAT SC1.Cr = 1.8;
GMAT SC1.DragArea = 15;
GMAT SC1.SRPArea = 1;

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
GMAT DefaultProp.Accuracy = 1.0e-13;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;

Create ReportFile If22;
GMAT If22.Filename = ./output/SystemTest/If22.report;
GMAT If22.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar1;
Create Variable testVar2;
Create Variable type;
Create Variable ansFlag; % -99 = Incorrect IF execution | 1 = Correct IF execution | 0 = IF not executed

GMAT testArray(1,1) = 5;
GMAT testVar1 = 145;
GMAT testVar2 = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% If22
% LHS: variable
% RHS: variable
% ********
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar1 ~= testVar2
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT If22.WriteHeaders = Off;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar1 > testVar2
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar1 < testVar2
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar1 >= testVar2
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar1 <= testVar2
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar1 ~= testVar2
        GMAT ansFlag = 1;
    Else
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar1 < testVar2
    Else
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 8 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
   GMAT ansFlag = 1;
   If testVar1 == testVar2
       GMAT ansFlag = -99;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 9 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testVar2 = 1*testVar1;
   If testVar1 == testVar2
       GMAT ansFlag = 1;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      If22 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT testVar2 = 145;
EndScript;