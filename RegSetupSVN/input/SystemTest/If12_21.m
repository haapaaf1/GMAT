% REVISION HISTORY
% $Id: If12_21.m,v 1.2 2007/07/26 19:13:11 edove Exp $

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

Create ReportFile If12_21;
GMAT If12_21.Filename = ./output/SystemTest/If12_21.report;
GMAT If12_21.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar;
Create Variable type;
Create Variable ansFlag; % -99 = Incorrect IF execution | 1 = Correct IF execution | 0 = IF not executed

GMAT testArray(1,1) = 5;
GMAT testVar = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% If12_21
% LHS: number
% RHS: variable
% ********
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If 145 ~= testVar
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT If12_21.WriteHeaders = Off;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If 145 > testVar
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If 145 < testVar
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If 145 >= testVar
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If 145 <= testVar
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If 145 ~= testVar
        GMAT ansFlag = 1;
    Else
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If 145 < testVar
    Else
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;

    % ===============
    %   LHS <-> RHS
    % ===============

BeginScript;
    % ===== 8 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar ~= 145
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 9 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar > 145
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 10 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar < 145
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 11 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar >= 145
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 12 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar <= 145
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 13 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar ~= 145
        GMAT ansFlag = 1;
    Else
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 14 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar > 145
    Else
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;

    % ===============
    %   Added Tests
    % ===============

BeginScript;
    % ===== 15 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
   GMAT ansFlag = 1;
   If 20 == testVar 
       GMAT ansFlag = -99;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 16 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
   If 5 == testVar 
       GMAT ansFlag = 1;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 17 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
   GMAT ansFlag = 1;
   If testVar == 20 
       GMAT ansFlag = -99;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 18 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
   If testVar == 5 
       GMAT ansFlag = 1;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      If12_21 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;