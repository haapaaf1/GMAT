% REVISION HISTORY
% $Id: IfIf43_34.m,v 1.2 2007/07/26 19:13:12 edove Exp $

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

Create ReportFile IfIf43_34;
GMAT IfIf43_34.Filename = './output/SystemTest/IfIf43_34.report';
GMAT IfIf43_34.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar testVar2;
Create Variable type;
Create Variable ansFlag; % -99 = Incorrect IF execution | 1 = Correct IF execution | 0 = IF not executed

GMAT testArray(1,1) = 5;
GMAT testVar = 5;
GMAT testVar2 = 2006;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% IfIf43_34
% LHS: S/C non-time Param 
% RHS: number
% ********
BeginScript;
    % ===== 1 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA ~= testArray(1,1)
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT IfIf43_34.WriteHeaders = Off;

    % ===== 2 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA > testArray(1,1)
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA < testArray(1,1)
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA >= testArray(1,1)
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA <= testArray(1,1)
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 6 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA ~= testArray(1,1)
        Else
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA < testArray(1,1)
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;

    % ===============
    %   LHS <-> RHS
    % ===============

BeginScript;
    % ===== 8 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) ~= SC1.TA
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 9 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) > SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 10 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) < SC1.TA
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 11 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) >= SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 12 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) <= SC1.TA
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 13 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) ~= SC1.TA
        Else
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 14 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testArray(1,1) > SC1.TA
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;


    % ===============
    %   Added Tests
    % ===============

BeginScript;
    % ===== 15 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar2 ~= 10
        If SC1.TA == testVar 
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 16 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar2 ~= 10
        If testVar == SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 17 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar2 ~= 10
        If testVar == SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 18 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar2 < 10
        If testVar == SC1.TA
            GMAT ansFlag = -99;
        Else
            GMAT ansFlag = -99;
        EndIf;
    Else;
        If testVar == SC1.TA
            GMAT ansFlag = -99;
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;                
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 19 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar2 ~= 10
      BeginScript;
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
      EndScript;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;

    % ===== 20 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testVar2 ~= 10
      BeginScript;
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
      EndScript;
    EndIf;
    Report      IfIf43_34 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;