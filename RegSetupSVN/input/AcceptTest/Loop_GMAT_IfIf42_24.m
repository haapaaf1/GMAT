%% $Id: Loop_GMAT_IfIf42_24.m,v 1.3 2007/07/26 19:12:30 edove Exp $

%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      06/29/2006      Original
%   E.Dove      11/22/2006      Last Modified

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

Create ReportFile Loop_GMAT_IfIf42_24;
GMAT Loop_GMAT_IfIf42_24.Filename = ./output/AcceptTest/Loop_GMAT_IfIf42_24.report;
GMAT Loop_GMAT_IfIf42_24.Precision = 15;

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
% Loop_GMAT_IfIf42_24
% LHS: S/C non-time Param 
% RHS: number
% ********
BeginScript;
    % ===== 1 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA ~= testVar
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT Loop_GMAT_IfIf42_24.WriteHeaders = Off;

    % ===== 2 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA > testVar
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA < testVar
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA >= testVar
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA <= testVar
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 6 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA ~= testVar
        Else
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If SC1.TA < testVar
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;

    % ===============
    %   LHS <-> RHS
    % ===============

BeginScript;
    % ===== 8 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar ~= SC1.TA
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 9 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar > SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 10 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar < SC1.TA
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 11 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar >= SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 12 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar <= SC1.TA
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 13 ======
    GMAT ansFlag = 1;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar ~= SC1.TA
        Else
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 14 ======
    GMAT ansFlag = 0;
    GMAT type = type + 1;
    If testVar2 ~= 10
        If testVar > SC1.TA
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
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
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 16 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testVar2 ~= 10
        If testVar == SC1.TA
            GMAT ansFlag = -99;
            Propagate   DefaultProp(SC1);
        EndIf;
    EndIf;
    Report      Loop_GMAT_IfIf42_24 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;