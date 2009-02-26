%% $Id: Loop_GMAT_ForIf51_15.m,v 1.4 2007/07/26 19:12:29 edove Exp $

%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      06/30/2006      Original
%   E.Dove      02/13/2007      Last Modified

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

Create ReportFile Loop_GMAT_ForIf51_15;
GMAT Loop_GMAT_ForIf51_15.Filename = ./output/AcceptTest/Loop_GMAT_ForIf51_15.report;
GMAT Loop_GMAT_ForIf51_15.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar type StepSize;
Create Variable ansFlag; % -99 = Incorrect IF execution | 1 = Correct IF execution | 0 = IF not executed
Create Variable ranOK; % 0 or less = Didn't run correctly | 1 = Ran correctly

GMAT testArray(1,1) = 5;
GMAT testVar = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% Loop_GMAT_ForIf51_15
% LHS: S/C non-time Param 
% RHS: variable
% ********
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
   GMAT ranOK = 1;
   For StepSize = 1:5;
        GMAT ansFlag = 0;
       If SC1.A1ModJulian == 5
           GMAT ansFlag = -99;
           GMAT ranOK = 0;
           Propagate   DefaultProp(SC1);
       EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   EndFor;
    GMAT Loop_GMAT_ForIf51_15.WriteHeaders = Off;
   Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT SC1.TA = 145;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian ~= 5
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian > 5
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian < 5
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK - 1;
        EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian >= 5
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian <= 5
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
        EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 7 ====== ELSE statement tests
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian ~= 5
            GMAT ranOK = 1;
        Else
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
        EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 8 ====== ELSE statement tests
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If SC1.A1ModJulian < 5
            GMAT ranOK = ranOK - 1;
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;
EndScript;

    % ===============
    %   LHS <-> RHS
    % ===============

BeginScript;
    % ===== 9 ======
    GMAT type = type + 1;
   GMAT ranOK = 1;
   For StepSize = 1:5;
        GMAT ansFlag = 0;
       If 5 == SC1.A1ModJulian
           GMAT ansFlag = -99;
            GMAT ranOK = 0;
           Propagate   DefaultProp(SC1);
       EndIf;
       Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   EndFor;
    Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT SC1.TA = 145;

    % ===== 10 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 ~= SC1.A1ModJulian
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 11 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 > SC1.A1ModJulian
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK - 1;
        EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 12 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 < SC1.A1ModJulian
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 13 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 >= SC1.A1ModJulian
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
        EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 14 ======
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 <= SC1.A1ModJulian
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 15 ====== ELSE statement tests
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 ~= SC1.A1ModJulian
            GMAT ranOK = 1;
        Else
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
        EndIf;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;

    % ===== 16 ====== ELSE statement tests
    GMAT type = type + 1;
    GMAT ranOK = 1;
    For StepSize = 1:5;
        GMAT ansFlag = 0;
        If 5 > SC1.A1ModJulian
            GMAT ranOK = ranOK - 1;
        Else
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT ranOK = ranOK + 1;
        EndIf;
        GMAT ranOK = ranOK - 1;
        Report      Loop_GMAT_ForIf51_15 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;
    GMAT SC1.TA = 145;
EndScript;