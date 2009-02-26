%% $Id: Loop_GMAT_If33.m,v 1.3 2007/07/26 19:12:29 edove Exp $

%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      06/23/2006      Original
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

Create ReportFile Loop_GMAT_If33;
GMAT Loop_GMAT_If33.Filename = ./output/AcceptTest/Loop_GMAT_If33.report;
GMAT Loop_GMAT_If33.Precision = 15;

Create Array testArray[1,2];
Create Variable type;
Create Variable ansFlag; % -99 = Incorrect IF execution | 1 = Correct IF execution | 0 = IF not executed

GMAT testArray(1,1) = 145;
GMAT testArray(1,2) = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% Loop_GMAT_If33
% LHS: array 
% RHS: array
% ********
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testArray(1,1) ~= testArray(1,2)
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT Loop_GMAT_If33.WriteHeaders = Off;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testArray(1,1) > testArray(1,2)
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testArray(1,1) < testArray(1,2)
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testArray(1,1) >= testArray(1,2)
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT ansFlag = 1;
    If testArray(1,1) <= testArray(1,2)
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testArray(1,1) ~= testArray(1,2)
        GMAT ansFlag = 1;
    Else
        GMAT ansFlag = -99;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    If testArray(1,1) < testArray(1,2)
    Else
        GMAT ansFlag = 1;
        Propagate   DefaultProp(SC1);
    EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 8 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
   GMAT ansFlag = 1;
   If testArray(1,1) == testArray(1,2)
       GMAT ansFlag = -99;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 9 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testArray(1,2) = testArray(1,1);
   If testArray(1,1) == testArray(1,2)
       GMAT ansFlag = 1;
       Propagate   DefaultProp(SC1);
   EndIf;
    Report      Loop_GMAT_If33 type ansFlag SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT testArray(1,2)= 145;
EndScript;