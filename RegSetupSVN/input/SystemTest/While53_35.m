% REVISION HISTORY
% $Id: While53_35.m,v 1.2 2007/07/26 19:13:12 edove Exp $

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
GMAT DefaultProp.Accuracy = 1.0e-12;
GMAT DefaultProp.MinStep = 60;
GMAT DefaultProp.MaxStep = 60;
GMAT DefaultProp.MaxStepAttempts = 50;

Create ReportFile While53_35;
GMAT While53_35.Filename = ./output/SystemTest/While53_35.report;
GMAT While53_35.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar;
Create Variable type;
Create Variable count1;
Create Variable ansFlag; % -99 = Incorrect While execution | 1 = Correct While execution | 0 = While not executed
Create Variable ranOK; % 0 = Didn't run correctly | 1 = Ran correctly

GMAT testArray(1,1) = 5;
GMAT testVar = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% While53_35
% LHS: S/C time Param 
% RHS: variable
% ********
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
   GMAT testArray(1,1) = 5;
   GMAT ranOK = 1;
   While SC1.A1ModJulian == testArray(1,1)
       GMAT ansFlag = -99;
       GMAT ranOK = 0;
       Propagate   DefaultProp(SC1);
       GMAT testArray(1,1) = 5;
       Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
       GMAT While53_35.WriteHeaders = Off;
   EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT While53_35.WriteHeaders = Off;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
   GMAT testArray(1,1) = 1*SC1.A1ModJulian;
   GMAT ranOK = 0;
   While SC1.A1ModJulian == testArray(1,1)
       GMAT ansFlag = 1;
       GMAT ranOK = 1;
       Propagate   DefaultProp(SC1);
       GMAT testArray(1,1) = 5;
       Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
       GMAT ranOK = ranOK + 1;
   EndWhile;
   GMAT ranOK = ranOK - 1;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testArray(1,1) = 5;
    GMAT ranOK = 0;
    While SC1.A1ModJulian ~= testArray(1,1)
        GMAT ansFlag = 1;
        GMAT ranOK = 1;
        Propagate   DefaultProp(SC1);
        GMAT testArray(1,1) = 1*SC1.A1ModJulian;
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        GMAT ranOK = ranOK + 1;
    EndWhile;
    GMAT ranOK = ranOK - 1;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testArray(1,1) = 1*SC1.A1ModJulian;
    GMAT ranOK = 1;
    While SC1.A1ModJulian ~= testArray(1,1)
        GMAT ansFlag = -99;
        GMAT ranOK = 0;
        Propagate   DefaultProp(SC1);
        GMAT testArray(1,1) = 1*SC1.A1ModJulian;
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndWhile;
    GMAT While53_35.WriteHeaders = Off;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = SC1.A1ModJulian - (1/400);
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    While SC1.A1ModJulian > testArray(1,1)
        GMAT ansFlag = 1;
        GMAT ranOK = 1;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        GMAT testArray(1,1) = testArray(1,1) + (1/720);
        GMAT ranOK = ranOK + 1;
    EndWhile;
    GMAT ranOK = testArray(1,1) - SC1.A1ModJulian;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = 5;
    GMAT ansFlag = 0;
    GMAT ranOK = 1;
    While SC1.A1ModJulian < testArray(1,1)
        GMAT ansFlag = -99;
        GMAT ranOK = 0;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = SC1.A1ModJulian - (1/400);
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    While SC1.A1ModJulian >= testArray(1,1)
        GMAT ansFlag = 1;
        GMAT ranOK = 1;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        GMAT testArray(1,1) = testArray(1,1) + (1/720);
        GMAT ranOK = ranOK + 1;
    EndWhile;
    GMAT ranOK = testArray(1,1) - (SC1.A1ModJulian + 0.00001);
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;


    % ===== 8 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = SC1.A1ModJulian - (1/400);
    GMAT ansFlag = 0;
    GMAT ranOK = 1;
    While SC1.A1ModJulian <= testArray(1,1)
        GMAT ansFlag = -99;
        GMAT ranOK = 0;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;

    % ===============
    %   LHS <-> RHS
    % ===============

BeginScript;
    % ===== 9 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
   GMAT testArray(1,1) = 5;
   GMAT ranOK = 1;
   While testArray(1,1) == SC1.A1ModJulian
       GMAT ansFlag = -99;
       GMAT ranOK = 0;
       Propagate   DefaultProp(SC1);
       GMAT testArray(1,1) = 1*SC1.A1ModJulian;
       Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 10 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
   GMAT testArray(1,1) = 1*SC1.A1ModJulian;
   GMAT ranOK = 0;
   While testArray(1,1) == SC1.A1ModJulian
       GMAT ansFlag = 1;
       GMAT ranOK = 1;
       Propagate   DefaultProp(SC1);
       Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
       GMAT ranOK = ranOK + 1;
   EndWhile;
    GMAT ranOK = ranOK - 1;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 11 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testArray(1,1) = 5;
    GMAT ranOK = 0;
    While testArray(1,1) ~= SC1.A1ModJulian
        GMAT ansFlag = 1;
        GMAT ranOK = 1;
        Propagate   DefaultProp(SC1);
        GMAT testArray(1,1) = 1*SC1.A1ModJulian;
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        GMAT ranOK = ranOK + 1;
    EndWhile;
    GMAT ranOK = ranOK - 1;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 12 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testArray(1,1) = 1*SC1.A1ModJulian;
    GMAT ranOK = 1;
    While testArray(1,1) ~= SC1.A1ModJulian
        GMAT ansFlag = -99;
        GMAT ranOK = 0;
        Propagate   DefaultProp(SC1);
        GMAT testArray(1,1) = 1*SC1.A1ModJulian;
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 13 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = 5;
    GMAT ansFlag = 0;
    GMAT ranOK = 1;
    While testArray(1,1) > SC1.A1ModJulian
        GMAT ansFlag = -99;
        GMAT ranOK = 0;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 14 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = SC1.A1ModJulian - (1/400);
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    While testArray(1,1) < SC1.A1ModJulian
        GMAT ansFlag = 1;
        GMAT ranOK = 1;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        GMAT testArray(1,1) = testArray(1,1) + (1/720);
        GMAT ranOK = ranOK + 1;
    EndWhile;
    GMAT ranOK = testArray(1,1) - SC1.A1ModJulian;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 15 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = 5;
    GMAT ansFlag = 0;
    GMAT ranOK = 1;
    While testArray(1,1) >= SC1.A1ModJulian
        GMAT ansFlag = -99;
        GMAT ranOK = 0;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndWhile;
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;


    % ===== 16 ======
    GMAT type = type + 1;
    GMAT testArray(1,1) = SC1.A1ModJulian - (1/400);
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    While testArray(1,1) <= SC1.A1ModJulian
        GMAT ansFlag = 1;
        GMAT ranOK = 1;
        Propagate   DefaultProp(SC1);
        Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        GMAT testArray(1,1) = testArray(1,1) + (1/720);
        GMAT ranOK = ranOK + 1;
    EndWhile;
    GMAT ranOK = testArray(1,1) - (SC1.A1ModJulian + 0.00001);
    Report      While53_35 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;