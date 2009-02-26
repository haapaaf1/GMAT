% REVISION HISTORY
% $Id: ForWhile42.m,v 1.2 2007/07/26 19:13:11 edove Exp $

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

Create ReportFile ForWhile42;
GMAT ForWhile42.Filename = ./output/SystemTest/ForWhile42.report;
GMAT ForWhile42.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar type StepSize;
Create Variable count1;
Create Variable ansFlag; % -99 = Incorrect While execution | 1 = Correct While execution | 0 = While not executed
Create Variable ranOK; % 0 = Didn't run correctly | 1 = Ran correctly

GMAT testArray(1,1) = 5;
GMAT testVar = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

% ********
% ForWhile42
% LHS: S/C non-time Param 
% RHS: variable
% ********
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
   GMAT testVar = 5;
   For StepSize = 1:5;
      GMAT ranOK = 1;
      While SC1.TA == testVar
          GMAT ansFlag = -99;
          GMAT ranOK = 0;
          Propagate   DefaultProp(SC1);
          Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
          GMAT ForWhile42.WriteHeaders = Off;
      EndWhile;
   EndFor;
    Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ForWhile42.WriteHeaders = Off;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
   For StepSize = 1:5;
       GMAT testVar = 1*SC1.TA;
       While SC1.TA == testVar
           GMAT ansFlag = 1;
           Propagate   DefaultProp(SC1);
           GMAT ranOK = ranOK + 1;
           Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
       EndWhile;
    EndFor;
    GMAT ranOK = ranOK - 4;
    Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    For StepSize = 1:5;
        GMAT testVar = 5;
        While SC1.TA ~= testVar
            GMAT ansFlag = 1;
            Propagate   DefaultProp(SC1);
            GMAT testVar = 1*SC1.TA;
            GMAT ranOK = ranOK + 1;
            Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        EndWhile;
    EndFor;
    GMAT ranOK = ranOK - 4;
    Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    For StepSize = 1:5;
        GMAT testVar = 1*SC1.TA;
        GMAT ranOK = ranOK + 1;
        While SC1.TA ~= testVar
            GMAT ansFlag = -99;
            GMAT ranOK = ranOK - 1;
            Propagate   DefaultProp(SC1);
            GMAT testVar = 1*SC1.TA;
            Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        EndWhile;
    EndFor;
    GMAT ranOK = ranOK - 4;
    Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT ranOK = 0;
    For StepSize = 1:5;
        GMAT SC1.TA = 350;
        GMAT testVar = 5;
        While SC1.TA > testVar
            GMAT ansFlag = 1;
            GMAT ranOK = 1;
            Propagate   DefaultProp(SC1);
            Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        EndWhile;
        GMAT ranOK = testVar - SC1.TA;
        Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    For StepSize = 1:5;
        GMAT SC1.TA = 350;
        GMAT testVar = 5;
        GMAT ansFlag = 0;
        GMAT ranOK = ranOK + 1;
        While SC1.TA < testVar
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
            Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        EndWhile;
    EndFor;
    GMAT ranOK = ranOK - 4;
    Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;

    % ===== 7 ======
    GMAT type = type + 1;
    For StepSize = 1:5;
        GMAT SC1.TA = 350;
        GMAT testVar = 5;
        GMAT ansFlag = 0;
        GMAT ranOK = 0;
        While SC1.TA >= testVar
            GMAT ansFlag = 1;
            GMAT ranOK = 1;
            Propagate   DefaultProp(SC1);
            Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        EndWhile;
        GMAT ranOK = testVar - SC1.TA + 0.0001;
        Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    EndFor;

    % ===== 8 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    For StepSize = 1:5;
        GMAT SC1.TA = 350;
        GMAT testVar = 5;
        GMAT ansFlag = 0;
        GMAT ranOK = ranOK + 1;
        While SC1.TA <= testVar
            GMAT ansFlag = -99;
            GMAT ranOK = 0;
            Propagate   DefaultProp(SC1);
            Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
        EndWhile;
    EndFor;
    GMAT ranOK = ranOK - 4;
    Report      ForWhile42 type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
EndScript;