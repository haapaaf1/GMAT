%% $Id: WhileForArray.m,v 1.4 2007/07/26 19:13:12 edove Exp $

%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      07/03/2006      Original
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

Create ReportFile WhileForArray;
GMAT WhileForArray.Filename = ./output/SystemTest/WhileForArray.report;
GMAT WhileForArray.Precision = 15;

Create Array testArray[2,1];
Create Variable testVar1 testVar2;
Create Variable type count StepSize;
Create Variable ansFlag; % -99 = Incorrect For execution | 1 = Correct For execution | 0 = For not executed
Create Variable ranOK; % 0 or less = Didn't run correctly | 1 = Ran correctly

GMAT testArray(1,1) = 1;
GMAT testArray(2,1) = 5;
GMAT testVar1 = 1;
GMAT testVar2 = 5;
GMAT type = 0;

% ********
% WhileForArray
% LHS: S/C non-time Param 
% RHS: variable
% ********
GMAT type = 0;
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT count = 1;
    GMAT ranOK = -1;
    GMAT ansFlag = 0;
    While count < 5.5
        GMAT ansFlag = 0;
        GMAT ranOK = 0;
        For StepSize = 1:5
            GMAT ansFlag = 1;
            GMAT ranOK = ranOK + 1;
            GMAT SC1.TA = 145;
            Propagate   DefaultProp(SC1);
            Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
        EndFor;
        GMAT ranOK = ranOK - 4;
        GMAT count = count + 1;
        GMAT WhileForArray.WriteHeaders = Off;
        Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
        GMAT ranOK = 0;
    EndWhile;
    GMAT ranOK = ranOK + 1;
    Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    GMAT SC1.TA = 145;

   % ===== 2 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = testVar1:testVar2
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 3 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = testArray(1,1):testArray(2,1)
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 4 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = 1:testVar2
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 5 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = testVar1:5
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 6 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = 1:testArray(2,1)
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 7 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = testArray(1,1):5
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 8 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = testArray(1,1):testVar2
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;

   % ===== 9 ======
   GMAT type = type + 1;
   GMAT count = 1;
   GMAT ranOK = -1;
   GMAT ansFlag = 0;
   While count < 5.5
       GMAT ansFlag = 0;
       GMAT ranOK = 0;
       For StepSize = testVar1:testArray(2,1)
           GMAT ansFlag = 1;
           GMAT ranOK = ranOK + 1;
           GMAT SC1.TA = 145;
           Propagate   DefaultProp(SC1);
           Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       EndFor;
       GMAT ranOK = ranOK - 4;
       GMAT count = count + 1;
       GMAT WhileForArray.WriteHeaders = Off;
       Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
       GMAT ranOK = 0;
   EndWhile;
   GMAT ranOK = ranOK + 1;
   Report      WhileForArray type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT SC1.TA = 145;
EndScript;