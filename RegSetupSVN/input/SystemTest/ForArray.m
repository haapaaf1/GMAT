%% $Id: ForArray.m,v 1.7 2007/12/18 21:20:30 edove Exp $

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

Create ReportFile Loop_GMAT_For;
GMAT Loop_GMAT_For.Filename = ./output/SystemTest/ForArray.report;
GMAT Loop_GMAT_For.Precision = 15;

Create Array testArray[2,1];
Create Variable testVar1;
Create Variable testVar2;
Create Variable type Ivar count StepSize;
Create Variable ansFlag; % -99 = Incorrect For execution | 1 = Correct For execution | 0 = For not executed
Create Variable ranOK; % 0 or less = Didn't run correctly | 1 = Ran correctly
Create Array Iarr[5,1];
Create Array Iarr2[5,1];

GMAT testArray(1,1) = 1;
GMAT testArray(2,1) = 5;
GMAT testVar1 = 1;
GMAT testVar2 = 5;
GMAT type = 0;

% **************
% Loop_GMAT_For
% **************
BeginScript;
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = 1:5
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
    GMAT Loop_GMAT_For.WriteHeaders = Off;
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 2 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = testVar1:testVar2
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 3 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = testArray(1,1):testArray(2,1)
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 4 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = 1:testVar2
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 5 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = testVar1:5
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 6 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = 1:testArray(2,1)
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 7 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = testArray(1,1):5
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 8 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = testArray(1,1):testVar2
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;

    % ===== 9 ======
    GMAT type = type + 1;
    GMAT ranOK = 0;
    GMAT ansFlag = 0;
    For StepSize = testVar1:testArray(2,1)
        GMAT ansFlag = 1;
        GMAT ranOK = ranOK + 1;
        GMAT SC1.TA = 145;
        Propagate   DefaultProp(SC1);
        Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
    EndFor;
    GMAT ranOK = ranOK - 4.999;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT SC1.TA = 145;
EndScript;

BeginScript;
% ===== 10 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
For StepSize = 1:2:9
    GMAT ansFlag = 1;
    GMAT ranOK = ranOK + 1;
    GMAT SC1.TA = 145;
    Propagate   DefaultProp(SC1);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
EndFor;
GMAT ranOK = ranOK - 4.999;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 11 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
For StepSize = testVar1:testVar1:testVar2
    GMAT ansFlag = 1;
    GMAT ranOK = ranOK + 1;
    GMAT SC1.TA = 145;
    Propagate   DefaultProp(SC1);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
EndFor;
GMAT ranOK = ranOK - 4.999;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
    GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 12 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
For StepSize = testArray(1,1):testArray(1,1):testArray(2,1)
   GMAT ansFlag = 1;
   GMAT ranOK = ranOK + 1;
   GMAT SC1.TA = 145;
   Propagate   DefaultProp(SC1);
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
EndFor;
GMAT ranOK = ranOK - 4.999;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
   GMAT ranOK = 5.0001 - (ranOK + 4.999);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 13 ====== % GMAT not supporting this For index configuration
 GMAT type = type + 1;
% GMAT ranOK = 0;
% GMAT ansFlag = 0;
% GMAT SC1.RAAN = 1.0;
% GMAT SC1.AOP = 5.0;
% For StepSize = SC1.RAAN:SC1.AOP
%     GMAT ansFlag = 1;
%     GMAT ranOK = ranOK + 1;
%     GMAT SC1.TA = 145;
%     Propagate   DefaultProp(SC1);
%     Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
% EndFor;
% GMAT ranOK = ranOK - 4.999;
% Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
% GMAT ranOK = 5.0001 - (ranOK + 4.999);
% Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
% GMAT SC1.TA = 145;

% ===== 14 ====== % GMAT not supporting this For index configuration
 GMAT type = type + 1;
%GMAT ranOK = 0;
%GMAT ansFlag = 0;
%GMAT SC1.RAAN = 1.0;
%GMAT SC1.AOP = 5.0;
%For StepSize = SC1.RAAN:SC1.RAAN:SC1.AOP
%    GMAT ansFlag = 1;
%    GMAT ranOK = ranOK + 1;
%    GMAT SC1.TA = 145;
%    Propagate   DefaultProp(SC1);
%    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
%EndFor;
%GMAT ranOK = ranOK - 4.999;
%Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
%GMAT ranOK = 5.0001 - (ranOK + 4.999);
%Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
%GMAT SC1.TA = 145;

% ===== 15 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
For Ivar = 1:5
    GMAT ansFlag = 1;
    GMAT ranOK = ranOK + 1;
    GMAT SC1.TA = 145;
    Propagate   DefaultProp(SC1);
    Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
EndFor;
GMAT ranOK = ranOK - 4.999;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT ranOK = 5.0001 - (ranOK + 4.999);
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 16 ====== % GMAT not supporting this For index configuration
GMAT type = type + 1;
%GMAT ranOK = 0;
%GMAT ansFlag = 0;
%For Iarr(1,1) = 1:5
%   GMAT ansFlag = 1;
%   GMAT ranOK = ranOK + 1;
%   GMAT SC1.TA = 145;
%   Propagate   DefaultProp(SC1);
%   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
%EndFor;
%GMAT ranOK = ranOK - 4.999;
%Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
%GMAT ranOK = 5.0001 - (ranOK + 4.999);
%Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
%GMAT Iarr(1,1) = 1;
%GMAT SC1.TA = 145;

% ===== 17 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
GMAT Iarr(1,1) = 1;
GMAT Iarr(2,1) = 2;
GMAT Iarr(3,1) = 9;
For Ivar = Iarr(1,1):Iarr(2,1):Iarr(3,1)
   GMAT ansFlag = 1;
   GMAT ranOK = ranOK + 1;
   GMAT SC1.TA = 145;
   Propagate   DefaultProp(SC1);
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
EndFor;
GMAT ranOK = ranOK - 4.999;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT ranOK = 5.0001 - (ranOK + 4.999);
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 18 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
GMAT count = 1;
For Ivar = 1:5
   GMAT ansFlag = 1;
   GMAT ranOK = Ivar;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT ranOK = Ivar - count + 0.0001;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT ranOK = count - Ivar + 0.0001;
   Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
   GMAT count = count + 1;
EndFor;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 19 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
GMAT count = 1;
For Ivar = 1:5
  GMAT ansFlag = 1;
  GMAT Iarr(Ivar,1) = count; 
  GMAT ranOK = Iarr(Ivar,1);
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT ranOK = Iarr(Ivar,1) - count + 0.0001;
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT ranOK = count - Iarr(Ivar,1) + 0.0001;
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT count = count + 1;
EndFor;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 20 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
GMAT count = 1;
For Ivar = 1:5
  GMAT ansFlag = 1;
  GMAT Iarr(Ivar,1) = Ivar; 
  GMAT ranOK = Iarr(Ivar,1);
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT ranOK = Iarr(Ivar,1) - count + 0.0001;
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT ranOK = count - Iarr(Ivar,1) + 0.0001;
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT count = count + 1;
EndFor;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

% ===== 21 ======
GMAT type = type + 1;
GMAT ranOK = 0;
GMAT ansFlag = 0;
GMAT count = 1;
For Ivar = 1:5
  GMAT ansFlag = 1;
  GMAT Iarr(Ivar,1) = count;
  GMAT Iarr2(Ivar,1) = Iarr(Ivar,1) + 1 - 1;
  GMAT ranOK = Iarr2(Ivar,1);
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT ranOK = Iarr2(Ivar,1) - count + 0.0001;
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT ranOK = count - Iarr2(Ivar,1) + 0.0001;
  Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z; 
  GMAT count = count + 1;
EndFor;
Report      Loop_GMAT_For type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.X SC1.Y SC1.Z;
GMAT SC1.TA = 145;

EndScript;