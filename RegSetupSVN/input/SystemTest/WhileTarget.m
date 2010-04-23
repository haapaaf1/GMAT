% REVISION HISTORY
% $Id: WhileTarget.m,v 1.3 2007/09/05 14:58:35 edove Exp $

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
GMAT DefaultProp.MinStep = .0001;
GMAT DefaultProp.MaxStep = 60;
GMAT DefaultProp.MaxStepAttempts = 50;

Create ImpulsiveBurn DefaultIB;
GMAT DefaultIB.Origin = Earth;
GMAT DefaultIB.Axes = VNB;
GMAT DefaultIB.VectorFormat = Cartesian;
GMAT DefaultIB.Element1 = 0;
GMAT DefaultIB.Element2 = 0;
GMAT DefaultIB.Element3 = 0;

Create DifferentialCorrector DC1;
GMAT DC1.ShowProgress = true;
GMAT DC1.ReportStyle = Normal;
GMAT DC1.TargeterTextFile = 'targeter_DC1.data';
GMAT DC1.MaximumIterations = 25;
GMAT DC1.UseCentralDifferences = false;

Create ReportFile WhileTarget;
GMAT WhileTarget.Filename = './output/SystemTest/WhileTarget.report';
GMAT WhileTarget.Precision = 15;

Create Array testArray[1,1];
Create Variable testVar;
Create Variable type;
Create Variable count1;
Create Variable ansFlag; % -99 = Incorrect While execution | 1 = Correct While execution | 0 = While not executed
Create Variable ranOK; % 0 = Didn't run correctly | 1 = Ran correctly
Create Variable SMAtarget;

GMAT testArray(1,1) = 5;
GMAT testVar = 5;
GMAT type = 0;

GMAT SC1.TA = 145;

BeginScript;    
    % ===== 1 ======
    GMAT type = type + 1;
    GMAT ansFlag = 0;
    GMAT testVar = 5;
    GMAT ranOK = 0;
    While SC1.A1ModJulian ~= testVar
        Propagate DefaultProp(SC1, {SC1.TA = 220});
        
        GMAT SMAtarget = SC1.SMA + 300;
        Target DC1;
           Vary DC1(DefaultIB.Element1 = 0.08, {Perturbation = 1e-004, MaxStep = 0.05, Lower = -0.5, Upper = 0.5});

           Maneuver DefaultIB(SC1);

           Propagate DefaultProp(SC1 , {SC1.TA = 180});

           GMAT ansFlag = 1;
           GMAT ranOK = 1;
           Report      WhileTarget type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.SMA SC1.X SC1.Y SC1.Z;
           GMAT WhileTarget.WriteHeaders = Off;

           Achieve DC1(SC1.SMA = SMAtarget, {Tolerance = 0.0001});
        EndTarget;

     
        GMAT testVar = 1*SC1.A1ModJulian;
        Report      WhileTarget type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.SMA SC1.X SC1.Y SC1.Z;
        GMAT ranOK = ranOK + 1;

    EndWhile;
    GMAT ranOK = ranOK - 1;
    Report      WhileTarget type ansFlag ranOK SC1.A1ModJulian SC1.ElapsedSecs SC1.TA SC1.SMA SC1.X SC1.Y SC1.Z;
EndScript;