
%------  Prepare GMAT creates the sandbox, and sets path data

global TestCase
clc;
clear classes;
clear all;
OpenGMAT
TestCase = 1;

%------  Define the spacecraft properties
ODSat       = Create('Spacecraft','ODSat');
ODSat.Id    = 21639;
ODSat.Epoch = 24228.72771990741;
ODSat.X     = 9892.164071524565;
ODSat.Y     = -23;
ODSat.Z     = 1837.579337039707;
ODSat.VX    = 2;
ODSat.VY    = 6.233189510799131;
ODSat.VZ    = 0.8480529946665489;
ODSat.DryMass = 1703.6700;
ODSat.OrbitCovariance = diag([100000^2*ones(3,1);1000^2*ones(3,1)]);
ODSat.Cr      = 2.2;
ODSat.Cd      = 1.8;

%------  Define the batch least squares solver
MauiData                = Create('GroundStationRange','MauiData');
MauiData.Filename       = 'LEOMaui.mat';
MauiData.AddDataType{1} = {'Range','ODSat','Maui'};


%------ Create the measuremetn simulator
MeasSim = Create('MeasurementSimulator','MeasSim');
MeasSim.Measurements    = {'MauiData'};
MeasSim.Propagator      = 'ODProp';
MeasSim.InitialEpoch    = 24228.72771990741;
MeasSim.FinalEpoch      = 24229.02771990741;
MeasSim.MeasurementTimeStep = 100;
MeasSim.Filename        = 'c:\delete\testmeas.mat';
MeasSim.Fileformat      = 'mat';
 
%------  Define the batch least squares solver
BLS = Create('BatchEstimator','BLS');
BLS.MaxIterations   = 10;
BLS.RelTolerance    = 1e-5;
BLS.AbsTolerance    = 1e-5;
BLS.Measurements    = {'MauiData'};
BLS.SolveFor        = {'ODSat.CartesianState'};
BLS.Propagator      = 'ODProp';
BLS.RunMode         = 'Solve';
 
%-----  Define the ground station properties    
Maui = Create('GroundStation','Maui');
Maui.Id = 222;
Maui.X  = -4450.8;
Maui.Y  =  2676.1;
Maui.Z  = -3691.38 ;

%----- Create a vector

%theVector = Create('PeriapsisVector','theVector');
%theVector = Create('ApoapsisVector','theVector');
%theVector = Create('PositionVector','theVector');
%theVector = Create('OrbitAngularMomentumVector','theVector');
theVector = Create('VelocityVector','theVector');
theVector.setPrimaryBody(ODSat);
%theVector.setScaleMethod('None');
%theVector.setScaleMethod('FixedLength')
theVector.setScaleMethod('ScaleFactor');
%theVector.setLengthValue(10000);
theVector.setScaleFactor(1000);
%theVector.computeVector();
theVector.setGraphicsOrigin(ODSat);

%----- Create a vector
% theVector2 = Create('PointToPoint','theVector2');
% theVector2.setPrimaryBody(ODSat);
% theVector2.setScaleFactor(3);
% theVector2.setScaleMethod('aFixedLength');
% theVector2.setLengthValue(5);
% theVector2.computeVector();

%-----  Define the Propagator   
ODProp = Create('Propagator','ODProp');
ODProp.FM.CentralBody  = 'Earth';
ODProp.FM.PointMasses  = {'Earth'};
ODProp.FM.SRP          = 'Off';
ODProp.Type            = 'RungeKutta89';
ODProp.InitialStepSize = 60;
ODProp.Accuracy        = 1.0e-8;

%------ The mission sequence
RunPropSimulator('MeasSim');
RunGMAT