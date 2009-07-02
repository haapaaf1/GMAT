
%------  Prepare GMAT creates the sandbox, and sets path data
global TestCase
OpenGMAT
TestCase = 1;

%------  Define the spacecraft properties
ODSat       = Create('Spacecraft','ODSat');
ODSat.Id    = 21639;
ODSat.Epoch = 24228.72771990741;
ODSat.X     = 9882.164071524565;
ODSat.Y     = -13;
ODSat.Z     = 1827.579337039707;
ODSat.VX    = 0;
ODSat.VY    = 6.233189510799131;
ODSat.VZ    = 0.8480529946665489;
ODSat.DryMass = 1703.6700;
ODSat.OrbitCovariance = diag([100000^2*ones(3,1);1000^2*ones(3,1)]);
ODSat.Cr      = 2.2;
ODSat.Cd      = 1.8;

%------  Define the batch least squares solver
MauiData                = Create('GroundStationRaDec','MauiData');
MauiData.Filename       = 'LEOMauiRADec.mat';
MauiData.AddDataType{1} = {'Range','ODSat','Maui'};

%------  Define the batch least squares solver
BLS = Create('BatchEstimator','BLS');
BLS.MaxIterations   = 10;
BLS.RelTolerance    = 1e-7;
BLS.AbsTolerance    = 1e-7;
BLS.Measurements    = {'MauiData'};
BLS.SolveFor        = {'ODSat.CartesianState'};
BLS.Propagator      = 'ODProp';
BLS.RunMode         = 'Solve';
BlS.UseAprioriCovariance = 0;
 
%-----  Define the ground station properties    
Maui = Create('GroundStation','Maui');
Maui.Id = 222;
Maui.X  = -4450.8;
Maui.Y  =  2676.1;
Maui.Z  = -3691.38 ;

%-----  Define the Propagator   
ODProp = Create('Propagator','ODProp');
ODProp.FM.CentralBody  = 'Earth';
ODProp.FM.PointMasses  = {'Earth'};
ODProp.FM.SRP          = 'Off';
ODProp.Type            = 'RungeKutta89';
ODProp.InitialStepSize = 60;
ODProp.Accuracy        = 1.0e-9;

%------ The mission sequence
RunEstimator('BLS');

RunGMAT