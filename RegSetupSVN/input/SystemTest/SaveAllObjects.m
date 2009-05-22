%$Id: SaveAllObjects.m,v 1.12 2008/09/30 21:09:15 edove Exp $ 

%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft Sat1;
GMAT Sat1.DateFormat = TAIModJulian;
GMAT Sat1.Epoch = 21545;
GMAT Sat1.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat1.DisplayStateType = Keplerian;
GMAT Sat1.SMA = 19999.99999999998;
GMAT Sat1.ECC = 0.2999999999999993;
GMAT Sat1.INC = 78;
GMAT Sat1.RAAN = 306.6148021947984;
GMAT Sat1.AOP = 314.1905515359919;
GMAT Sat1.TA = 99.88774933204908;
GMAT Sat1.DryMass = 850;
GMAT Sat1.Cd = 2.2;
GMAT Sat1.Cr = 1.8;
GMAT Sat1.DragArea = 15;
GMAT Sat1.SRPArea = 1;

Create Spacecraft Sat2;
GMAT Sat2.DateFormat = TAIModJulian;
GMAT Sat2.Epoch = 21545;
GMAT Sat2.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat2.DisplayStateType = Cartesian;
GMAT Sat2.X = 7100;
GMAT Sat2.Y = 0;
GMAT Sat2.Z = 1300;
GMAT Sat2.VX = 0;
GMAT Sat2.VY = 7.35;
GMAT Sat2.VZ = 1;
GMAT Sat2.DryMass = 850;
GMAT Sat2.Cd = 2.2;
GMAT Sat2.Cr = 1.8;
GMAT Sat2.DragArea = 15;
GMAT Sat2.SRPArea = 1;

Create Spacecraft Sat3;
GMAT Sat3.DateFormat = TAIModJulian;
GMAT Sat3.Epoch = 21545;
GMAT Sat3.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat3.DisplayStateType = Cartesian;
GMAT Sat3.X = 7100;
GMAT Sat3.Y = 0;
GMAT Sat3.Z = 1300;
GMAT Sat3.VX = 0;
GMAT Sat3.VY = 7.35;
GMAT Sat3.VZ = 1;
GMAT Sat3.DryMass = 850;
GMAT Sat3.Cd = 2.2;
GMAT Sat3.Cr = 1.8;
GMAT Sat3.DragArea = 15;
GMAT Sat3.SRPArea = 1;

Create Spacecraft Sat4;
GMAT Sat4.DateFormat = TAIModJulian;
GMAT Sat4.Epoch = 21545;
GMAT Sat4.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat4.DisplayStateType = Cartesian;
GMAT Sat4.X = 7100;
GMAT Sat4.Y = 0;
GMAT Sat4.Z = 1300;
GMAT Sat4.VX = 0;
GMAT Sat4.VY = 7.35;
GMAT Sat4.VZ = 1;
GMAT Sat4.DryMass = 850;
GMAT Sat4.Cd = 2.2;
GMAT Sat4.Cr = 1.8;
GMAT Sat4.DragArea = 15;
GMAT Sat4.SRPArea = 1;

Create Spacecraft Sat5;
GMAT Sat5.DateFormat = TAIModJulian;
GMAT Sat5.Epoch = 21545;
GMAT Sat5.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat5.DisplayStateType = Cartesian;
GMAT Sat5.X = 7100;
GMAT Sat5.Y = 0;
GMAT Sat5.Z = 1300;
GMAT Sat5.VX = 0;
GMAT Sat5.VY = 7.35;
GMAT Sat5.VZ = 1;
GMAT Sat5.DryMass = 850;
GMAT Sat5.Cd = 2.2;
GMAT Sat5.Cr = 1.8;
GMAT Sat5.DragArea = 15;
GMAT Sat5.SRPArea = 1;


%----------------------------------------
%---------- Hardware Components
%----------------------------------------

Create FuelTank Tank1;
GMAT Tank1.FuelMass = 756;
GMAT Tank1.Pressure = 1500;
GMAT Tank1.Temperature = 20;
GMAT Tank1.RefTemperature = 20;
GMAT Tank1.Volume = 0.75;
GMAT Tank1.FuelDensity = 1260;
GMAT Tank1.PressureRegulated = true;

Create Thruster Thruster1;
GMAT Thruster1.Element1 = 1;
GMAT Thruster1.Element2 = 0;
GMAT Thruster1.Element3 = 0;
GMAT Thruster1.C1 = 500;
GMAT Thruster1.C2 = 0;
GMAT Thruster1.C3 = 0;
GMAT Thruster1.C4 = 0;
GMAT Thruster1.C5 = 0;
GMAT Thruster1.C6 = 0;
GMAT Thruster1.C7 = 0;
GMAT Thruster1.C8 = 0;
GMAT Thruster1.C9 = 0;
GMAT Thruster1.C10 = 0;
GMAT Thruster1.C11 = 1;
GMAT Thruster1.C12 = 0;
GMAT Thruster1.C13 = 0;
GMAT Thruster1.C14 = 0;
GMAT Thruster1.K1 = 2150;
GMAT Thruster1.K2 = 0;
GMAT Thruster1.K3 = 0;
GMAT Thruster1.K4 = 0;
GMAT Thruster1.K5 = 0;
GMAT Thruster1.K6 = 0;
GMAT Thruster1.K7 = 0;
GMAT Thruster1.K8 = 0;
GMAT Thruster1.K9 = 0;
GMAT Thruster1.K10 = 0;
GMAT Thruster1.K11 = 1;
GMAT Thruster1.K12 = 0;
GMAT Thruster1.K13 = 0;
GMAT Thruster1.K14 = 0;
GMAT Thruster1.CoordinateSystem = MJ2000EarthEquator;
GMAT Thruster1.ThrustScaleFactor = 1;


%----------------------------------------
%---------- Formations
%----------------------------------------

Create Formation Formation1;
GMAT Formation1.A1Epoch = 21545;
GMAT Formation1.Add = {Sat1, Sat2, Sat3, Sat4};


%----------------------------------------
%---------- ForceModels
%----------------------------------------

Create ForceModel RKVProp_ForceModel;
GMAT RKVProp_ForceModel.CentralBody = Earth;
GMAT RKVProp_ForceModel.PrimaryBodies = {Earth};
GMAT RKVProp_ForceModel.Drag = None;
GMAT RKVProp_ForceModel.SRP = Off;
GMAT RKVProp_ForceModel.ErrorControl = RSSStep;
GMAT RKVProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT RKVProp_ForceModel.Gravity.Earth.Order = 4;
GMAT RKVProp_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';

Create ForceModel RKNProp_ForceModel;
GMAT RKNProp_ForceModel.CentralBody = Earth;
GMAT RKNProp_ForceModel.PrimaryBodies = {Earth};
GMAT RKNProp_ForceModel.PointMasses = {Luna, Sun, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto};
GMAT RKNProp_ForceModel.Drag = Exponential;
GMAT RKNProp_ForceModel.SRP = On;
GMAT RKNProp_ForceModel.ErrorControl = RSSStep;
GMAT RKNProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT RKNProp_ForceModel.Gravity.Earth.Order = 4;
GMAT RKNProp_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';
GMAT RKNProp_ForceModel.Drag.AtmosphereModel = Exponential;
GMAT RKNProp_ForceModel.Drag.InputSource = Constant;
GMAT RKNProp_ForceModel.Drag.F107 = 150;
GMAT RKNProp_ForceModel.Drag.F107A = 150;
GMAT RKNProp_ForceModel.Drag.MagneticIndex = 3;
GMAT RKNProp_ForceModel.SRP.Flux = 1367;
GMAT RKNProp_ForceModel.SRP.Nominal_Sun = 149597870.691;

Create ForceModel RKF56Prop_ForceModel;
GMAT RKF56Prop_ForceModel.CentralBody = Luna;
GMAT RKF56Prop_ForceModel.PrimaryBodies = {Earth, Luna};
GMAT RKF56Prop_ForceModel.PointMasses = {Sun};
GMAT RKF56Prop_ForceModel.Drag = MSISE90;
GMAT RKF56Prop_ForceModel.SRP = Off;
GMAT RKF56Prop_ForceModel.ErrorControl = RSSStep;
GMAT RKF56Prop_ForceModel.Gravity.Earth.Degree = 4;
GMAT RKF56Prop_ForceModel.Gravity.Earth.Order = 4;
GMAT RKF56Prop_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/luna/lp165p.cof';
GMAT RKF56Prop_ForceModel.Gravity.Luna.Degree = 4;
GMAT RKF56Prop_ForceModel.Gravity.Luna.Order = 4;
GMAT RKF56Prop_ForceModel.Drag.AtmosphereModel = MSISE90;
GMAT RKF56Prop_ForceModel.Drag.InputSource = Constant;
GMAT RKF56Prop_ForceModel.Drag.F107 = 150;
GMAT RKF56Prop_ForceModel.Drag.F107A = 150;
GMAT RKF56Prop_ForceModel.Drag.MagneticIndex = 3;

Create ForceModel PD45Prop_ForceModel;
GMAT PD45Prop_ForceModel.CentralBody = Earth;
GMAT PD45Prop_ForceModel.PrimaryBodies = {Earth};
GMAT PD45Prop_ForceModel.Drag = None;
GMAT PD45Prop_ForceModel.SRP = Off;
GMAT PD45Prop_ForceModel.ErrorControl = RSSStep;
GMAT PD45Prop_ForceModel.Gravity.Earth.Degree = 4;
GMAT PD45Prop_ForceModel.Gravity.Earth.Order = 4;
GMAT PD45Prop_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';

Create ForceModel PD78Prop_ForceModel;
GMAT PD78Prop_ForceModel.CentralBody = Earth;
GMAT PD78Prop_ForceModel.PrimaryBodies = {Earth};
GMAT PD78Prop_ForceModel.Drag = None;
GMAT PD78Prop_ForceModel.SRP = Off;
GMAT PD78Prop_ForceModel.ErrorControl = RSSStep;
GMAT PD78Prop_ForceModel.Gravity.Earth.Degree = 4;
GMAT PD78Prop_ForceModel.Gravity.Earth.Order = 4;
GMAT PD78Prop_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';

Create ForceModel BSProp_ForceModel;
GMAT BSProp_ForceModel.CentralBody = Earth;
GMAT BSProp_ForceModel.PrimaryBodies = {Earth};
GMAT BSProp_ForceModel.Drag = None;
GMAT BSProp_ForceModel.SRP = Off;
GMAT BSProp_ForceModel.ErrorControl = RSSStep;
GMAT BSProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT BSProp_ForceModel.Gravity.Earth.Order = 4;
GMAT BSProp_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';

Create ForceModel ABMProp_ForceModel;
GMAT ABMProp_ForceModel.CentralBody = Earth;
GMAT ABMProp_ForceModel.PrimaryBodies = {Earth};
GMAT ABMProp_ForceModel.Drag = None;
GMAT ABMProp_ForceModel.SRP = Off;
GMAT ABMProp_ForceModel.ErrorControl = RSSStep;
GMAT ABMProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT ABMProp_ForceModel.Gravity.Earth.Order = 4;
GMAT ABMProp_ForceModel.Gravity.Earth.PotentialFile = './files/gravity/earth/JGM2.cof';


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create Propagator RKVProp;
GMAT RKVProp.FM = RKVProp_ForceModel;
GMAT RKVProp.Type = RungeKutta89;
GMAT RKVProp.InitialStepSize = 60;
GMAT RKVProp.Accuracy = 9.999999999999999e-012;
GMAT RKVProp.MinStep = 0.001;
GMAT RKVProp.MaxStep = 2700;
GMAT RKVProp.MaxStepAttempts = 50;

Create Propagator RKNProp;
GMAT RKNProp.FM = RKNProp_ForceModel;
GMAT RKNProp.Type = DormandElMikkawyPrince68;
GMAT RKNProp.InitialStepSize = 60;
GMAT RKNProp.Accuracy = 9.999999999999999e-012;
GMAT RKNProp.MinStep = 0.001;
GMAT RKNProp.MaxStep = 2700;
GMAT RKNProp.MaxStepAttempts = 50;

Create Propagator RKF56Prop;
GMAT RKF56Prop.FM = RKF56Prop_ForceModel;
GMAT RKF56Prop.Type = RungeKuttaFehlberg56;
GMAT RKF56Prop.InitialStepSize = 60;
GMAT RKF56Prop.Accuracy = 9.999999999999999e-012;
GMAT RKF56Prop.MinStep = 0.001;
GMAT RKF56Prop.MaxStep = 2700;
GMAT RKF56Prop.MaxStepAttempts = 50;

Create Propagator PD45Prop;
GMAT PD45Prop.FM = PD45Prop_ForceModel;
GMAT PD45Prop.Type = PrinceDormand45;
GMAT PD45Prop.InitialStepSize = 60;
GMAT PD45Prop.Accuracy = 9.999999999999999e-012;
GMAT PD45Prop.MinStep = 0.001;
GMAT PD45Prop.MaxStep = 2700;
GMAT PD45Prop.MaxStepAttempts = 50;

Create Propagator PD78Prop;
GMAT PD78Prop.FM = PD78Prop_ForceModel;
GMAT PD78Prop.Type = PrinceDormand78;
GMAT PD78Prop.InitialStepSize = 60;
GMAT PD78Prop.Accuracy = 9.999999999999999e-012;
GMAT PD78Prop.MinStep = 0.001;
GMAT PD78Prop.MaxStep = 2700;
GMAT PD78Prop.MaxStepAttempts = 50;

Create Propagator BSProp;
GMAT BSProp.FM = BSProp_ForceModel;
GMAT BSProp.Type = BulirschStoer;
GMAT BSProp.InitialStepSize = 60;
GMAT BSProp.Accuracy = 9.999999999999999e-012;
GMAT BSProp.MinStep = 0.001;
GMAT BSProp.MaxStep = 2700;
GMAT BSProp.MaxStepAttempts = 50;
GMAT BSProp.MinimumReduction = 0.7;
GMAT BSProp.MaximumReduction = 1e-005;
GMAT BSProp.MinimumTolerance = 1e-012;

Create Propagator ABMProp;
GMAT ABMProp.FM = ABMProp_ForceModel;
GMAT ABMProp.Type = AdamsBashforthMoulton;
GMAT ABMProp.InitialStepSize = 60;
GMAT ABMProp.Accuracy = 1e-010;
GMAT ABMProp.MinStep = 0.001;
GMAT ABMProp.MaxStep = 2700;
GMAT ABMProp.MaxStepAttempts = 50;
GMAT ABMProp.LowerError = 1e-013;
GMAT ABMProp.TargetError = 9.999999999999999e-012;


%----------------------------------------
%---------- Burns
%----------------------------------------

Create ImpulsiveBurn Impulse1;
GMAT Impulse1.Origin = Earth;
GMAT Impulse1.Axes = VNB;
GMAT Impulse1.VectorFormat = Cartesian;
GMAT Impulse1.Element1 = 0;
GMAT Impulse1.Element2 = 0;
GMAT Impulse1.Element3 = 0;

Create FiniteBurn FiniteBurn1;
GMAT FiniteBurn1.Origin = Earth;
GMAT FiniteBurn1.Axes = VNB;
GMAT FiniteBurn1.Thrusters = {Thruster1};
GMAT FiniteBurn1.BurnScaleFactor = 1;

Create ImpulsiveBurn Impulse2;
GMAT Impulse2.Origin = Luna;
GMAT Impulse2.Axes = MJ2000Eq;
GMAT Impulse2.VectorFormat = Cartesian;
GMAT Impulse2.Element1 = 0;
GMAT Impulse2.Element2 = 0;
GMAT Impulse2.Element3 = 0;

Create FiniteBurn FiniteBurn2;
GMAT FiniteBurn2.Origin = Luna;
GMAT FiniteBurn2.Axes = MJ2000Eq;
GMAT FiniteBurn2.Thrusters = {Thruster1};
GMAT FiniteBurn2.BurnScaleFactor = 1;


%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable fred;
GMAT fred = 0;

Create Array rotmat[3, 3];
GMAT rotmat(1, 1) = 0;
GMAT rotmat(1, 2) = 0;
GMAT rotmat(1, 3) = 0;
GMAT rotmat(2, 1) = 0;
GMAT rotmat(2, 2) = 0;
GMAT rotmat(2, 3) = 0;
GMAT rotmat(3, 1) = 0;
GMAT rotmat(3, 2) = 0;
GMAT rotmat(3, 3) = 0;

Create String showMe;
GMAT showMe = Missouri, The Show Me State


%----------------------------------------
%---------- Coordinate Systems
%----------------------------------------

Create CoordinateSystem EarthMJ2000Eq;
GMAT EarthMJ2000Eq.Origin = Earth;
GMAT EarthMJ2000Eq.Axes = MJ2000Eq;
GMAT EarthMJ2000Eq.UpdateInterval = 60;
GMAT EarthMJ2000Eq.OverrideOriginInterval = false;

Create CoordinateSystem EarthMJ2000Ec;
GMAT EarthMJ2000Ec.Origin = Earth;
GMAT EarthMJ2000Ec.Axes = MJ2000Ec;
GMAT EarthMJ2000Ec.UpdateInterval = 60;
GMAT EarthMJ2000Ec.OverrideOriginInterval = false;

Create CoordinateSystem EarthFixed;
GMAT EarthFixed.Origin = Earth;
GMAT EarthFixed.Axes = BodyFixed;
GMAT EarthFixed.UpdateInterval = 60;
GMAT EarthFixed.OverrideOriginInterval = false;

Create CoordinateSystem spiffyNewCS;
GMAT spiffyNewCS.Origin = Earth;
GMAT spiffyNewCS.Axes = ObjectReferenced;
GMAT spiffyNewCS.UpdateInterval = 60;
GMAT spiffyNewCS.OverrideOriginInterval = false;
GMAT spiffyNewCS.XAxis = R;
GMAT spiffyNewCS.ZAxis = N;
GMAT spiffyNewCS.Primary = Sat2;
GMAT spiffyNewCS.Secondary = Sat5;


%----------------------------------------
%---------- Solvers
%----------------------------------------

Create DifferentialCorrector DC1;
GMAT DC1.ShowProgress = true;
GMAT DC1.ReportStyle = Normal;
GMAT DC1.TargeterTextFile = 'DifferentialCorrectorDC1.data';
GMAT DC1.MaximumIterations = 25;
GMAT DC1.UseCentralDifferences = false;

Create FminconOptimizer SQP1;
GMAT SQP1.ShowProgress = true;
GMAT SQP1.ReportStyle = Normal;
GMAT SQP1.TargeterTextFile = 'FminconOptimizerSQP1.data';
GMAT SQP1.MaximumIterations = 25;
GMAT SQP1.ObjectiveFunction = Objective;
GMAT SQP1.Tolerance = 0;
GMAT SQP1.SourceType = MATLAB;
GMAT SQP1.DiffMaxChange = 0.1000;
GMAT SQP1.DiffMinChange = 1.0000e-08; 
GMAT SQP1.MaxFunEvals = 1000; 
GMAT SQP1.MaxIter = 400;
GMAT SQP1.TolX = 1.0000e-04;
GMAT SQP1.TolFun = 1.0000e-04; 
GMAT SQP1.TolCon = 1.0000e-04; 
GMAT SQP1.DerivativeCheck = Off; 
GMAT SQP1.Diagnostics = Off; 
GMAT SQP1.Display = iter; 
GMAT SQP1.GradObj = Off;
GMAT SQP1.GradConstr = Off;


%----------------------------------------
%---------- Subscribers
%----------------------------------------

Create OpenGLPlot OpenGL;
GMAT OpenGL.Add = {Sat1, Earth};
GMAT OpenGL.CoordinateSystem = EarthMJ2000Eq;
GMAT OpenGL.ViewPointReference = Earth;
GMAT OpenGL.ViewDirection = Earth;
GMAT OpenGL.ViewScaleFactor = 1;
GMAT OpenGL.FixedFovAngle = 45;
GMAT OpenGL.ViewUpCoordinateSystem = EarthMJ2000Eq;
GMAT OpenGL.ViewUpAxis = Z;
GMAT OpenGL.CelestialPlane = Off;
GMAT OpenGL.XYPlane = On;
GMAT OpenGL.WireFrame = Off;
GMAT OpenGL.SolverIterations = None;
GMAT OpenGL.Axes = On;
GMAT OpenGL.Grid = On;
GMAT OpenGL.SunLine = On;
GMAT OpenGL.UseInitialView = On;
GMAT OpenGL.PerspectiveMode = Off;
GMAT OpenGL.UseFixedFov = Off;
GMAT OpenGL.DataCollectFrequency = 1;
GMAT OpenGL.UpdatePlotFrequency = 50;
GMAT OpenGL.NumPointsToRedraw = 0;
GMAT OpenGL.ShowPlot = true;

Create ReportFile ReportFile1;
GMAT ReportFile1.Filename = './output/SystemTest/SaveAllObjects.report';
GMAT ReportFile1.Precision = 15;
GMAT ReportFile1.Add = {Sat1.A1ModJulian, Sat1.EarthMJ2000Eq.X};
GMAT ReportFile1.WriteHeaders = On;
GMAT ReportFile1.LeftJustify = On;
GMAT ReportFile1.ZeroFill = Off;
GMAT ReportFile1.ColumnWidth = 20;
GMAT ReportFile1.SolverIterations = None;

Create XYPlot XYPlot1;
GMAT XYPlot1.IndVar = Sat1.A1ModJulian;
GMAT XYPlot1.Add = {Sat1.EarthMJ2000Eq.X};
GMAT XYPlot1.Grid = On;
GMAT XYPlot1.SolverIterations = None;
GMAT XYPlot1.ShowPlot = true;


%----------------------------------------
%---------- Functions
%----------------------------------------

Create MatlabFunction mlCall;
GMAT mlCall.FunctionPath = './input/SystemTest/';

Create GmatFunction GMCall;


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------

% First save in one big file
Save Sat1 Sat2 Sat3 Sat4 Sat5 FiniteBurn1 FiniteBurn2 Impulse1 Impulse2 Formation1 ...
     RKVProp RKNProp RKF56Prop PD45Prop PD78Prop BSProp ABMProp DC1 SQP1 ...
     OpenGL ReportFile1 XYPlot1 spiffyNewCS mlCall GMCall rotmat;
%Save fred showMe;

% Now save in separate files
Save Sat1;
Save Sat2;
Save Sat3;
Save Sat4;
Save Sat5;
Save FiniteBurn1;
Save FiniteBurn2;
Save Impulse1;
Save Impulse2;
Save Formation1;
Save RKVProp;
Save RKNProp;
Save RKF56Prop;
Save PD45Prop;
Save PD78Prop;
Save BSProp;
Save ABMProp;
Save DC1;
Save SQP1;
Save OpenGL;
Save ReportFile1;
Save XYPlot1;
%Save fred;
%Save showMe;
Save spiffyNewCS;
Save mlCall;
Save GMCall;
Save rotmat;

% SolarSystem is deferred
% Save SolarSystem;

