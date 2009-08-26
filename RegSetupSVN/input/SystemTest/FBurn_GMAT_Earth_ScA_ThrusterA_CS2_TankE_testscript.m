%-----  Spacecraft A
Create Spacecraft Sc;
GMAT Sc.DateFormat = TAIModJulian;
GMAT Sc.Epoch = '21545.000000000';
GMAT Sc.CoordinateSystem = EarthMJ2000Eq;
GMAT Sc.DisplayStateType = Cartesian;
GMAT Sc.X = 7100;
GMAT Sc.Y = 0;
GMAT Sc.Z = 1300;
GMAT Sc.VX = 0;
GMAT Sc.VY = 7.35;
GMAT Sc.VZ = 1;
GMAT Sc.DryMass = 850;
GMAT Sc.Cd = 2.2;
GMAT Sc.Cr = 1.8;
GMAT Sc.DragArea = 15;
GMAT Sc.SRPArea = 1;
GMAT Sc.Tanks = {tank1}
GMAT Sc.Thrusters = {engine1};
GMAT Sc.Attitude = CoordinateSystemFixed;
GMAT Sc.AttitudeDisplayStateType = 'Quaternion';
GMAT Sc.AttitudeRateDisplayStateType = 'AngularVelocity';
GMAT Sc.AttitudeCoordinateSystem = 'EarthMJ2000Eq';
GMAT Sc.Q1 = 0;
GMAT Sc.Q2 = 0;
GMAT Sc.Q3 = 0;
GMAT Sc.Q4 = 1;
GMAT Sc.EulerAngleSequence = '312';
GMAT Sc.AngularVelocityX = 0;
GMAT Sc.AngularVelocityY = 0;
GMAT Sc.AngularVelocityZ = 0;
 
 
Create CoordinateSystem ScLVLH;
ScLVLH.Axes = ObjectReferenced;
ScLVLH.Origin = Sc;
ScLVLH.Primary = Earth;
ScLVLH.Secondary = Sc;
ScLVLH.ZAxis = R;
ScLVLH.YAxis = -N;
 
%-----  Thruster A;
Create Thruster engine1;
GMAT engine1.CoordinateSystem = ScLVLH;
 
GMAT engine1.Element1 = 1;
GMAT engine1.Element2 = 0;
GMAT engine1.Element3 = 0;
GMAT engine1.DutyCycle = 1;
GMAT engine1.ThrustScaleFactor = 1;
GMAT engine1.DecrementMass = true;
GMAT engine1.Tank = {tank1};
GMAT engine1.GravitationalAccel = 9.81;
GMAT engine1.C1 = 10;
GMAT engine1.C2 = 0.25;
GMAT engine1.C3 = 0.25;
GMAT engine1.C4 = 0;
GMAT engine1.C5 = 0;
GMAT engine1.C6 = 0;
GMAT engine1.C7 = 0;
GMAT engine1.C8 = 0;
GMAT engine1.C9 = 0;
GMAT engine1.C10 = 0;
GMAT engine1.C11 = 0;
GMAT engine1.C12 = 0;
GMAT engine1.C13 = 0;
GMAT engine1.C14 = 0;
GMAT engine1.C15 = 0;
GMAT engine1.C16 = 0;
GMAT engine1.K1 = 300;
GMAT engine1.K2 = 0.25;
GMAT engine1.K3 = 0.25;
GMAT engine1.K4 = 0;
GMAT engine1.K5 = 0;
GMAT engine1.K6 = 0;
GMAT engine1.K7 = 0;
GMAT engine1.K8 = 0;
GMAT engine1.K9 = 0;
GMAT engine1.K10 = 0;
GMAT engine1.K11 = 0;
GMAT engine1.K12 = 0;
GMAT engine1.K13 = 0;
GMAT engine1.K14 = 0;
GMAT engine1.K15 = 0;
GMAT engine1.K16 = 0;
 
%-----  Fuel Tank E
Create FuelTank tank1;
GMAT tank1.FuelMass = 725;
GMAT tank1.Pressure = 1200;
GMAT tank1.Temperature = 200;
GMAT tank1.RefTemperature = 12;
GMAT tank1.Volume = 0.8;
GMAT tank1.FuelDensity = 1029;
GMAT tank1.PressureModel = PressureRegulated;
 
 
% -------------------------------------------------------------------------
% --------------------------- Create Finite Burn  -------------------------
% -------------------------------------------------------------------------
Create FiniteBurn fb;
GMAT fb.Thrusters = {engine1};
 
%  Create a force model
Create ForceModel fm;
GMAT fm.CentralBody = Earth;
GMAT fm.PointMasses = {Earth};
GMAT fm.Drag = None;
GMAT fm.SRP = Off;
GMAT fm.ErrorControl = RSSStep;
 
%  Create a propagator
Create Propagator prop;
GMAT prop.FM = fm;
GMAT prop.Type = RungeKutta89;
GMAT prop.InitialStepSize = 60;
GMAT prop.Accuracy = 1e-9;
GMAT prop.MinStep = 60;
GMAT prop.MaxStep = 60;
GMAT prop.MaxStepAttempts = 50;
 
%  Create a report file
Create ReportFile rf;
GMAT rf.Filename = '.\output\SystemTest\FBurn_GMAT_Earth_ScA_ThrusterA_CS2_TankE_output.report';
GMAT rf.Precision = 16;
GMAT rf.Add = {Sc.TAIModJulian, Sc.X, Sc.Y, Sc.Z, Sc.VX, Sc.VY, Sc.VZ, Sc.TotalMass};
GMAT rf.WriteHeaders = Off;
GMAT rf.ZeroFill = On;
GMAT rf.ColumnWidth = 27;
GMAT rf.LeftJustify = On;
 
% -------------------------------------------------------------------------
% --------------------------- Mission Sequence ----------------------------
% -------------------------------------------------------------------------
 
%  Turn on thrusters....they will remain on through all events until the
%  "EndFiniteBurn fb(Sc)" command is executed.
BeginFiniteBurn fb(Sc);
 
% Propagate for 2 hours, while the thrusters are turned on.
Propagate prop(Sc,{Sc.ElapsedSecs = 7200});
 
%  Turn off thrusters   
EndFiniteBurn fb(Sc);
 
 
