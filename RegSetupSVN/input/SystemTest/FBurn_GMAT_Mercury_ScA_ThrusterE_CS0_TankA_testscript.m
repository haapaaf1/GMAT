%-----  Spacecraft A
Create Spacecraft Sc;
GMAT Sc.DateFormat = TAIModJulian;
GMAT Sc.Epoch = '21545.000000000';
GMAT Sc.CoordinateSystem = MercuryMJ2000Eq;
GMAT Sc.DisplayStateType = Keplerian;
GMAT Sc.SMA = 3440;
GMAT Sc.ECC = 0.0009999999999957077;
GMAT Sc.INC = 12.84999999999996;
GMAT Sc.RAAN = 306.6100000000001;
GMAT Sc.AOP = 314.1900000002609;
GMAT Sc.TA = 99.88769999973877;
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
GMAT Sc.AttitudeCoordinateSystem = 'MercuryMJ2000Eq';
GMAT Sc.Q1 = 0;
GMAT Sc.Q2 = 0;
GMAT Sc.Q3 = 0;
GMAT Sc.Q4 = 1;
GMAT Sc.EulerAngleSequence = '312';
GMAT Sc.AngularVelocityX = 0;
GMAT Sc.AngularVelocityY = 0;
GMAT Sc.AngularVelocityZ = 0;
 
 
%-----  Thruster E;
Create Thruster engine1;
GMAT engine1.CoordinateSystem = MercuryMJ2000Eq;
 
GMAT engine1.Element1 = 1;
GMAT engine1.Element2 = 0;
GMAT engine1.Element3 = 0;
GMAT engine1.DutyCycle = 1;
GMAT engine1.ThrustScaleFactor = 1;
GMAT engine1.DecrementMass = true;
GMAT engine1.Tank = {tank1};
GMAT engine1.GravitationalAccel = 3.14;
GMAT engine1.C1 = 10;
GMAT engine1.C2 = 0;
GMAT engine1.C3 = 0;
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
GMAT engine1.K2 = 0;
GMAT engine1.K3 = 0;
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
 
%-----  Fuel Tank A
Create FuelTank tank1;
GMAT tank1.FuelMass = 725;
GMAT tank1.Pressure = 1200;
GMAT tank1.Temperature = 20;
GMAT tank1.RefTemperature = 12;
GMAT tank1.Volume = 0.8;
GMAT tank1.FuelDensity = 1029;
GMAT tank1.PressureModel = PressureRegulated;
 
 
% -------------------------------------------------------------------------
% -------------------- Create Coordinate Systems  -------------------------
% -------------------------------------------------------------------------
Create CoordinateSystem MercuryMJ2000Eq;
GMAT MercuryMJ2000Eq.Origin = Mercury;
GMAT MercuryMJ2000Eq.Axes = MJ2000Eq;
GMAT MercuryMJ2000Eq.UpdateInterval = 60;
GMAT MercuryMJ2000Eq.OverrideOriginInterval = false;
 
% -------------------------------------------------------------------------
% --------------------------- Create Finite Burn  -------------------------
% -------------------------------------------------------------------------
Create FiniteBurn fb;
GMAT fb.Thrusters = {engine1};
 
%  Create a force model
Create ForceModel fm;
GMAT fm.CentralBody = Mercury;
GMAT fm.PointMasses = {Mercury};
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
GMAT rf.Filename = '.\output\SystemTest\FBurn_GMAT_Mercury_ScA_ThrusterE_CS0_TankA_output.report';
GMAT rf.Precision = 16;
GMAT rf.Add = {Sc.TAIModJulian, Sc.Mercury.SMA, Sc.Mercury.ECC, Sc.MercuryMJ2000Eq.INC, Sc.MercuryMJ2000Eq.RAAN, Sc.MercuryMJ2000Eq.AOP, Sc.Mercury.TA, Sc.TotalMass};
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
 
 
