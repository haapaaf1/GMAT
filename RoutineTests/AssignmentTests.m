%$Id: AssignmentTests.m,v 1.1 2008/09/25 19:03:36 ljun Exp $ 

%----------------------------------------
%---------- Spacecraft
%----------------------------------------

Create Spacecraft Sat1;
GMAT Sat1.DateFormat = TAIModJulian;
GMAT Sat1.Epoch = 21545.000000000;
GMAT Sat1.CoordinateSystem = EarthMJ2000Eq;
GMAT Sat1.DisplayStateType = Cartesian;
GMAT Sat1.X = 42165;
GMAT Sat1.Y = 0;
GMAT Sat1.Z = 100;
GMAT Sat1.VX = 0;
GMAT Sat1.VY = 3.35;
GMAT Sat1.VZ = 0;
GMAT Sat1.DryMass = 850;
GMAT Sat1.Cd = 2.2;
GMAT Sat1.Cr = 1.8;
GMAT Sat1.DragArea = 15;
GMAT Sat1.SRPArea = 1;

Create Spacecraft Sat2;
GMAT Sat2.DateFormat = TAIModJulian;
GMAT Sat2.Epoch = 21545.000000000;
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

Create Spacecraft hwSat1;
GMAT hwSat1.DateFormat = TAIModJulian;
GMAT hwSat1.Epoch = 21545.000000000;
GMAT hwSat1.CoordinateSystem = EarthMJ2000Eq;
GMAT hwSat1.DisplayStateType = Cartesian;
GMAT hwSat1.X = 42165;
GMAT hwSat1.Y = 0;
GMAT hwSat1.Z = 100;
GMAT hwSat1.VX = 0;
GMAT hwSat1.VY = 3.35;
GMAT hwSat1.VZ = 1;
GMAT hwSat1.DryMass = 850;
GMAT hwSat1.Cd = 2.2;
GMAT hwSat1.Cr = 1.8;
GMAT hwSat1.DragArea = 15;
GMAT hwSat1.SRPArea = 1;

Create Spacecraft hwSat2;
GMAT hwSat2.DateFormat = TAIModJulian;
GMAT hwSat2.Epoch = 21545.000000000;
GMAT hwSat2.CoordinateSystem = EarthMJ2000Eq;
GMAT hwSat2.DisplayStateType = Cartesian;
GMAT hwSat2.X = 7100;
GMAT hwSat2.Y = 0;
GMAT hwSat2.Z = 1300;
GMAT hwSat2.VX = 0;
GMAT hwSat2.VY = 7.35;
GMAT hwSat2.VZ = 1;
GMAT hwSat2.DryMass = 850;
GMAT hwSat2.Cd = 2.2;
GMAT hwSat2.Cr = 1.8;
GMAT hwSat2.DragArea = 15;
GMAT hwSat2.SRPArea = 1;


%----------------------------------------
%---------- Hardware Components
%----------------------------------------

Create FuelTank FuelTank1;
GMAT FuelTank1.FuelMass = 756;
GMAT FuelTank1.Pressure = 1500;
GMAT FuelTank1.Temperature = 20;
GMAT FuelTank1.RefTemperature = 20;
GMAT FuelTank1.Volume = 0.75;
GMAT FuelTank1.FuelDensity = 1260;
GMAT FuelTank1.PressureModel = PressureRegulated;

Create Thruster Thruster1;
GMAT Thruster1.ThrustDirection1 = 1;
GMAT Thruster1.ThrustDirection2 = 0;
GMAT Thruster1.ThrustDirection3 = 0;
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
GMAT Thruster1.CoordinateSystem = EarthMJ2000Eq;
GMAT Thruster1.ThrustScaleFactor = 1;


%----------------------------------------
%---------- Propagators
%----------------------------------------

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Earth;
GMAT DefaultProp_ForceModel.PrimaryBodies = {Earth};
GMAT DefaultProp_ForceModel.Drag = None;
GMAT DefaultProp_ForceModel.SRP = Off;
GMAT DefaultProp_ForceModel.ErrorControl = RSSStep;
GMAT DefaultProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT DefaultProp_ForceModel.Gravity.Earth.Order = 4;
GMAT DefaultProp_ForceModel.Gravity.Earth.PotentialFile = JGM2.cof;

Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 60;
GMAT DefaultProp.Accuracy = 9.999999999999999e-012;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;


%----------------------------------------
%---------- Burns
%----------------------------------------

Create ImpulsiveBurn DefaultIB;
GMAT DefaultIB.Origin = Earth;
GMAT DefaultIB.Axes = VNB;
GMAT DefaultIB.Element1 = 0;
GMAT DefaultIB.Element2 = 0;
GMAT DefaultIB.Element3 = 0;


%----------------------------------------
%---------- Parameters
%----------------------------------------

Create Variable var1;
GMAT var1 = 12;

Create Variable var2;
GMAT var2 = 0;

Create Variable temp1 temp2 temp3 temp4;

Create Array array1[3, 3];
GMAT array1(1, 1) = 10;
GMAT array1(1, 2) = 20;
GMAT array1(1, 3) = 30;
GMAT array1(2, 1) = 40;
GMAT array1(2, 2) = 50;
GMAT array1(2, 3) = 60;
GMAT array1(3, 1) = 70;
GMAT array1(3, 2) = 80;
GMAT array1(3, 3) = 90;

Create Array array2[3, 3];
GMAT array2(1, 1) = 0;
GMAT array2(1, 2) = 0;
GMAT array2(1, 3) = 0;
GMAT array2(2, 1) = 0;
GMAT array2(2, 2) = 0;
GMAT array2(2, 3) = 0;
GMAT array2(3, 1) = 0;
GMAT array2(3, 2) = 0;
GMAT array2(3, 3) = 0;

Create String string1;
GMAT string1 = Here is a test string;
Create String string2;
GMAT string2 = TTModJulian


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


%----------------------------------------
%---------- Mission Sequence
%----------------------------------------

BeginMissionSequence;

BeginScript
   % Parm = ... tests
   GMAT Sat2.Cr = 2.0;
   GMAT Sat1.Cd = Sat2.Cr; % This line causes problems with scripts that run after it
   GMAT hwSat1.Cd = array1(1,1);
   GMAT Sat1.DateFormat = string2;
   
   % Object = Object tests
   GMAT Sat2 = Sat1;
   GMAT hwSat2 = hwSat1;
   GMAT var2 = var1;
   GMAT array2 = array1;
   GMAT string2 = string1;
   
   % Variable = ... tests
   GMAT temp1 = 12345.6789098765;
   GMAT temp2 = Sat1.SMA;
   GMAT temp3 = temp2;
   GMAT temp4 = array1(3,3);
   
   % Array element = ... tests
   GMAT array1(1,1) = 9876.54321012345;
   GMAT array1(1,2) = Sat1.SMA;
   GMAT array1(1,3) = temp3;
   GMAT array1(2,1) = array1(1,3);
   
   Save Sat1 Sat2
   Save hwSat1 hwSat2;
   Save var1 var2;
   Save array1 array2;
   Save string1 string2;
   Save temp1 temp2 temp3 temp4;
EndScript

