%% $Id: DeltaV_GMAT_IMars_Cartesian.m,v 1.3 2007/07/26 19:12:27 edove Exp $

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = TAIModJulian;
GMAT DefaultSC.Epoch = 21545;
GMAT DefaultSC.CoordinateSystem = MarsMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = 4500;
GMAT DefaultSC.Y = 0;
GMAT DefaultSC.Z = 0;
GMAT DefaultSC.VX = 0;
GMAT DefaultSC.VY = 2.1814448386859766;
GMAT DefaultSC.VZ = 2.1814448386859713;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;

Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Mars;
GMAT DefaultProp_ForceModel.PointMasses = {Mars};
GMAT DefaultProp_ForceModel.Drag = None;
GMAT DefaultProp_ForceModel.SRP = Off;
GMAT DefaultProp_ForceModel.ErrorControl = RSSStep;


Create Propagator DefaultProp;
GMAT DefaultProp.FM = DefaultProp_ForceModel;
GMAT DefaultProp.Type = RungeKutta89;
GMAT DefaultProp.InitialStepSize = 60;
GMAT DefaultProp.Accuracy = 1e-013;
GMAT DefaultProp.MinStep = 0.001;
GMAT DefaultProp.MaxStep = 2700;
GMAT DefaultProp.MaxStepAttempts = 50;


Create ImpulsiveBurn ImpulsiveBurn1;
GMAT ImpulsiveBurn1.Origin = Mars;
GMAT ImpulsiveBurn1.Axes = MJ2000Eq;
GMAT ImpulsiveBurn1.Element1 = 0.1;
GMAT ImpulsiveBurn1.Element2 = 0.1;
GMAT ImpulsiveBurn1.Element3 = 0.1;


Create ReportFile DeltaV_Report;
GMAT DeltaV_Report.Filename = ./output/AcceptTest/DeltaV_GMAT_IMars_Cartesian.report;
GMAT DeltaV_Report.Precision = 16;
GMAT DeltaV_Report.WriteHeaders = On;
GMAT DeltaV_Report.LeftJustify = On;
GMAT DeltaV_Report.ZeroFill = Off;
GMAT DeltaV_Report.ColumnWidth = 20;


Create CoordinateSystem MarsMJ2000Eq;
GMAT MarsMJ2000Eq.Origin = Mars;
GMAT MarsMJ2000Eq.Axes   = MJ2000Eq;


Report DeltaV_Report DefaultSC.A1ModJulian DefaultSC.MarsMJ2000Eq.X DefaultSC.MarsMJ2000Eq.Y DefaultSC.MarsMJ2000Eq.Z DefaultSC.MarsMJ2000Eq.VX DefaultSC.MarsMJ2000Eq.VY DefaultSC.MarsMJ2000Eq.VZ;
Maneuver ImpulsiveBurn1(DefaultSC);
GMAT DeltaV_Report.WriteHeaders = Off;
Report DeltaV_Report DefaultSC.A1ModJulian DefaultSC.MarsMJ2000Eq.X DefaultSC.MarsMJ2000Eq.Y DefaultSC.MarsMJ2000Eq.Z DefaultSC.MarsMJ2000Eq.VX DefaultSC.MarsMJ2000Eq.VY DefaultSC.MarsMJ2000Eq.VZ;