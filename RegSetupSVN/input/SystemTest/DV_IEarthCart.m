%% $Id: DV_IEarthCart.m,v 1.3 2007/11/20 22:37:14 edove Exp $

Create Spacecraft DefaultSC;
GMAT DefaultSC.DateFormat = TAIModJulian;
GMAT DefaultSC.Epoch = 21545.000000000;
GMAT DefaultSC.CoordinateSystem = EarthMJ2000Eq;
GMAT DefaultSC.DisplayStateType = Cartesian;
GMAT DefaultSC.X = 7378;
GMAT DefaultSC.Y = 0;
GMAT DefaultSC.Z = 0;
GMAT DefaultSC.VX = 0;
GMAT DefaultSC.VY = 5.1973811193846027;
GMAT DefaultSC.VZ = 5.1973811193846018;
GMAT DefaultSC.DryMass = 850;
GMAT DefaultSC.Cd = 2.2;
GMAT DefaultSC.Cr = 1.8;
GMAT DefaultSC.DragArea = 15;
GMAT DefaultSC.SRPArea = 1;


Create ForceModel DefaultProp_ForceModel;
GMAT DefaultProp_ForceModel.CentralBody = Earth;
GMAT DefaultProp_ForceModel.PointMasses  = {Earth};
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
GMAT ImpulsiveBurn1.Origin = Earth;
GMAT ImpulsiveBurn1.Axes = MJ2000Eq;
GMAT ImpulsiveBurn1.Element1 = 0.1;
GMAT ImpulsiveBurn1.Element2 = 0.1;
GMAT ImpulsiveBurn1.Element3 = 0.1;

Create ReportFile DeltaV_Report
GMAT DeltaV_Report.Filename = ./output/SystemTest/DV_IEarthCart.report;
GMAT DeltaV_Report.Precision = 16;
GMAT DeltaV_Report.WriteHeaders = On;
GMAT DeltaV_Report.ColumnWidth = 20;

Report DeltaV_Report DefaultSC.A1ModJulian DefaultSC.X DefaultSC.Y DefaultSC.Z DefaultSC.VX DefaultSC.VY DefaultSC.VZ;
Maneuver ImpulsiveBurn1(DefaultSC);
GMAT DeltaV_Report.WriteHeaders = Off;
Report DeltaV_Report DefaultSC.A1ModJulian DefaultSC.X DefaultSC.Y DefaultSC.Z DefaultSC.VX DefaultSC.VY DefaultSC.VZ;