%% $Id: CSParams_GMAT_GEO_2Body.m,v 1.4 2007/07/26 19:12:26 edove Exp $

Create Spacecraft GEO;
GMAT GEO.J2000BodyName = Earth;
GMAT GEO.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT GEO.DisplayStateType = Cartesian;
GMAT GEO.CoordinateSystem = EarthMJ2000Eq;
GMAT GEO.X = 36607.3582560;
GMAT GEO.Y = -20921.723703;
GMAT GEO.Z = 0.0;
GMAT GEO.VX = 1.52563600;
GMAT GEO.VY = 2.66945100;
GMAT GEO.VZ = 0.0;
GMAT GEO.Cd = 2.2;
GMAT GEO.Cr = 1.2;
GMAT GEO.DragArea = 20;
GMAT GEO.SRPArea = 20;
GMAT GEO.DryMass = 1000;
GMAT GEO.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.PointMasses   = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 60;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 1e-7;
GMAT RKV89.MaxStep = 60;
GMAT RKV89.MaxStepAttempts = 50; 
 
GMAT SolarSystem.EphemerisUpdateInterval = 0.0;
GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0; 

Create CoordinateSystem EarthMODEc;
GMAT EarthMODEc.Origin = Earth;
GMAT EarthMODEc.J2000Body = Earth;
GMAT EarthMODEc.Axes = MODEc;
GMAT EarthMODEc.Epoch = 21545.000000397937;
GMAT EarthMODEc.UpdateInterval = 60;

Create CoordinateSystem EarthMODEq;
GMAT EarthMODEq.Origin = Earth;
GMAT EarthMODEq.J2000Body = Earth;
GMAT EarthMODEq.Axes = MODEq;
GMAT EarthMODEq.Epoch = 21545.000000397937;
GMAT EarthMODEq.UpdateInterval = 60;

Create CoordinateSystem EarthMOEEc;
GMAT EarthMOEEc.Origin = Earth;
GMAT EarthMOEEc.J2000Body = Earth;
GMAT EarthMOEEc.Axes = MOEEc;
GMAT EarthMOEEc.Epoch = 21544.99962789831;
GMAT EarthMOEEc.UpdateInterval = 60;

Create CoordinateSystem EarthMOEEq;
GMAT EarthMOEEq.Origin = Earth;
GMAT EarthMOEEq.J2000Body = Earth;
GMAT EarthMOEEq.Axes = MOEEq;
GMAT EarthMOEEq.Epoch = 21544.99962789831;
GMAT EarthMOEEq.UpdateInterval = 60;

Create CoordinateSystem EarthTODEc;
GMAT EarthTODEc.Origin = Earth;
GMAT EarthTODEc.J2000Body = Earth;
GMAT EarthTODEc.Axes = TODEc;
GMAT EarthTODEc.Epoch = 21545.000000397937;
GMAT EarthTODEc.UpdateInterval = 60;

Create CoordinateSystem EarthTODEq;
GMAT EarthTODEq.Origin = Earth;
GMAT EarthTODEq.J2000Body = Earth;
GMAT EarthTODEq.Axes = TODEq;
GMAT EarthTODEq.Epoch = 21545.000000397937;
GMAT EarthTODEq.UpdateInterval = 60;

Create CoordinateSystem EarthTOEEc;
GMAT EarthTOEEc.Origin = Earth;
GMAT EarthTOEEc.J2000Body = Earth;
GMAT EarthTOEEc.Axes = TOEEc;
GMAT EarthTOEEc.Epoch = 21544.99962789831;
GMAT EarthTOEEc.UpdateInterval = 60;

Create CoordinateSystem EarthTOEEq;
GMAT EarthTOEEq.Origin = Earth;
GMAT EarthTOEEq.J2000Body = Earth;
GMAT EarthTOEEq.Axes = TOEEq;
GMAT EarthTOEEq.Epoch = 21544.99962789831;
GMAT EarthTOEEq.UpdateInterval = 60;

Create ReportFile GEO_Report
GMAT GEO_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthFixed.report;
GMAT GEO_Report.Precision = 16;
GMAT GEO_Report.WriteHeaders = On;
GMAT GEO_Report.ColumnWidth = 20;

Create ReportFile GEO2_Report
GMAT GEO2_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthMJ2000Ec.report;
GMAT GEO2_Report.Precision = 16;
GMAT GEO2_Report.WriteHeaders = On;
GMAT GEO2_Report.ColumnWidth = 20;

Create ReportFile GEO3_Report
GMAT GEO3_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthMJ2000Eq.report;
GMAT GEO3_Report.Precision = 16;
GMAT GEO3_Report.WriteHeaders = On;
GMAT GEO3_Report.ColumnWidth = 20;

Create ReportFile GEO4_Report
GMAT GEO4_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthMODEc.report;
GMAT GEO4_Report.Precision = 16;
GMAT GEO4_Report.WriteHeaders = On;
GMAT GEO4_Report.ColumnWidth = 20;

Create ReportFile GEO5_Report
GMAT GEO5_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthMODEq.report;
GMAT GEO5_Report.Precision = 16;
GMAT GEO5_Report.WriteHeaders = On;
GMAT GEO5_Report.ColumnWidth = 20;

Create ReportFile GEO6_Report
GMAT GEO6_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthMOEEc.report;
GMAT GEO6_Report.Precision = 16;
GMAT GEO6_Report.WriteHeaders = On;
GMAT GEO6_Report.ColumnWidth = 20;

Create ReportFile GEO7_Report
GMAT GEO7_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthMOEEq.report;
GMAT GEO7_Report.Precision = 16;
GMAT GEO7_Report.WriteHeaders = On;
GMAT GEO7_Report.ColumnWidth = 20;

Create ReportFile GEO8_Report
GMAT GEO8_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthTODEc.report;
GMAT GEO8_Report.Precision = 16;
GMAT GEO8_Report.WriteHeaders = On;
GMAT GEO8_Report.ColumnWidth = 20;

Create ReportFile GEO9_Report
GMAT GEO9_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthTODEq.report;
GMAT GEO9_Report.Precision = 16;
GMAT GEO9_Report.WriteHeaders = On;
GMAT GEO9_Report.ColumnWidth = 20;

Create ReportFile GEO10_Report
GMAT GEO10_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthTOEEc.report;
GMAT GEO10_Report.Precision = 16;
GMAT GEO10_Report.WriteHeaders = On;
GMAT GEO10_Report.ColumnWidth = 20;

Create ReportFile GEO11_Report
GMAT GEO11_Report.Filename = ./output/AcceptTest/CSParams_GMAT_GEO_2Body_EarthTOEEq.report;
GMAT GEO11_Report.Precision = 16;
GMAT GEO11_Report.WriteHeaders = On;
GMAT GEO11_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      GEO_Report GEO.A1ModJulian GEO.EarthFixed.X GEO.EarthFixed.Y GEO.EarthFixed.Z GEO.EarthFixed.VX GEO.EarthFixed.VY GEO.EarthFixed.VZ GEO.EarthFixed.VMAG GEO.EarthFixed.RAV GEO.EarthFixed.HX GEO.EarthFixed.HY GEO.EarthFixed.HZ GEO.EarthFixed.AOP GEO.EarthFixed.DEC GEO.EarthFixed.DECV GEO.EarthFixed.INC GEO.EarthFixed.RA GEO.EarthFixed.RAAN;
Report      GEO2_Report GEO.A1ModJulian GEO.EarthMJ2000Ec.X GEO.EarthMJ2000Ec.Y GEO.EarthMJ2000Ec.Z GEO.EarthMJ2000Ec.VX GEO.EarthMJ2000Ec.VY GEO.EarthMJ2000Ec.VZ GEO.EarthMJ2000Ec.VMAG GEO.EarthMJ2000Ec.RAV GEO.EarthMJ2000Ec.HX GEO.EarthMJ2000Ec.HY GEO.EarthMJ2000Ec.HZ GEO.EarthMJ2000Ec.AOP GEO.EarthMJ2000Ec.DEC GEO.EarthMJ2000Ec.DECV GEO.EarthMJ2000Ec.INC GEO.EarthMJ2000Ec.RA GEO.EarthMJ2000Ec.RAAN;
Report      GEO3_Report GEO.A1ModJulian GEO.EarthMJ2000Eq.X GEO.EarthMJ2000Eq.Y GEO.EarthMJ2000Eq.Z GEO.EarthMJ2000Eq.VX GEO.EarthMJ2000Eq.VY GEO.EarthMJ2000Eq.VZ GEO.EarthMJ2000Eq.VMAG GEO.EarthMJ2000Eq.RAV GEO.EarthMJ2000Eq.HX GEO.EarthMJ2000Eq.HY GEO.EarthMJ2000Eq.HZ GEO.EarthMJ2000Eq.AOP GEO.EarthMJ2000Eq.DEC GEO.EarthMJ2000Eq.DECV GEO.EarthMJ2000Eq.INC GEO.EarthMJ2000Eq.RA GEO.EarthMJ2000Eq.RAAN;
Report      GEO4_Report GEO.A1ModJulian GEO.EarthMODEc.X GEO.EarthMODEc.Y GEO.EarthMODEc.Z GEO.EarthMODEc.VX GEO.EarthMODEc.VY GEO.EarthMODEc.VZ GEO.EarthMODEc.VMAG GEO.EarthMODEc.RAV GEO.EarthMODEc.HX GEO.EarthMODEc.HY GEO.EarthMODEc.HZ GEO.EarthMODEc.AOP GEO.EarthMODEc.DEC GEO.EarthMODEc.DECV GEO.EarthMODEc.INC GEO.EarthMODEc.RA GEO.EarthMODEc.RAAN;
Report      GEO5_Report GEO.A1ModJulian GEO.EarthMODEq.X GEO.EarthMODEq.Y GEO.EarthMODEq.Z GEO.EarthMODEq.VX GEO.EarthMODEq.VY GEO.EarthMODEq.VZ GEO.EarthMODEq.VMAG GEO.EarthMODEq.RAV GEO.EarthMODEq.HX GEO.EarthMODEq.HY GEO.EarthMODEq.HZ GEO.EarthMODEq.AOP GEO.EarthMODEq.DEC GEO.EarthMODEq.DECV GEO.EarthMODEq.INC GEO.EarthMODEq.RA GEO.EarthMODEq.RAAN;
Report      GEO6_Report GEO.A1ModJulian GEO.EarthMOEEc.X GEO.EarthMOEEc.Y GEO.EarthMOEEc.Z GEO.EarthMOEEc.VX GEO.EarthMOEEc.VY GEO.EarthMOEEc.VZ GEO.EarthMOEEc.VMAG GEO.EarthMOEEc.RAV GEO.EarthMOEEc.HX GEO.EarthMOEEc.HY GEO.EarthMOEEc.HZ GEO.EarthMOEEc.AOP GEO.EarthMOEEc.DEC GEO.EarthMOEEc.DECV GEO.EarthMOEEc.INC GEO.EarthMOEEc.RA GEO.EarthMOEEc.RAAN;
Report      GEO7_Report GEO.A1ModJulian GEO.EarthMOEEq.X GEO.EarthMOEEq.Y GEO.EarthMOEEq.Z GEO.EarthMOEEq.VX GEO.EarthMOEEq.VY GEO.EarthMOEEq.VZ GEO.EarthMOEEq.VMAG GEO.EarthMOEEq.RAV GEO.EarthMOEEq.HX GEO.EarthMOEEq.HY GEO.EarthMOEEq.HZ GEO.EarthMOEEq.AOP GEO.EarthMOEEq.DEC GEO.EarthMOEEq.DECV GEO.EarthMOEEq.INC GEO.EarthMOEEq.RA GEO.EarthMOEEq.RAAN;
Report      GEO8_Report GEO.A1ModJulian GEO.EarthTODEc.X GEO.EarthTODEc.Y GEO.EarthTODEc.Z GEO.EarthTODEc.VX GEO.EarthTODEc.VY GEO.EarthTODEc.VZ GEO.EarthTODEc.VMAG GEO.EarthTODEc.RAV GEO.EarthTODEc.HX GEO.EarthTODEc.HY GEO.EarthTODEc.HZ GEO.EarthTODEc.AOP GEO.EarthTODEc.DEC GEO.EarthTODEc.DECV GEO.EarthTODEc.INC GEO.EarthTODEc.RA GEO.EarthTODEc.RAAN;
Report      GEO9_Report GEO.A1ModJulian GEO.EarthTODEq.X GEO.EarthTODEq.Y GEO.EarthTODEq.Z GEO.EarthTODEq.VX GEO.EarthTODEq.VY GEO.EarthTODEq.VZ GEO.EarthTODEq.VMAG GEO.EarthTODEq.RAV GEO.EarthTODEq.HX GEO.EarthTODEq.HY GEO.EarthTODEq.HZ GEO.EarthTODEq.AOP GEO.EarthTODEq.DEC GEO.EarthTODEq.DECV GEO.EarthTODEq.INC GEO.EarthTODEq.RA GEO.EarthTODEq.RAAN;
Report      GEO10_Report GEO.A1ModJulian GEO.EarthTOEEc.X GEO.EarthTOEEc.Y GEO.EarthTOEEc.Z GEO.EarthTOEEc.VX GEO.EarthTOEEc.VY GEO.EarthTOEEc.VZ GEO.EarthTOEEc.VMAG GEO.EarthTOEEc.RAV GEO.EarthTOEEc.HX GEO.EarthTOEEc.HY GEO.EarthTOEEc.HZ GEO.EarthTOEEc.AOP GEO.EarthTOEEc.DEC GEO.EarthTOEEc.DECV GEO.EarthTOEEc.INC GEO.EarthTOEEc.RA GEO.EarthTOEEc.RAAN;
Report      GEO11_Report GEO.A1ModJulian GEO.EarthTOEEq.X GEO.EarthTOEEq.Y GEO.EarthTOEEq.Z GEO.EarthTOEEq.VX GEO.EarthTOEEq.VY GEO.EarthTOEEq.VZ GEO.EarthTOEEq.VMAG GEO.EarthTOEEq.RAV GEO.EarthTOEEq.HX GEO.EarthTOEEq.HY GEO.EarthTOEEq.HZ GEO.EarthTOEEq.AOP GEO.EarthTOEEq.DEC GEO.EarthTOEEq.DECV GEO.EarthTOEEq.INC GEO.EarthTOEEq.RA GEO.EarthTOEEq.RAAN;
GMAT GEO_Report.WriteHeaders = Off;
GMAT GEO2_Report.WriteHeaders = Off;
GMAT GEO3_Report.WriteHeaders = Off;
GMAT GEO4_Report.WriteHeaders = Off;
GMAT GEO5_Report.WriteHeaders = Off;
GMAT GEO6_Report.WriteHeaders = Off;
GMAT GEO7_Report.WriteHeaders = Off;
GMAT GEO8_Report.WriteHeaders = Off;
GMAT GEO9_Report.WriteHeaders = Off;
GMAT GEO10_Report.WriteHeaders = Off;
GMAT GEO11_Report.WriteHeaders = Off;
For OutputStepSize = 1:1008;
	Propagate   RKV89(GEO, {GEO.ElapsedSecs = 600});
	Report      GEO_Report GEO.A1ModJulian GEO.EarthFixed.X GEO.EarthFixed.Y GEO.EarthFixed.Z GEO.EarthFixed.VX GEO.EarthFixed.VY GEO.EarthFixed.VZ GEO.EarthFixed.VMAG GEO.EarthFixed.RAV GEO.EarthFixed.HX GEO.EarthFixed.HY GEO.EarthFixed.HZ GEO.EarthFixed.AOP GEO.EarthFixed.DEC GEO.EarthFixed.DECV GEO.EarthFixed.INC GEO.EarthFixed.RA GEO.EarthFixed.RAAN;
	Report      GEO2_Report GEO.A1ModJulian GEO.EarthMJ2000Ec.X GEO.EarthMJ2000Ec.Y GEO.EarthMJ2000Ec.Z GEO.EarthMJ2000Ec.VX GEO.EarthMJ2000Ec.VY GEO.EarthMJ2000Ec.VZ GEO.EarthMJ2000Ec.VMAG GEO.EarthMJ2000Ec.RAV GEO.EarthMJ2000Ec.HX GEO.EarthMJ2000Ec.HY GEO.EarthMJ2000Ec.HZ GEO.EarthMJ2000Ec.AOP GEO.EarthMJ2000Ec.DEC GEO.EarthMJ2000Ec.DECV GEO.EarthMJ2000Ec.INC GEO.EarthMJ2000Ec.RA GEO.EarthMJ2000Ec.RAAN;
	Report      GEO3_Report GEO.A1ModJulian GEO.EarthMJ2000Eq.X GEO.EarthMJ2000Eq.Y GEO.EarthMJ2000Eq.Z GEO.EarthMJ2000Eq.VX GEO.EarthMJ2000Eq.VY GEO.EarthMJ2000Eq.VZ GEO.EarthMJ2000Eq.VMAG GEO.EarthMJ2000Eq.RAV GEO.EarthMJ2000Eq.HX GEO.EarthMJ2000Eq.HY GEO.EarthMJ2000Eq.HZ GEO.EarthMJ2000Eq.AOP GEO.EarthMJ2000Eq.DEC GEO.EarthMJ2000Eq.DECV GEO.EarthMJ2000Eq.INC GEO.EarthMJ2000Eq.RA GEO.EarthMJ2000Eq.RAAN;
	Report      GEO4_Report GEO.A1ModJulian GEO.EarthMODEc.X GEO.EarthMODEc.Y GEO.EarthMODEc.Z GEO.EarthMODEc.VX GEO.EarthMODEc.VY GEO.EarthMODEc.VZ GEO.EarthMODEc.VMAG GEO.EarthMODEc.RAV GEO.EarthMODEc.HX GEO.EarthMODEc.HY GEO.EarthMODEc.HZ GEO.EarthMODEc.AOP GEO.EarthMODEc.DEC GEO.EarthMODEc.DECV GEO.EarthMODEc.INC GEO.EarthMODEc.RA GEO.EarthMODEc.RAAN;
	Report      GEO5_Report GEO.A1ModJulian GEO.EarthMODEq.X GEO.EarthMODEq.Y GEO.EarthMODEq.Z GEO.EarthMODEq.VX GEO.EarthMODEq.VY GEO.EarthMODEq.VZ GEO.EarthMODEq.VMAG GEO.EarthMODEq.RAV GEO.EarthMODEq.HX GEO.EarthMODEq.HY GEO.EarthMODEq.HZ GEO.EarthMODEq.AOP GEO.EarthMODEq.DEC GEO.EarthMODEq.DECV GEO.EarthMODEq.INC GEO.EarthMODEq.RA GEO.EarthMODEq.RAAN;
	Report      GEO6_Report GEO.A1ModJulian GEO.EarthMOEEc.X GEO.EarthMOEEc.Y GEO.EarthMOEEc.Z GEO.EarthMOEEc.VX GEO.EarthMOEEc.VY GEO.EarthMOEEc.VZ GEO.EarthMOEEc.VMAG GEO.EarthMOEEc.RAV GEO.EarthMOEEc.HX GEO.EarthMOEEc.HY GEO.EarthMOEEc.HZ GEO.EarthMOEEc.AOP GEO.EarthMOEEc.DEC GEO.EarthMOEEc.DECV GEO.EarthMOEEc.INC GEO.EarthMOEEc.RA GEO.EarthMOEEc.RAAN;
	Report      GEO7_Report GEO.A1ModJulian GEO.EarthMOEEq.X GEO.EarthMOEEq.Y GEO.EarthMOEEq.Z GEO.EarthMOEEq.VX GEO.EarthMOEEq.VY GEO.EarthMOEEq.VZ GEO.EarthMOEEq.VMAG GEO.EarthMOEEq.RAV GEO.EarthMOEEq.HX GEO.EarthMOEEq.HY GEO.EarthMOEEq.HZ GEO.EarthMOEEq.AOP GEO.EarthMOEEq.DEC GEO.EarthMOEEq.DECV GEO.EarthMOEEq.INC GEO.EarthMOEEq.RA GEO.EarthMOEEq.RAAN;
	Report      GEO8_Report GEO.A1ModJulian GEO.EarthTODEc.X GEO.EarthTODEc.Y GEO.EarthTODEc.Z GEO.EarthTODEc.VX GEO.EarthTODEc.VY GEO.EarthTODEc.VZ GEO.EarthTODEc.VMAG GEO.EarthTODEc.RAV GEO.EarthTODEc.HX GEO.EarthTODEc.HY GEO.EarthTODEc.HZ GEO.EarthTODEc.AOP GEO.EarthTODEc.DEC GEO.EarthTODEc.DECV GEO.EarthTODEc.INC GEO.EarthTODEc.RA GEO.EarthTODEc.RAAN;
	Report      GEO9_Report GEO.A1ModJulian GEO.EarthTODEq.X GEO.EarthTODEq.Y GEO.EarthTODEq.Z GEO.EarthTODEq.VX GEO.EarthTODEq.VY GEO.EarthTODEq.VZ GEO.EarthTODEq.VMAG GEO.EarthTODEq.RAV GEO.EarthTODEq.HX GEO.EarthTODEq.HY GEO.EarthTODEq.HZ GEO.EarthTODEq.AOP GEO.EarthTODEq.DEC GEO.EarthTODEq.DECV GEO.EarthTODEq.INC GEO.EarthTODEq.RA GEO.EarthTODEq.RAAN;
	Report      GEO10_Report GEO.A1ModJulian GEO.EarthTOEEc.X GEO.EarthTOEEc.Y GEO.EarthTOEEc.Z GEO.EarthTOEEc.VX GEO.EarthTOEEc.VY GEO.EarthTOEEc.VZ GEO.EarthTOEEc.VMAG GEO.EarthTOEEc.RAV GEO.EarthTOEEc.HX GEO.EarthTOEEc.HY GEO.EarthTOEEc.HZ GEO.EarthTOEEc.AOP GEO.EarthTOEEc.DEC GEO.EarthTOEEc.DECV GEO.EarthTOEEc.INC GEO.EarthTOEEc.RA GEO.EarthTOEEc.RAAN;
	Report      GEO11_Report GEO.A1ModJulian GEO.EarthTOEEq.X GEO.EarthTOEEq.Y GEO.EarthTOEEq.Z GEO.EarthTOEEq.VX GEO.EarthTOEEq.VY GEO.EarthTOEEq.VZ GEO.EarthTOEEq.VMAG GEO.EarthTOEEq.RAV GEO.EarthTOEEq.HX GEO.EarthTOEEq.HY GEO.EarthTOEEq.HZ GEO.EarthTOEEq.AOP GEO.EarthTOEEq.DEC GEO.EarthTOEEq.DECV GEO.EarthTOEEq.INC GEO.EarthTOEEq.RA GEO.EarthTOEEq.RAAN;
EndFor ;