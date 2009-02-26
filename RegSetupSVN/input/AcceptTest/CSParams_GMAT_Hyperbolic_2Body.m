%% $Id: CSParams_GMAT_Hyperbolic_2Body.m,v 1.4 2007/07/26 19:12:26 edove Exp $

Create Spacecraft Hyper;
GMAT Hyper.J2000BodyName = Earth;
GMAT Hyper.Epoch.UTCGregorian = 01 Jun 2004 12:00:00.000;
GMAT Hyper.DisplayStateType = Cartesian;
GMAT Hyper.CoordinateSystem = EarthMJ2000Eq;
GMAT Hyper.X = 12371.791482634855;
GMAT Hyper.Y = 5050.7627227610719;
GMAT Hyper.Z = 5050.762722761071;
GMAT Hyper.VX = -7.9859921512608487;
GMAT Hyper.VY = 2.44520073255755;
GMAT Hyper.VZ = 2.4452007325575495;
GMAT Hyper.Cd = 2.2;
GMAT Hyper.Cr = 1.2;
GMAT Hyper.DragArea = 20;
GMAT Hyper.SRPArea = 20;
GMAT Hyper.DryMass = 1000;
GMAT Hyper.TotalMass = 1000;

Create ForceModel Earth2Body;
GMAT Earth2Body.PointMasses   = {Earth};
GMAT Earth2Body.Drag = None;
GMAT Earth2Body.SRP = Off;

Create Propagator RKV89;
GMAT RKV89.FM = Earth2Body;
GMAT RKV89.Type = RungeKutta89;
GMAT RKV89.InitialStepSize = 5;
GMAT RKV89.Accuracy = 1e-013;
GMAT RKV89.MinStep = 1e-7;
GMAT RKV89.MaxStep = 5;
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

Create ReportFile Hyper_Report
GMAT Hyper_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthFixed.report;
GMAT Hyper_Report.Precision = 16;
GMAT Hyper_Report.WriteHeaders = On;
GMAT Hyper_Report.ColumnWidth = 20;

Create ReportFile Hyper2_Report
GMAT Hyper2_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthMJ2000Ec.report;
GMAT Hyper2_Report.Precision = 16;
GMAT Hyper2_Report.WriteHeaders = On;
GMAT Hyper2_Report.ColumnWidth = 20;

Create ReportFile Hyper3_Report
GMAT Hyper3_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthMJ2000Eq.report;
GMAT Hyper3_Report.Precision = 16;
GMAT Hyper3_Report.WriteHeaders = On;
GMAT Hyper3_Report.ColumnWidth = 20;

Create ReportFile Hyper4_Report
GMAT Hyper4_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthMODEc.report;
GMAT Hyper4_Report.Precision = 16;
GMAT Hyper4_Report.WriteHeaders = On;
GMAT Hyper4_Report.ColumnWidth = 20;

Create ReportFile Hyper5_Report
GMAT Hyper5_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthMODEq.report;
GMAT Hyper5_Report.Precision = 16;
GMAT Hyper5_Report.WriteHeaders = On;
GMAT Hyper5_Report.ColumnWidth = 20;

Create ReportFile Hyper6_Report
GMAT Hyper6_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthMOEEc.report;
GMAT Hyper6_Report.Precision = 16;
GMAT Hyper6_Report.WriteHeaders = On;
GMAT Hyper6_Report.ColumnWidth = 20;

Create ReportFile Hyper7_Report
GMAT Hyper7_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthMOEEq.report;
GMAT Hyper7_Report.Precision = 16;
GMAT Hyper7_Report.WriteHeaders = On;
GMAT Hyper7_Report.ColumnWidth = 20;

Create ReportFile Hyper8_Report
GMAT Hyper8_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthTODEc.report;
GMAT Hyper8_Report.Precision = 16;
GMAT Hyper8_Report.WriteHeaders = On;
GMAT Hyper8_Report.ColumnWidth = 20;

Create ReportFile Hyper9_Report
GMAT Hyper9_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthTODEq.report;
GMAT Hyper9_Report.Precision = 16;
GMAT Hyper9_Report.WriteHeaders = On;
GMAT Hyper9_Report.ColumnWidth = 20;

Create ReportFile Hyper10_Report
GMAT Hyper10_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthTOEEc.report;
GMAT Hyper10_Report.Precision = 16;
GMAT Hyper10_Report.WriteHeaders = On;
GMAT Hyper10_Report.ColumnWidth = 20;

Create ReportFile Hyper11_Report
GMAT Hyper11_Report.Filename = ./output/AcceptTest/CSParams_GMAT_Hyperbolic_2Body_EarthTOEEq.report;
GMAT Hyper11_Report.Precision = 16;
GMAT Hyper11_Report.WriteHeaders = On;
GMAT Hyper11_Report.ColumnWidth = 20;

Create Variable OutputStepSize;

%-----------Begin Propagation and Report Generation--------
% Propagate based on preset propagation parameters
% and current stop conditions.

% Output Report file data for each propagation set in the FOR loop
Report      Hyper_Report Hyper.A1ModJulian Hyper.EarthFixed.X Hyper.EarthFixed.Y Hyper.EarthFixed.Z Hyper.EarthFixed.VX Hyper.EarthFixed.VY Hyper.EarthFixed.VZ Hyper.EarthFixed.VMAG Hyper.EarthFixed.RAV Hyper.EarthFixed.HX Hyper.EarthFixed.HY Hyper.EarthFixed.HZ Hyper.EarthFixed.AOP Hyper.EarthFixed.DEC Hyper.EarthFixed.DECV Hyper.EarthFixed.INC Hyper.EarthFixed.RA Hyper.EarthFixed.RAAN;
Report      Hyper2_Report Hyper.A1ModJulian Hyper.EarthMJ2000Ec.X Hyper.EarthMJ2000Ec.Y Hyper.EarthMJ2000Ec.Z Hyper.EarthMJ2000Ec.VX Hyper.EarthMJ2000Ec.VY Hyper.EarthMJ2000Ec.VZ Hyper.EarthMJ2000Ec.VMAG Hyper.EarthMJ2000Ec.RAV Hyper.EarthMJ2000Ec.HX Hyper.EarthMJ2000Ec.HY Hyper.EarthMJ2000Ec.HZ Hyper.EarthMJ2000Ec.AOP Hyper.EarthMJ2000Ec.DEC Hyper.EarthMJ2000Ec.DECV Hyper.EarthMJ2000Ec.INC Hyper.EarthMJ2000Ec.RA Hyper.EarthMJ2000Ec.RAAN;
Report      Hyper3_Report Hyper.A1ModJulian Hyper.EarthMJ2000Eq.X Hyper.EarthMJ2000Eq.Y Hyper.EarthMJ2000Eq.Z Hyper.EarthMJ2000Eq.VX Hyper.EarthMJ2000Eq.VY Hyper.EarthMJ2000Eq.VZ Hyper.EarthMJ2000Eq.VMAG Hyper.EarthMJ2000Eq.RAV Hyper.EarthMJ2000Eq.HX Hyper.EarthMJ2000Eq.HY Hyper.EarthMJ2000Eq.HZ Hyper.EarthMJ2000Eq.AOP Hyper.EarthMJ2000Eq.DEC Hyper.EarthMJ2000Eq.DECV Hyper.EarthMJ2000Eq.INC Hyper.EarthMJ2000Eq.RA Hyper.EarthMJ2000Eq.RAAN;
Report      Hyper4_Report Hyper.A1ModJulian Hyper.EarthMODEc.X Hyper.EarthMODEc.Y Hyper.EarthMODEc.Z Hyper.EarthMODEc.VX Hyper.EarthMODEc.VY Hyper.EarthMODEc.VZ Hyper.EarthMODEc.VMAG Hyper.EarthMODEc.RAV Hyper.EarthMODEc.HX Hyper.EarthMODEc.HY Hyper.EarthMODEc.HZ Hyper.EarthMODEc.AOP Hyper.EarthMODEc.DEC Hyper.EarthMODEc.DECV Hyper.EarthMODEc.INC Hyper.EarthMODEc.RA Hyper.EarthMODEc.RAAN;
Report      Hyper5_Report Hyper.A1ModJulian Hyper.EarthMODEq.X Hyper.EarthMODEq.Y Hyper.EarthMODEq.Z Hyper.EarthMODEq.VX Hyper.EarthMODEq.VY Hyper.EarthMODEq.VZ Hyper.EarthMODEq.VMAG Hyper.EarthMODEq.RAV Hyper.EarthMODEq.HX Hyper.EarthMODEq.HY Hyper.EarthMODEq.HZ Hyper.EarthMODEq.AOP Hyper.EarthMODEq.DEC Hyper.EarthMODEq.DECV Hyper.EarthMODEq.INC Hyper.EarthMODEq.RA Hyper.EarthMODEq.RAAN;
Report      Hyper6_Report Hyper.A1ModJulian Hyper.EarthMOEEc.X Hyper.EarthMOEEc.Y Hyper.EarthMOEEc.Z Hyper.EarthMOEEc.VX Hyper.EarthMOEEc.VY Hyper.EarthMOEEc.VZ Hyper.EarthMOEEc.VMAG Hyper.EarthMOEEc.RAV Hyper.EarthMOEEc.HX Hyper.EarthMOEEc.HY Hyper.EarthMOEEc.HZ Hyper.EarthMOEEc.AOP Hyper.EarthMOEEc.DEC Hyper.EarthMOEEc.DECV Hyper.EarthMOEEc.INC Hyper.EarthMOEEc.RA Hyper.EarthMOEEc.RAAN;
Report      Hyper7_Report Hyper.A1ModJulian Hyper.EarthMOEEq.X Hyper.EarthMOEEq.Y Hyper.EarthMOEEq.Z Hyper.EarthMOEEq.VX Hyper.EarthMOEEq.VY Hyper.EarthMOEEq.VZ Hyper.EarthMOEEq.VMAG Hyper.EarthMOEEq.RAV Hyper.EarthMOEEq.HX Hyper.EarthMOEEq.HY Hyper.EarthMOEEq.HZ Hyper.EarthMOEEq.AOP Hyper.EarthMOEEq.DEC Hyper.EarthMOEEq.DECV Hyper.EarthMOEEq.INC Hyper.EarthMOEEq.RA Hyper.EarthMOEEq.RAAN;
Report      Hyper8_Report Hyper.A1ModJulian Hyper.EarthTODEc.X Hyper.EarthTODEc.Y Hyper.EarthTODEc.Z Hyper.EarthTODEc.VX Hyper.EarthTODEc.VY Hyper.EarthTODEc.VZ Hyper.EarthTODEc.VMAG Hyper.EarthTODEc.RAV Hyper.EarthTODEc.HX Hyper.EarthTODEc.HY Hyper.EarthTODEc.HZ Hyper.EarthTODEc.AOP Hyper.EarthTODEc.DEC Hyper.EarthTODEc.DECV Hyper.EarthTODEc.INC Hyper.EarthTODEc.RA Hyper.EarthTODEc.RAAN;
Report      Hyper9_Report Hyper.A1ModJulian Hyper.EarthTODEq.X Hyper.EarthTODEq.Y Hyper.EarthTODEq.Z Hyper.EarthTODEq.VX Hyper.EarthTODEq.VY Hyper.EarthTODEq.VZ Hyper.EarthTODEq.VMAG Hyper.EarthTODEq.RAV Hyper.EarthTODEq.HX Hyper.EarthTODEq.HY Hyper.EarthTODEq.HZ Hyper.EarthTODEq.AOP Hyper.EarthTODEq.DEC Hyper.EarthTODEq.DECV Hyper.EarthTODEq.INC Hyper.EarthTODEq.RA Hyper.EarthTODEq.RAAN;
Report      Hyper10_Report Hyper.A1ModJulian Hyper.EarthTOEEc.X Hyper.EarthTOEEc.Y Hyper.EarthTOEEc.Z Hyper.EarthTOEEc.VX Hyper.EarthTOEEc.VY Hyper.EarthTOEEc.VZ Hyper.EarthTOEEc.VMAG Hyper.EarthTOEEc.RAV Hyper.EarthTOEEc.HX Hyper.EarthTOEEc.HY Hyper.EarthTOEEc.HZ Hyper.EarthTOEEc.AOP Hyper.EarthTOEEc.DEC Hyper.EarthTOEEc.DECV Hyper.EarthTOEEc.INC Hyper.EarthTOEEc.RA Hyper.EarthTOEEc.RAAN;
Report      Hyper11_Report Hyper.A1ModJulian Hyper.EarthTOEEq.X Hyper.EarthTOEEq.Y Hyper.EarthTOEEq.Z Hyper.EarthTOEEq.VX Hyper.EarthTOEEq.VY Hyper.EarthTOEEq.VZ Hyper.EarthTOEEq.VMAG Hyper.EarthTOEEq.RAV Hyper.EarthTOEEq.HX Hyper.EarthTOEEq.HY Hyper.EarthTOEEq.HZ Hyper.EarthTOEEq.AOP Hyper.EarthTOEEq.DEC Hyper.EarthTOEEq.DECV Hyper.EarthTOEEq.INC Hyper.EarthTOEEq.RA Hyper.EarthTOEEq.RAAN;
GMAT Hyper_Report.WriteHeaders = Off;
GMAT Hyper2_Report.WriteHeaders = Off;
GMAT Hyper3_Report.WriteHeaders = Off;
GMAT Hyper4_Report.WriteHeaders = Off;
GMAT Hyper5_Report.WriteHeaders = Off;
GMAT Hyper6_Report.WriteHeaders = Off;
GMAT Hyper7_Report.WriteHeaders = Off;
GMAT Hyper8_Report.WriteHeaders = Off;
GMAT Hyper9_Report.WriteHeaders = Off;
GMAT Hyper10_Report.WriteHeaders = Off;
GMAT Hyper11_Report.WriteHeaders = Off;
For OutputStepSize = 1:144;
	Propagate   RKV89(Hyper, {Hyper.ElapsedSecs = 600});
	Report      Hyper_Report Hyper.A1ModJulian Hyper.EarthFixed.X Hyper.EarthFixed.Y Hyper.EarthFixed.Z Hyper.EarthFixed.VX Hyper.EarthFixed.VY Hyper.EarthFixed.VZ Hyper.EarthFixed.VMAG Hyper.EarthFixed.RAV Hyper.EarthFixed.HX Hyper.EarthFixed.HY Hyper.EarthFixed.HZ Hyper.EarthFixed.AOP Hyper.EarthFixed.DEC Hyper.EarthFixed.DECV Hyper.EarthFixed.INC Hyper.EarthFixed.RA Hyper.EarthFixed.RAAN;
	Report      Hyper2_Report Hyper.A1ModJulian Hyper.EarthMJ2000Ec.X Hyper.EarthMJ2000Ec.Y Hyper.EarthMJ2000Ec.Z Hyper.EarthMJ2000Ec.VX Hyper.EarthMJ2000Ec.VY Hyper.EarthMJ2000Ec.VZ Hyper.EarthMJ2000Ec.VMAG Hyper.EarthMJ2000Ec.RAV Hyper.EarthMJ2000Ec.HX Hyper.EarthMJ2000Ec.HY Hyper.EarthMJ2000Ec.HZ Hyper.EarthMJ2000Ec.AOP Hyper.EarthMJ2000Ec.DEC Hyper.EarthMJ2000Ec.DECV Hyper.EarthMJ2000Ec.INC Hyper.EarthMJ2000Ec.RA Hyper.EarthMJ2000Ec.RAAN;
	Report      Hyper3_Report Hyper.A1ModJulian Hyper.EarthMJ2000Eq.X Hyper.EarthMJ2000Eq.Y Hyper.EarthMJ2000Eq.Z Hyper.EarthMJ2000Eq.VX Hyper.EarthMJ2000Eq.VY Hyper.EarthMJ2000Eq.VZ Hyper.EarthMJ2000Eq.VMAG Hyper.EarthMJ2000Eq.RAV Hyper.EarthMJ2000Eq.HX Hyper.EarthMJ2000Eq.HY Hyper.EarthMJ2000Eq.HZ Hyper.EarthMJ2000Eq.AOP Hyper.EarthMJ2000Eq.DEC Hyper.EarthMJ2000Eq.DECV Hyper.EarthMJ2000Eq.INC Hyper.EarthMJ2000Eq.RA Hyper.EarthMJ2000Eq.RAAN;
	Report      Hyper4_Report Hyper.A1ModJulian Hyper.EarthMODEc.X Hyper.EarthMODEc.Y Hyper.EarthMODEc.Z Hyper.EarthMODEc.VX Hyper.EarthMODEc.VY Hyper.EarthMODEc.VZ Hyper.EarthMODEc.VMAG Hyper.EarthMODEc.RAV Hyper.EarthMODEc.HX Hyper.EarthMODEc.HY Hyper.EarthMODEc.HZ Hyper.EarthMODEc.AOP Hyper.EarthMODEc.DEC Hyper.EarthMODEc.DECV Hyper.EarthMODEc.INC Hyper.EarthMODEc.RA Hyper.EarthMODEc.RAAN;
	Report      Hyper5_Report Hyper.A1ModJulian Hyper.EarthMODEq.X Hyper.EarthMODEq.Y Hyper.EarthMODEq.Z Hyper.EarthMODEq.VX Hyper.EarthMODEq.VY Hyper.EarthMODEq.VZ Hyper.EarthMODEq.VMAG Hyper.EarthMODEq.RAV Hyper.EarthMODEq.HX Hyper.EarthMODEq.HY Hyper.EarthMODEq.HZ Hyper.EarthMODEq.AOP Hyper.EarthMODEq.DEC Hyper.EarthMODEq.DECV Hyper.EarthMODEq.INC Hyper.EarthMODEq.RA Hyper.EarthMODEq.RAAN;
	Report      Hyper6_Report Hyper.A1ModJulian Hyper.EarthMOEEc.X Hyper.EarthMOEEc.Y Hyper.EarthMOEEc.Z Hyper.EarthMOEEc.VX Hyper.EarthMOEEc.VY Hyper.EarthMOEEc.VZ Hyper.EarthMOEEc.VMAG Hyper.EarthMOEEc.RAV Hyper.EarthMOEEc.HX Hyper.EarthMOEEc.HY Hyper.EarthMOEEc.HZ Hyper.EarthMOEEc.AOP Hyper.EarthMOEEc.DEC Hyper.EarthMOEEc.DECV Hyper.EarthMOEEc.INC Hyper.EarthMOEEc.RA Hyper.EarthMOEEc.RAAN;
	Report      Hyper7_Report Hyper.A1ModJulian Hyper.EarthMOEEq.X Hyper.EarthMOEEq.Y Hyper.EarthMOEEq.Z Hyper.EarthMOEEq.VX Hyper.EarthMOEEq.VY Hyper.EarthMOEEq.VZ Hyper.EarthMOEEq.VMAG Hyper.EarthMOEEq.RAV Hyper.EarthMOEEq.HX Hyper.EarthMOEEq.HY Hyper.EarthMOEEq.HZ Hyper.EarthMOEEq.AOP Hyper.EarthMOEEq.DEC Hyper.EarthMOEEq.DECV Hyper.EarthMOEEq.INC Hyper.EarthMOEEq.RA Hyper.EarthMOEEq.RAAN;
	Report      Hyper8_Report Hyper.A1ModJulian Hyper.EarthTODEc.X Hyper.EarthTODEc.Y Hyper.EarthTODEc.Z Hyper.EarthTODEc.VX Hyper.EarthTODEc.VY Hyper.EarthTODEc.VZ Hyper.EarthTODEc.VMAG Hyper.EarthTODEc.RAV Hyper.EarthTODEc.HX Hyper.EarthTODEc.HY Hyper.EarthTODEc.HZ Hyper.EarthTODEc.AOP Hyper.EarthTODEc.DEC Hyper.EarthTODEc.DECV Hyper.EarthTODEc.INC Hyper.EarthTODEc.RA Hyper.EarthTODEc.RAAN;
	Report      Hyper9_Report Hyper.A1ModJulian Hyper.EarthTODEq.X Hyper.EarthTODEq.Y Hyper.EarthTODEq.Z Hyper.EarthTODEq.VX Hyper.EarthTODEq.VY Hyper.EarthTODEq.VZ Hyper.EarthTODEq.VMAG Hyper.EarthTODEq.RAV Hyper.EarthTODEq.HX Hyper.EarthTODEq.HY Hyper.EarthTODEq.HZ Hyper.EarthTODEq.AOP Hyper.EarthTODEq.DEC Hyper.EarthTODEq.DECV Hyper.EarthTODEq.INC Hyper.EarthTODEq.RA Hyper.EarthTODEq.RAAN;
	Report      Hyper10_Report Hyper.A1ModJulian Hyper.EarthTOEEc.X Hyper.EarthTOEEc.Y Hyper.EarthTOEEc.Z Hyper.EarthTOEEc.VX Hyper.EarthTOEEc.VY Hyper.EarthTOEEc.VZ Hyper.EarthTOEEc.VMAG Hyper.EarthTOEEc.RAV Hyper.EarthTOEEc.HX Hyper.EarthTOEEc.HY Hyper.EarthTOEEc.HZ Hyper.EarthTOEEc.AOP Hyper.EarthTOEEc.DEC Hyper.EarthTOEEc.DECV Hyper.EarthTOEEc.INC Hyper.EarthTOEEc.RA Hyper.EarthTOEEc.RAAN;
	Report      Hyper11_Report Hyper.A1ModJulian Hyper.EarthTOEEq.X Hyper.EarthTOEEq.Y Hyper.EarthTOEEq.Z Hyper.EarthTOEEq.VX Hyper.EarthTOEEq.VY Hyper.EarthTOEEq.VZ Hyper.EarthTOEEq.VMAG Hyper.EarthTOEEq.RAV Hyper.EarthTOEEq.HX Hyper.EarthTOEEq.HY Hyper.EarthTOEEq.HZ Hyper.EarthTOEEq.AOP Hyper.EarthTOEEq.DEC Hyper.EarthTOEEq.DECV Hyper.EarthTOEEq.INC Hyper.EarthTOEEq.RA Hyper.EarthTOEEq.RAAN;
EndFor ;