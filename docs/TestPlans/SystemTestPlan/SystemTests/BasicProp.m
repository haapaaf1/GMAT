%% $Id: BasicProp.m,v 1.5 2006/10/11 16:37:00 dconway Exp $
%% GMAT System Test Script File
%
% This test case is designed to test the following elements:
%
% 1.  Spacecraft state specification in Earth MJ2000 Cartesian, Keplerian, and
%     Modified Keplerian Coordinates.
% 2.  Force models appropriate to LEO, HEO and GEO orbits.
% 3.  Basic orbit Propagation.
%
% The only output file is BasicPropHEOReport.txt, which contains various output 
% parameters for the HEO spacecraft.  The data in this report should be the same
% from run to run.
%
% There are no external dependencies.
%
% This file has been edited to reduce size, so that it can be used as an example
% in the System Test Plan.

Create Spacecraft LEO;
GMAT LEO.DateFormat = TAIModJulian;
GMAT LEO.Epoch = 21545;
GMAT LEO.CoordinateSystem = EarthMJ2000Eq;
GMAT LEO.StateType = Cartesian;
GMAT LEO.X = 7100;
GMAT LEO.Y = 0;
GMAT LEO.Z = 1300;
GMAT LEO.VX = 0;
GMAT LEO.VY = 7.35;
GMAT LEO.VZ = 1;

Create Spacecraft HEO;
GMAT HEO.DateFormat = TAIGregorian;
GMAT HEO.Epoch = 12 Sep 2006 21:28:00.000;
GMAT HEO.CoordinateSystem = EarthMJ2000Eq;
GMAT HEO.StateType = Keplerian;
GMAT HEO.SMA = 43200;
GMAT HEO.ECC = 0.8;
GMAT HEO.INC = 78;
GMAT HEO.RAAN = 15;
GMAT HEO.AOP = 35;
GMAT HEO.TA = 120;

Create Spacecraft GEO;
GMAT GEO.DateFormat = UTCGregorian;
GMAT GEO.Epoch = 25 Dec 2010 00:00:00.000;
GMAT GEO.CoordinateSystem = EarthMJ2000Eq;
GMAT GEO.StateType = ModifiedKeplerian;
GMAT GEO.RadPer = 42164.5;
GMAT GEO.RadApo = 42165.5;
GMAT GEO.INC = 0.5;
GMAT GEO.RAAN = 90;
GMAT GEO.AOP = 90;
GMAT GEO.TA = 90;

Create ForceModel LeoProp_ForceModel;
GMAT LeoProp_ForceModel.CentralBody = Earth;
GMAT LeoProp_ForceModel.PrimaryBodies = {Earth};
GMAT LeoProp_ForceModel.Drag = Exponential;
GMAT LeoProp_ForceModel.Gravity.Earth.Degree = 20;
GMAT LeoProp_ForceModel.Gravity.Earth.Order = 20;
GMAT LeoProp_ForceModel.Gravity.Earth.PotentialFile = c:/GmatDataFiles/gravity/earth/JGM2.grv;
GMAT LeoProp_ForceModel.Drag.AtmosphereBody = Earth;

Create Propagator LeoProp;
GMAT LeoProp.FM = LeoProp_ForceModel;
GMAT LeoProp.Type = RungeKutta89;

Create ForceModel HeoProp_ForceModel;
GMAT HeoProp_ForceModel.CentralBody = Earth;
GMAT HeoProp_ForceModel.PrimaryBodies = {Earth};
GMAT HeoProp_ForceModel.Drag = MSISE90;
GMAT HeoProp_ForceModel.SRP = On;
GMAT HeoProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT HeoProp_ForceModel.Gravity.Earth.Order = 4;
GMAT HeoProp_ForceModel.Gravity.Earth.PotentialFile = c:/GmatDataFiles/gravity/earth/JGM3.grv;
GMAT HeoProp_ForceModel.Drag.InputSource = Constant;

Create Propagator HeoProp;
GMAT HeoProp.FM = HeoProp_ForceModel;
GMAT HeoProp.Type = RungeKutta89;

Create ForceModel GeoProp_ForceModel;
GMAT GeoProp_ForceModel.CentralBody = Earth;
GMAT GeoProp_ForceModel.PrimaryBodies = {Earth};
GMAT GeoProp_ForceModel.PointMasses = {Sun, Luna, Jupiter, Venus};
GMAT GeoProp_ForceModel.SRP = On;
GMAT GeoProp_ForceModel.Gravity.Earth.Degree = 4;
GMAT GeoProp_ForceModel.Gravity.Earth.Order = 4;

Create Propagator GeoProp;
GMAT GeoProp.FM = GeoProp_ForceModel;
GMAT GeoProp.Type = PrinceDormand78;
Create ReportFile HeoReport;
GMAT HeoReport.Filename = BasicPropHEOReport.txt;
GMAT HeoReport.Precision = 16;
GMAT HeoReport.Add = {LEO.A1Gregorian, LEO.A1ModJulian, LEO.ElapsedSecs, ...
   LEO.ElapsedDays, LEO.Earth.SMA, LEO.Earth.ECC, LEO.EarthMJ2000Eq.INC, ...
   LEO.EarthMJ2000Eq.RAAN, LEO.EarthMJ2000Eq.AOP, LEO.Earth.TA};

%%--------------------------------------------------------------
%%----------  Mission Sequence
%%--------------------------------------------------------------
Propagate LeoProp(LEO, {LEO.ElapsedSecs = 8640.0});
Propagate HeoProp(HEO, {HEO.ElapsedSecs = 432000.0});
Propagate GeoProp(GEO, {GEO.ElapsedDays = 30.0});
