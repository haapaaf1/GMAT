function varargout = GMATTestScript(Sc,Thrst,CSnum,Tnk);
inputfolder = 'D:\GMAT Repository\RegSetupSVN\input\AcceptTest\';
outputfolder = '.\output\AcceptTest\';
fname = ['FBurn_GMAT_','Earth_',Sc,'_',Thrst,'_CS',num2str(CSnum),'_',Tnk];


%% Variable Scripts that Change
Sc_String = GMATSpacecraftString(Sc);
Thruster_String = GMATThrusterString(Thrst,CSnum);
Tank_String = GMATTankString(Tnk);


%%Constant script for producing results
Script_Cell = {' ';
'% -------------------------------------------------------------------------';
'% --------------------------- Create Finite Burn  -------------------------';
'% -------------------------------------------------------------------------';
'Create FiniteBurn fb;';
'GMAT fb.Thrusters = {engine1};';
' ';
'%  Create a force model';
'Create ForceModel fm;';
'GMAT fm.CentralBody = Earth;';
'GMAT fm.PointMasses = {Earth};';
'GMAT fm.Drag = None;';
'GMAT fm.SRP = Off;';
'GMAT fm.ErrorControl = RSSStep;';
' ';
'%  Create a propagator';
'Create Propagator prop;';
'GMAT prop.FM = fm;';
'GMAT prop.Type = RungeKutta89;';
'GMAT prop.InitialStepSize = 60;';
'GMAT prop.Accuracy = 1e-9;';
'GMAT prop.MinStep = 60;';
'GMAT prop.MaxStep = 60;';
'GMAT prop.MaxStepAttempts = 50;';
' ';
'%  Create a report file';
'Create ReportFile rf;';
['GMAT rf.Filename = ','''',[outputfolder,fname,'_output.report'],''';'];
'GMAT rf.Precision = 16;';
'GMAT rf.Add = {Sc.TAIModJulian, Sc.X, Sc.Y, Sc.Z, Sc.VX, Sc.VY, Sc.VZ, Sc.TotalMass};';
'GMAT rf.WriteHeaders = Off;';
'GMAT rf.ZeroFill = On;';
'GMAT rf.ColumnWidth = 27;';
'GMAT rf.LeftJustify = On;';
' ';
'% -------------------------------------------------------------------------';
'% --------------------------- Mission Sequence ----------------------------';
'% -------------------------------------------------------------------------';
' ';
'%  Turn on thrusters....they will remain on through all events until the';
'%  "EndFiniteBurn fb(Sc)" command is executed.';
'BeginFiniteBurn fb(Sc);';
' ';
'% Propagate for 2 hours, while the thrusters are turned on.';
'Propagate prop(Sc,{Sc.ElapsedSecs = 7200});';
' ';
'%  Turn off thrusters   ';
'EndFiniteBurn fb(Sc);';
' ';
' '};
GMATFile([inputfolder,fname,'_testscript.m'],Sc_String,Thruster_String,Tank_String, Script_Cell);
end