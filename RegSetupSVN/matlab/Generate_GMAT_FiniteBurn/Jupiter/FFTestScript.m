function varargout = FFTestScript(Sc,Thrst,CSnum,Tnk);
inputfolder = 'D:\GMAT Repository\RefSoftFiles\FreeFlyerData\FiniteBurn\AcceptTest\';
outputfolder = 'D:\GMAT Repository\RegSetupSVN\output\AcceptTest\Good_reports\FF\';
fname = ['FBurn_FF_','Jupiter_',Sc,'_',Thrst,'_CS',num2str(CSnum),'_',Tnk];


%% Variable Scripts that Change
Sc_String = FFSpacecraftString(Sc);
Thruster_String = FFThrusterString(Thrst,CSnum);
Tank_String = FFTankString(Tnk);


%%Constant script for producing results
Script_Cell = {' ';
' ';
'fb.BurnDuration = prop.StepSize;  //Matches the fact that GMAT uses its propagator for maneuvering';
' ';
' ';
'Variable i;';
'For i = 0 to 7200 step fb.BurnDuration;';
'Call FF_Match_GMAT_Report(Sc,FF_GMAT_Report);';
'Maneuver Sc using fb;';
'End;';
' '};

FFFile([inputfolder,fname,'_testscript.script'],FFPreamble([outputfolder,fname,'_output.report']),Sc_String,Tank_String,Thruster_String, Script_Cell);
FFxmlWrapper([inputfolder,fname,'_testscript.script'],[inputfolder,fname,'_testscript.MissionPlan']);
end