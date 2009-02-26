% $Id: Comparison_RunAll.m,v 1.3 2007/05/11 15:49:22 edove Exp $

clc
clear all
close all

tempDir = mfilename('fullpath');
if ispc
    temp = findstr(tempDir,'\');
    scriptDir = tempDir(1:temp(size(temp,2))-1);
else
    temp = findstr(tempDir,'/');
    scriptDir = tempDir(1:temp(size(temp,2))-1);
end
addpath(scriptDir);
cd(scriptDir);

try
    Comparison_Tool1_Tool2_PV
catch
    disp(' ');
    disp('An error occured while running Comparison_Tool1_Tool2_PV.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
try
    Comparison_Tool1_Tool2_CS
catch
    disp(' ');
    disp('An error occured while running Comparison_Tool1_Tool2_CS.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
try
    Comparison_Tool1_Tool2_Cb
catch
    disp(' ');
    disp('An error occured while running Comparison_Tool1_Tool2_Cb.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
try
    Comparison_Tool1_Tool2_Libr
catch
    disp(' ');
    disp('An error occured while running Comparison_Tool1_Tool2_Libr.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
try
    Comparison_DeltaV
catch
    disp(' ');
    disp('An error occured while running Comparison_DeltaV.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
try
    Comparison_Integ
catch
    disp(' ');
    disp('An error occured while running Comparison_Integ.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
try
    Comparison_StopCond
catch
    disp(' ');
    disp('An error occured while running Comparison_StopCond.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end
% try
%     TimeComparo
% catch
%     disp(' ');
%     disp('An error occured while running TimeComparo.m')
%     disp('Record the error below for your records and fix the bug before re-running the script')
%     lasterror
%     pause
% end
try
    LoopTestSummary
catch
    disp(' ');
    disp('An error occured while running LoopTestSummary.m')
    disp('Record the error below for your records and fix the bug before re-running the script')
    lasterror
    pause
end

clc
disp('All scripts have been run.')
disp('Remember to track down any errors that might have occured in the comparison scripts.')