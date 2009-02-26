% Comparison_Tool1_Tool2_Cb Compares GMATs Central Body (Cb) parameters between two tools.
%   Purpose: This program was designed to allow the user to select two programs 
%   to compare to one another. The comparison involves taking the difference 
%    of the variables listed in the INPUT section of this help text. 
%     For each case that is compared the maximum absolute value of the 
%   diferences are computed and sent to a mat file. This script should be run from 
%   the GMAT_scripts folder.
%
%      File folders used
%   [Main GMAT_results Directory]/GMAT_scripts 
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports/[Tool1]_[Tool2]
%
%   INPUTS 
%   The report files/input must be formatted the same way. 
%   The columns are ordered as followed:
%   Time, Altitude (km), Beta Angle (deg), C3_Energy (km^2/sec^2), Eccentricity, 
%   Latitude (deg), Longitude (deg), (RxV)_Mag (km^2/sec), Mean Anomaly (deg), 
%   Mean Motion (rad/sec), Period (sec), Apoapsis Radius (km), Periapsis Radius (km),
%   R_Mag (km), Semi-major Axis (km), True Anomaly (deg), Semilatus Rectum(km), 
%   Apoapsis Velocity (km/sec), Periapsis Velocity (km/sec), Greenwich Hour Angle(deg)
%   Local Sidereal Time
% 
%      The data must be seperated by spaces. The report files must also follow 
%   the following naming convention: 
%   CbParams_[Tool]_[Trajectory]_2Body.report
%
%   OUTPUTS
%   Excel document w/ the current date in the filename and comparison results.
%   Matlab .mat file with the data for both tools, the comparison results, and 
%       a header variable to know what each column is.
%
%   REFERENCES
%   Steve Hughes' Latex table generation script [called to from script]
%   Kevin Berry's Excel data exportation script [embedded into script].
% 
%   REVISION HISTORY
%   $Id: Comparison_Tool1_Tool2_Cb.m,v 1.5 2007/09/14 15:52:12 edove Exp $
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      09/07/2005      Original
%   E.Dove      09/14/2007      Last Modified
%
% See also BUILDRUN_SCRIPT_GMAT, AUTOMATION_GMAT, COMPARISON_TOOL1_TOOL2_PV, COMPARISON_TOOL1_TOOL2_CS

clc
error = 1;

rerunScript = 1;
while size(rerunScript,1) ~= 0
    clc
    clear all
    close all
    rerunScript = 1;
    format long
    tempDir = mfilename('fullpath');
    if ispc
        temp = findstr(tempDir,'\');
        compareDir = tempDir(1:temp(size(temp,2))-1);
    else
        temp = findstr(tempDir,'/');
        compareDir = tempDir(1:temp(size(temp,2))-1);
    end
    cd(compareDir);
    cd('..');
    cd('..');
    mainDir = cd;
    cd('..');
    cvsRootDir = cd;
    AcceptTestDir = [cvsRootDir,'/GMATDocuments/AcceptTest'];
    
    addpath(genpath([mainDir,'/matlab']));
    inputDir = [mainDir,'/input'];
    outputTestDir = [mainDir,'/output/AcceptTest'];
    cd(compareDir);
        
    disp('Welcome to the Tool1-Tool2 Centralbody (Cb) Report Comparison Program.');
    disp('Press ENTER to continue.');
    pause
    
    [runExcelFlag] = askExcel(); % *** Function Call ***
    cd(outputTestDir);    
    [Tool1Folder,Tool2Folder] = dispToolMenu_1(); % *** Function Call ***
    [Tool1Folder,Tool2Folder] = orderToolFolder(Tool1Folder,Tool2Folder); % *** Function Call ***    
    [rowStartTool1,rowStartTool2,Tool1,Tool2] = dataStart_2(Tool1Folder,Tool2Folder); % *** Function Call ***
    
    % ===================  Initialize variables ========================
    if strcmp(Tool1Folder,'STK') | strcmp(Tool1Folder,'FF') | strcmp(Tool1Folder,'Exact');
        Tool1Dir    = [outputTestDir,'/Good_reports/',Tool1Folder];
    else;
        Tool1Dir    = [outputTestDir,'/',Tool1Folder,'_reports'];
    end;
    if strcmp(Tool2Folder,'STK') | strcmp(Tool2Folder,'FF') | strcmp(Tool2Folder,'Exact');
        Tool2Dir    = [outputTestDir,'/Good_reports/',Tool2Folder];
    else;
        Tool2Dir    = [outputTestDir,'/',Tool2Folder,'_reports'];
    end;
    nameFile        = 'CbParams';
    countReport     = 0;
    matchFound      = 0;
    mat_header      = {'Time (ModJDate)', 'Altitude (km)', 'Beta Angle (deg)', 'C3_Energy (km^2/sec^2)', 'Eccentricity', 'Latitude (deg)', 'Longitude (deg)', '(RxV)_Mag (km^2/sec)', 'Mean Anomaly (deg)', 'Mean Motion (rad/sec)', 'Period (sec)', 'Apoapsis Radius (km)', 'Periapsis Radius (km)', ' R_Mag (km)', 'Semi-major Axis (km)', 'True Anomaly (deg)', 'Semilatus Rectum(km)', 'Apoapsis Velocity (km/sec)', 'Periapsis Velocity (km/sec)', 'Mean Hour Angle (deg)','Local Sidereal Time (deg)'};
    columnSize      = size(mat_header,2);
    matchChoicesT1  = [];
    matchChoicesT2  = [];
    maxDiffsAll     = zeros(1,columnSize);
    maxDiffs2XL     = {'Test Case', 'Altitude (m)', 'Beta Angle (deg)', 'C3_Energy (m^2/sec^2)', 'Eccentricity', 'Latitude (deg)', 'Longitude (deg)', '(RxV)_Mag (m^2/sec)', 'Mean Anomaly (deg)', 'Mean Motion (rad/sec)', 'Period (sec)', 'Apoapsis Radius (m)', 'Periapsis Radius (m)', ' R_Mag (m)', 'Semi-major Axis (m)', 'True Anomaly (deg)', 'Semilatus Rectum(m)', 'Apoapsis Velocity (m/sec)', 'Periapsis Velocity (m/sec)', 'Mean Hour Angle (deg)','Local Sidereal Time (deg)'};
    DataDir = [outputTestDir,'/CompareResults/',Tool1Folder,'_',Tool2Folder];
    warning off
    mkdir(DataDir); % Creates a directory to save excel and matlab comparison data
    warning on
    disp(' ');
    disp('Select a directory to save the Acceptance Test Latex files to.');
    disp('(If you have access to the CVS controlled GMATDocuments repository,');
    disp('select the AcceptTest folder inside of that repository.)');

    if exist(AcceptTestDir,'dir')==7;
        saveDir = uigetdir(AcceptTestDir,'Select Directory to save Latex files to');
    else;
        saveDir = uigetdir(cd,'Select Directory to save Latex files to');
    end;
    
    while saveDir == 0; % Makes sure the user selects a directory
        if exist(AcceptTestDir,'dir')==7;
            saveDir = uigetdir(AcceptTestDir,'Select Directory to save Latex files to');
        else;
            saveDir = uigetdir(cd,'Select Directory to save Latex files to');
        end;
    end;    
    % ==================================================================    
    
    cd(Tool1Dir);
    txtFilesTool1 = dir('*.report');
    numTxtsTool1 = size(txtFilesTool1,1);

    [menuSize,reportFilesTool1] = checkTool1files(Tool1Dir,Tool1,countReport,nameFile); % *** Function Call ***
    if menuSize == 0
        break
    end
    [reportChoiceTool1] = dispReportMenu1(Tool1Folder,Tool2Folder,reportFilesTool1); % *** Function Call ***
    
    if reportChoiceTool1 == menuSize
        allFlag = 1;
        if runExcelFlag == 1
            [e,eSheets,sheetExists,eWorkbooks] = setupExcel(runExcelFlag,DataDir,Tool1Folder,Tool2Folder); % *** Function Call ***
        end
    else
        allFlag = 0;
    end
        
    for allLoopChoice = 1:(menuSize-1)
        matchFound = 0;
        
        if allFlag == 1
            reportChoiceTool1 = allLoopChoice;
        end
    
        reportLengthTool1 = size(reportFilesTool1{reportChoiceTool1},2);
        [reportLengthTool2,currTool1choice,currTool2choice,matchFound,charLoc1Tool2,reportFilesTool2,reportChoiceTool2] = checkTool2files_2(matchFound,Tool2Dir,Tool1,Tool2,Tool2Folder,reportLengthTool1,reportChoiceTool1, reportFilesTool1, nameFile); % *** Function Call ***
        if isempty(reportFilesTool2)
            return
        end
        
            while matchFound == 1
                % ======= Read numerical data from Tool1 and Tool2 ASCII Report Files =======
                % Read Tool1 output file
                file_Tool11      = [Tool1Dir,'/',reportFilesTool1{reportChoiceTool1}];
                mat_Tool11       = zeros(10000,columnSize);
                warning off;
                mat_Tool11       = dlmread(file_Tool11,'\s',rowStartTool1,0);
                warning on;
                Tool11_rows      = size(mat_Tool11,1);
                mat_Tool11(2:Tool11_rows,1) = round((mat_Tool11(2:Tool11_rows,1) - mat_Tool11(1,1))*86400); % Convert first column from Days to Seconds
                mat_Tool11(1,1)  = 0;
                
                % Tool1 exceptions due to the differences in outputing data
                if isempty(findstr(lower(Tool1),'stk')) == 0
                    mu = -mat_Tool11(1,4)*mat_Tool11(1,15);
                    p  = (mat_Tool11(1,8)^2)/mu;
                    Va = sqrt(2*(mat_Tool11(1,4)/2 + mu/mat_Tool11(1,12)));
                    Vp = sqrt(2*(mat_Tool11(1,4)/2 + mu/mat_Tool11(1,13)));
                    
                    warning off;
                    mat_Tool11_MHA = dlmread([Tool1Dir,'/Cb_STK_',currTool1choice(1:(size(currTool1choice,2)-size('.report',2))),'_MHA.report'],'\s',rowStartTool1,0);
                    warning on;
                    
                    mat_Tool11(:,20) = mat_Tool11_MHA(:,2);
                    for loop = 1:Tool11_rows
                        mat_Tool11(loop,17) = p;
                        mat_Tool11(loop,18) = Va;
                        mat_Tool11(loop,19) = Vp;
                        mat_Tool11(loop,21) = mat_Tool11(loop,20) + mat_Tool11(loop,7); % Local Sidereal Time
                        if (mat_Tool11(loop,21) < 0) & (mat_Tool11(loop,21) >= -360)
                            mat_Tool11(loop,21) = mat_Tool11(loop,21) + 360;
                        elseif mat_Tool11(loop,21) < -360
                            mat_Tool11(loop,21) = mat_Tool11(loop,21) + abs(floor(mat_Tool11(loop,21)/360))*360;
                        elseif mat_Tool11(loop,21) > 360
                            mat_Tool11(loop,21) = mat_Tool11(loop,21) - abs(floor(mat_Tool11(loop,21)/360))*360;
                        end
                    end
                elseif isempty(findstr(lower(Tool1),'ff')) == 0
                    mat_Tool11Temp = mat_Tool11;
                                        
                    mu = -mat_Tool11(1,4)*mat_Tool11(1,18);
                    Va = sqrt(2*(mat_Tool11(1,4)/2 + mu/mat_Tool11(1,13)));
                    Vp = sqrt(2*(mat_Tool11(1,4)/2 + mu/mat_Tool11(1,14)));
                    
                    mat_Tool11          = zeros(Tool11_rows,columnSize);
                    mat_Tool11(:,1:7)   = mat_Tool11Temp(:,1:7);
                    mat_Tool11(:,8)     = sqrt(mat_Tool11Temp(:,8).^2 + mat_Tool11Temp(:,9).^2 + mat_Tool11Temp(:,10).^2);
                    mat_Tool11(:,9)     = mat_Tool11Temp(:,11);
                    mat_Tool11(:,10)    = sqrt(mu./mat_Tool11Temp(:,18).^3);
                    mat_Tool11(:,11:13) = mat_Tool11Temp(:,12:14);
                    mat_Tool11(:,14)    = sqrt(mat_Tool11Temp(:,15).^2 + mat_Tool11Temp(:,16).^2 + mat_Tool11Temp(:,17).^2);
                    mat_Tool11(:,15:17) = mat_Tool11Temp(:,18:20);
                    mat_Tool11(:,20) = mat_Tool11Temp(:,21);
                    mat_Tool11(:,21) = mat_Tool11(:,20) + mat_Tool11(:,7);
                    for loop = 1:Tool11_rows
                        mat_Tool11(loop,18) = Va;
                        mat_Tool11(loop,19) = Vp;
                        if (mat_Tool11(loop,21) < 0) & (mat_Tool11(loop,21) >= -360)
                            mat_Tool11(loop,21) = mat_Tool11(loop,21) + 360;
                        elseif mat_Tool11(loop,21) < -360
                            mat_Tool11(loop,21) = mat_Tool11(loop,21) + abs(floor(mat_Tool11(loop,21)/360))*360;
                        elseif mat_Tool11(loop,21) > 360
                            mat_Tool11(loop,21) = mat_Tool11(loop,21) - abs(floor(mat_Tool11(loop,21)/360))*360;
                        end
                    end
                elseif isempty(findstr(lower(Tool1),'gmat')) == 0
                    mat_Tool11(:,3) = 90 - mat_Tool11(:,3);
                end

                % Read Tool2 output file
                file_Tool21     = [Tool2Dir,'/',reportFilesTool2{reportChoiceTool2}];
                mat_Tool21      = zeros(10000,columnSize);
                warning off;
                mat_Tool21      = dlmread(file_Tool21,'\s',rowStartTool2,0);
                warning on;
                Tool21_rows     = size(mat_Tool21,1);
                mat_Tool21(2:Tool21_rows,1) = round((mat_Tool21(2:Tool21_rows,1) - mat_Tool21(1,1))*86400); % Convert first column from Days to Seconds
                mat_Tool21(1,1)  = 0;
    
                % Tool2 exceptions due to the differences in outputing data
                if isempty(findstr(lower(Tool2),'stk')) == 0
                    mu = -mat_Tool21(1,4)*mat_Tool21(1,15);
                    p  = (mat_Tool21(1,8)^2)/mu;
                    Va = sqrt(2*(mat_Tool21(1,4)/2 + mu/mat_Tool21(1,12)));
                    Vp = sqrt(2*(mat_Tool21(1,4)/2 + mu/mat_Tool21(1,13)));
                    
                    warning off;
                    mat_Tool21_MHA = dlmread([Tool2Dir,'/Cb_STK_',currTool1choice(1:(size(currTool1choice,2)-size('.report',2))),'_MHA.report'],'\s',rowStartTool2,0);
                    warning on;
                    
                    mat_Tool21(:,20) = mat_Tool21_MHA(:,2);
                    for loop = 1:Tool21_rows
                        mat_Tool21(loop,17) = p;
                        mat_Tool21(loop,18) = Va;
                        mat_Tool21(loop,19) = Vp;
                        mat_Tool21(loop,21) = mat_Tool21(loop,20) + mat_Tool21(loop,7);
                        if (mat_Tool21(loop,21) < 0) & (mat_Tool21(loop,21) >= -360)
                            mat_Tool21(loop,21) = mat_Tool21(loop,21) + 360;
                        elseif mat_Tool21(loop,21) < -360
                            mat_Tool21(loop,21) = mat_Tool21(loop,21) + abs(floor(mat_Tool21(loop,21)/360))*360;
                        elseif mat_Tool21(loop,21) > 360
                            mat_Tool21(loop,21) = mat_Tool21(loop,21) - abs(floor(mat_Tool21(loop,21)/360))*360;
                        end
                    end
                elseif isempty(findstr(lower(Tool2),'ff')) == 0
                    mat_Tool21Temp = mat_Tool21;
                                        
                    mu = -mat_Tool21(1,4)*mat_Tool21(1,18);
                    Va = sqrt(2*(mat_Tool21(1,4)/2 + mu/mat_Tool21(1,13)));
                    Vp = sqrt(2*(mat_Tool21(1,4)/2 + mu/mat_Tool21(1,14)));
                    
                    mat_Tool21          = zeros(Tool21_rows,columnSize);
                    mat_Tool21(:,1:7)   = mat_Tool21Temp(:,1:7);
                    mat_Tool21(:,8)     = sqrt(mat_Tool21Temp(:,8).^2 + mat_Tool21Temp(:,9).^2 + mat_Tool21Temp(:,10).^2);
                    mat_Tool21(:,9)     = mat_Tool21Temp(:,11);
                    mat_Tool21(:,10)    = sqrt(mu./mat_Tool21Temp(:,18).^3);
                    mat_Tool21(:,11:13) = mat_Tool21Temp(:,12:14);
                    mat_Tool21(:,14)    = sqrt(mat_Tool21Temp(:,15).^2 + mat_Tool21Temp(:,16).^2 + mat_Tool21Temp(:,17).^2);
                    mat_Tool21(:,15:17) = mat_Tool21Temp(:,18:20);
                    mat_Tool21(:,20) = mat_Tool21Temp(:,21);
                    mat_Tool21(:,21) = mat_Tool21(:,20) + mat_Tool21(:,7);
                    for loop = 1:Tool21_rows
                        mat_Tool21(loop,18) = Va;
                        mat_Tool21(loop,19) = Vp;
                        if (mat_Tool21(loop,21) < 0) & (mat_Tool21(loop,21) >= -360)
                            mat_Tool21(loop,21) = mat_Tool21(loop,21) + 360;
                        elseif mat_Tool21(loop,21) < -360
                            mat_Tool21(loop,21) = mat_Tool21(loop,21) + abs(floor(mat_Tool21(loop,21)/360))*360;
                        elseif mat_Tool21(loop,21) > 360
                            mat_Tool21(loop,21) = mat_Tool21(loop,21) - abs(floor(mat_Tool21(loop,21)/360))*360;
                        end
                    end
                elseif isempty(findstr(lower(Tool2),'gmat')) == 0
                    mat_Tool21(:,3) = 90 - mat_Tool21(:,3);   
                end
                
                % ============== Determine Diferences for Tool1/Tool2 Comparison =================
                if Tool21_rows == Tool11_rows
                    diffMat_Tool1_Tool2 = zeros(Tool21_rows,columnSize);
                    diffMat_Tool1_Tool2(:,1) = mat_Tool21(:,1);
                    matchChoicesT1 = [matchChoicesT1, reportChoiceTool1];
                    matchChoicesT2 = [matchChoicesT2, reportChoiceTool2];
                else
                    disp(' ');
                    disp(['Report data between a ',Tool2Folder,' and ',Tool1Folder,' file do not match.']);
                    disp('Solve this problem and then rerun this script');
                    disp('Check the following file: ');
                    disp(file_Tool21);
                    return
                end
                diffMat_Tool1_Tool2(:,2:columnSize) = mat_Tool21(:,2:columnSize) - mat_Tool11(:,2:columnSize);
                
                [maxDiffs,diffMat_Tool1_Tool2] = find360nAbs(diffMat_Tool1_Tool2, Tool21_rows); % *** Function Call ***
                
                % ***Remove incorrect data and replace with zero ***
                if (mat_Tool11(1,5) > 1) | (mat_Tool21(1,5) > 1) % If orbit is hyperbolic
                    maxDiffs(11) = 0; % Period is undefined for Hyperbolic orbits
                    maxDiffs(12) = 0; % Apopapsis is undefined for Hyperbolic orbits
                    maxDiffs(18) = 0; % Apoapsis Velocity is undefined for Hyperbolic orbits
                end
                    
                reportLengthTool2 = size(reportFilesTool2{reportChoiceTool2}) - charLoc1Tool2;
                currCase = currTool1choice(1:(reportLengthTool2(2)-size('report',2)));
                unitConvert = ones(1,columnSize-1); % Variable to convert all meter units to kilometers
                unitConvert([1 11:14 16:18]) = 1000;
                unitConvert([3 7]) = 1000000;
                maxDiffs2XL = [maxDiffs2XL; [{currCase}, num2cell(maxDiffs(2:columnSize)*diag(unitConvert))]];
               
                maxDiffsAll(allLoopChoice,:) = maxDiffs;
                % ***Remove incorrect data and replace with string "N/A"***
                % (Only for LaTex and Excel output)
                if (mat_Tool11(1,5) > 1) | (mat_Tool21(1,5) > 1) % If orbit is hyperbolic
                    maxDiffs2XL{size(maxDiffs2XL,1),11} = 'N/A'; % Period is undefined for Hyperbolic orbits
                    maxDiffsAll(size(maxDiffsAll,1),11) = 0;
                    maxDiffs2XL{size(maxDiffs2XL,1),12} = 'N/A'; % Apopapsis is undefined for Hyperbolic orbits
                    maxDiffsAll(size(maxDiffsAll,1),12) = 0;
                    maxDiffs2XL{size(maxDiffs2XL,1),18} = 'N/A'; % Apoapsis Velocity is undefined for Hyperbolic orbits
                    maxDiffsAll(size(maxDiffsAll,1),18) = 0;
                end
                nonEarthFlag = isempty(findstr(lower(currCase),'mars')) == 0 | isempty(findstr(lower(currCase),'venus')) == 0 |isempty(findstr(lower(currCase),'mercury'))  == 0 | isempty(findstr(lower(currCase),'moon'))  == 0 | isempty(findstr(lower(currCase),'pluto'))  == 0 | isempty(findstr(lower(currCase),'uranus'))  == 0 | isempty(findstr(lower(currCase),'neptune'))  == 0 | isempty(findstr(lower(currCase),'saturn'))  == 0;
                if nonEarthFlag == 1 & (isempty(findstr(lower(Tool1),'stk')) | isempty(findstr(lower(Tool2),'stk')))
                    maxDiffs2XL{size(maxDiffs2XL,1),20} = 'N/A'; % MHA not currently outputted in STK
                    maxDiffsAll(size(maxDiffsAll,1),20) = 0;
                    maxDiffs2XL{size(maxDiffs2XL,1),21} = 'N/A'; % LST not currently outputted in STK
                    maxDiffsAll(size(maxDiffsAll,1),21) = 0;
                end
                % *********
                
                [storeFilename,savefile] = saveData_2(maxDiffs,maxDiffsAll,currCase,Tool1Folder,Tool2Folder,DataDir,mat_Tool11,mat_Tool21,mat_header,diffMat_Tool1_Tool2,allLoopChoice,nameFile); % *** Function Call ***
                disp(' ');
                disp('Cb Comparison data has been saved in the following file:');
                disp(savefile);

                matchFound = 0;
            end
        if allFlag ~= 1
            break
        end
    end
    
    if (allFlag == 1) & (allLoopChoice == (menuSize - 1))
        saveLaTex_3(maxDiffs2XL,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,saveDir)

        % Add a maximum differences row to the end of maxDiffs2XL
        if size(maxDiffs2XL,1) > 2
            maxDiffs2XL = [maxDiffs2XL; [{'Maximum Differences'}, num2cell(max(maxDiffsAll(1:size(maxDiffsAll,1),2:size(maxDiffsAll,2)))*diag(unitConvert))]];
        end
        
        if runExcelFlag == 1
            saveExcel_3(runExcelFlag,e,eSheets,sheetExists,eWorkbooks,maxDiffs2XL,DataDir,Tool1,Tool2,Tool1Folder,Tool2Folder)
        end
    end
    
    cd(compareDir);
    disp(' ');
    disp(['Press any alpha-numeric key and then ENTER to rerun this comparison script or']);
    disp('Press ENTER to exit.');
    rerunScript = input('');
end