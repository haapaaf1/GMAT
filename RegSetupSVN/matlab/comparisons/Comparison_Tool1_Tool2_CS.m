% Comparison_Tool1_Tool2_CS Compares GMATs Coordinate System (CS) parameters between two tools.
%   Comparison_Tool1_Tool2_CS was designed to allow the user to select two programs 
%   to compare to one another. The comparison involves taking the difference 
%   of the variables listed in the INPUT section of this help text. 
%     For each case that is compared the maximum absolute value of the 
%   diferences are computed and sent to a mat file. This script should be run 
%   from the GMAT_scripts folder.
%
%      File folders used
%   [Main GMAT_results Directory]/GMAT_scripts 
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports/[Tool1]_[Tool2]
%
%   INPUTS 
%   The report files/input must be formatted the same way. 
%   The columns are ordered as followed:
%   Time, [X,Y,and Z] Position(km), [X,Y,and Z] Velocity(km/sec),
%   Mag. of Velocity(km/sec), Right Ascension of Velocity(deg), 
%   [X,Y,and Z] RxV-Specific Angular Momentum(km^2/sec), Arg. of Perigee(deg),
%   Declination(deg), Declination of Velocity(deg), Inclination(deg), 
%   Right Ascension(deg), Right Ascension of Ascending Node(deg)
% 
%   The data must be seperated by spaces. The report files must also follow 
%   the following naming convention: 
%   CSParams_[Tool]_[Trajectory]_2Body_[CoordinateSystem].report
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
%   $Id: Comparison_Tool1_Tool2_CS.m,v 1.3 2007/05/04 20:51:43 edove Exp $
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      09/01/2005      Original
%   E.Dove      05/04/2007      Last Modified
%   E.Dove      05/06/2009      Changed folder to save Latex files to
%
%   See also BUILDRUN_SCRIPT_GMAT, AUTOMATION_GMAT, COMPARISON_TOOL1_TOOL2_PV, COMPARISON_TOOL1_TOOL2_CB

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
    AcceptTestDir = [cvsRootDir,'/docs/AcceptTest'];
    
    addpath(genpath([mainDir,'/matlab']));
    inputDir = [mainDir,'/input'];
    outputTestDir = [mainDir,'/output/AcceptTest'];
    cd(compareDir);
        
    disp('Welcome to the Tool1-Tool2 Coordinate System (CS) Report Comparison Program.');
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
    nameFile        = 'CSParams';
    countReport     = 0;
    matchFound      = 0;
    mat_header      = {'time', 'X-Pos (km)', 'Y-Pos (km)', 'Z-Pos (km)', 'X-Vel (km/s)', 'Y-Vel (km/s)', 'Z-Vel (km/s)', 'Mag-Vel (km/s)', 'Right Asc. of Vel. (deg)','X-(Pos._x_Vel.) (km^2/sec)','Y-(Pos._x_Vel.) (km^2/sec)','Z-(Pos._x_Vel.) (km^2/sec)', 'Arg. of Per. (deg)', 'Dec. (deg)', 'Dec. Vel. (deg)', 'Inclination (deg)', 'RA (deg)', 'RAAN (deg)'};
    columnSize      = size(mat_header,2);
    matchChoicesT1  = [];
    matchChoicesT2  = [];
    maxDiffsAll     = zeros(1,columnSize);
    maxDiffs2XL     = {'Test Case', 'X-Pos (m)', 'Y-Pos (m)', 'Z-Pos (m)', 'X-Vel (m/s)', 'Y-Vel (m/s)', 'Z-Vel (m/s)', 'Mag-Vel (m/s)', 'Right Asc. of Vel. (deg)','X-(Pos._x_Vel.) (m^2/sec)','Y-(Pos._x_Vel.) (m^2/sec)','Z-(Pos._x_Vel.) (m^2/sec)', 'Arg. of Per. (deg)', 'Dec. (deg)', 'Dec. Vel. (deg)', 'Inclination (deg)', 'RA (deg)', 'RAAN (deg)'};
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
                [Tool21_rows,diffMat_Tool1_Tool2,mat_Tool21,mat_Tool11] = readData(reportChoiceTool1,columnSize,rowStartTool1,rowStartTool2,Tool1Dir,Tool2Dir,reportFilesTool1,reportFilesTool2,reportChoiceTool2,matchChoicesT1,matchChoicesT2,Tool2Folder,Tool1Folder); % *** Function Call ***
                if isempty(diffMat_Tool1_Tool2)
                    return
                end
                
                [maxDiffs,diffMat_Tool1_Tool2] = find360nAbs(diffMat_Tool1_Tool2, Tool21_rows); % *** Function Call ***
                
                reportLengthTool2 = size(reportFilesTool2{reportChoiceTool2}) - charLoc1Tool2;
                currCase = currTool1choice(1:(reportLengthTool2(2)-size('report',2)));
                unitConvert = ones(1,columnSize-1); % Variable to convert all meter units to kilometers
                unitConvert(1:7) = 1000;
                unitConvert(9:11) = 1000000;
                maxDiffs2XL = [maxDiffs2XL; [{currCase}, num2cell(maxDiffs(2:columnSize)*diag(unitConvert))]]; 
                maxDiffsAll(allLoopChoice,:) = maxDiffs;
                
                [storeFilename,savefile] = saveData_2(maxDiffs,maxDiffsAll,currCase,Tool1Folder,Tool2Folder,DataDir,mat_Tool11,mat_Tool21,mat_header,diffMat_Tool1_Tool2,allLoopChoice,nameFile); % *** Function Call ***
                disp(' ');
                disp('CS Comparison data has been saved in the following file:');
                disp(savefile);

                matchFound = 0;
            end
        if allFlag ~= 1
            break
        end
    end
    
    if (allFlag == 1) & (allLoopChoice == (menuSize - 1))
        saveLaTex_2(maxDiffs2XL,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,saveDir);

        % Add a maximum differences row to the end of maxDiffs2XL
        if size(maxDiffs2XL,1) > 2
            maxDiffs2XL = [maxDiffs2XL; [{'Maximum Differences'}, num2cell(max(cell2mat(maxDiffs2XL(2:size(maxDiffs2XL,1),2:size(maxDiffs2XL,2)))))]];
        end
        
        if runExcelFlag == 1
            saveExcel_2(runExcelFlag,e,eSheets,sheetExists,eWorkbooks,maxDiffs2XL,DataDir,Tool1,Tool2,Tool1Folder,Tool2Folder)
        end
    end
    
    cd(compareDir);
    disp(' ');
    disp(['Press any alpha-numeric key and then ENTER to rerun this comparison script or']);
    disp('Press ENTER to exit.');
    rerunScript = input('');
end