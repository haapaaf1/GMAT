% Comparison_DeltaV Compares GMAT's DeltaV test case results to each other or the exact solution.
%
%   REVISION HISTORY
%   $Id: Comparison_DeltaV.m,v 1.3 2007/05/04 20:51:43 edove Exp $
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      04/05/2006      Original
%   E.Dove      05/04/2007      Last Modified
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
    AcceptTestDir = [cvsRootDir,'/GMATDocuments/AcceptTest'];
    
    addpath(genpath([mainDir,'/matlab']));
    inputDir = [mainDir,'/input'];
    outputTestDir = [mainDir,'/output/AcceptTest'];
    cd(compareDir);

    disp('Welcome to the GMAT Delta V Report Comparison Program.');
    disp('Press ENTER to continue.');
    pause
    
    [runExcelFlag] = askExcel(); % *** Function Call ***
    cd(outputTestDir);    
    [Tool1Folder,Tool2Folder] = dispToolMenu_2(); % *** Function Call ***
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
    nameFile        = 'DeltaV';
    countReport     = 0;
    matchFound      = 0;
    mat_header      = {'Time (A1MJD)', 'X-Pos (km)', 'Y-Pos (km)', 'Z-Pos (km)', 'X-Vel (km/s)', 'Y-Vel (km/s)', 'Z-Vel (km/s)'};
    columnSize      = size(mat_header,2);
    groupTraj       = {0};
    countTraj       = 0;
    countTrajRun    = 0;
    countMatches    = 1;
    matchChoicesT1  = [];
    matchChoicesT2  = [];
    maxDiffs2XL      = {'time', 'X-Pos (m)', 'Y-Pos (m)', 'Z-Pos (m)', 'X-Vel (m/s)', 'Y-Vel (m/s)', 'Z-Vel (m/s)'};
    maxDiffsAll      = zeros(size(maxDiffs2XL,2));
    ColumnLabels    = {'Test Case' 'X-Pos (m)', 'Y-Pos (m)', 'Z-Pos (m)', 'X-Vel (m/s)', 'Y-Vel (m/s)', 'Z-Vel (m/s)'}.';
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
        
        while (matchFound == 1) | (allLoopChoice == (menuSize-1))
            if matchFound == 1
                [Tool21_rows,diffMat_Tool1_Tool2,mat_Tool21,mat_Tool11] = readData(reportChoiceTool1,columnSize,rowStartTool1,rowStartTool2,Tool1Dir,Tool2Dir,reportFilesTool1,reportFilesTool2,reportChoiceTool2,matchChoicesT1,matchChoicesT2,Tool2Folder,Tool1Folder); % *** Function Call ***
                if isempty(diffMat_Tool1_Tool2)
                    return
                end

                [maxDiffs,diffMat_Tool1_Tool2] = find360nAbs(diffMat_Tool1_Tool2,Tool21_rows); % *** Function Call ***

                reportLengthTool2 = size(reportFilesTool2{reportChoiceTool2}) - charLoc1Tool2;
                currCase = currTool1choice(1:(reportLengthTool2(2)-size('report',2)));

                maxDiffs2XL = [maxDiffs2XL; [{currCase}, num2cell(maxDiffs(2:columnSize)*1000)]]; % Store values for export to Excel and Latex. Convert km to m.

                maxDiffsAll(allLoopChoice,:) = maxDiffs;
                
                [storeFilename,savefile] = saveData_2(maxDiffs,maxDiffsAll,currCase,Tool1Folder,Tool2Folder,DataDir,mat_Tool11,mat_Tool21,mat_header,diffMat_Tool1_Tool2,allLoopChoice,nameFile); % *** Function Call ***
                disp(' ');
                disp('StopCond Comparison data has been saved in the following file:');
                disp(savefile);
            end
            % ======== Format data for exporting into a LaTeX document =========
            % Create a variable which stores the trajectory name
            % and number of test cases for that trajectory.

            if allFlag == 1
                if matchFound == 1
                    findTraj = findstr(reportFilesTool2{reportChoiceTool2},'_');
                    currTraj = reportFilesTool2{reportChoiceTool2}((findTraj(2)+1):(findTraj(3)-1)); % Obtain the name of the current trajectory
                end
                
                if groupTraj{1,1} == 0
                    countTraj               = 1;
                    groupTraj{countTraj,1}  = currTraj;
                elseif (groupTraj{countTraj,1}(1,1) ~= 0) & (strcmp(groupTraj{countTraj,1},currTraj) == 0)
                    saveFile     = [Tool1Folder,'-',Tool2Folder,'-',nameFile,'-',groupTraj{countTraj,1},'.tex'];
                    [countMatches,groupTraj] = saveLaTex_1(ColumnLabels,countTrajRun,groupTraj,countTraj,maxDiffs2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,countMatches,saveFile,saveDir); % *** Function Call ***
                    
                    countTraj               = countTraj + 1;
                    groupTraj{countTraj,1}  = currTraj;
                    countTrajRun            = 0;
                elseif (groupTraj{countTraj,1}(1,1) ~= 0) & (allLoopChoice == (menuSize-1))
                    countTrajRun = countTrajRun + 1;
                    saveFile     = [Tool1Folder,'-',Tool2Folder,'-',nameFile,'-',groupTraj{countTraj,1},'.tex'];
                    if matchFound == 1
                        [countMatches,groupTraj] = saveLaTex_1(ColumnLabels,countTrajRun,groupTraj,countTraj,maxDiffs2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,countMatches,saveFile,saveDir); % *** Function Call ***
                        allLoopChoice = allLoopChoice + 1;
                    else
                        countTrajRun = countTrajRun - 1;
                        [countMatches,groupTraj] = saveLaTex_1(ColumnLabels,countTrajRun,groupTraj,countTraj,maxDiffs2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,countMatches,saveFile,saveDir); % *** Function Call ***
                        allLoopChoice = allLoopChoice + 1;
                    end
                    
                    if runExcelFlag == 1
                        saveExcel_4(runExcelFlag,e,eSheets,sheetExists,eWorkbooks,maxDiffs2XL,DataDir,Tool1,Tool2,Tool1Folder,Tool2Folder,nameFile); % *** Function Call ***
                    end
                end
                countTrajRun = countTrajRun + 1;
            end
            matchFound = 0;
        end
        if allFlag ~= 1
            break
        end
    end

    cd(compareDir);
    disp(' ');
    disp(['Press any alpha-numeric key and then ENTER to rerun this comparison script or']);
    disp('Press ENTER to exit.');
    rerunScript = input('');
end
