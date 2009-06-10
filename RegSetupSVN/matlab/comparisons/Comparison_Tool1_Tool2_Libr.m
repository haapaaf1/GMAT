% Comparison_Tool1_Tool2_Libr compares the position & velocity differences between tools.
%   Comparison_Tool1_Tool2_Libr was designed to allow the user to select two programs 
%   to compare to one another. The comparison involves taking the difference 
%   of the position and velocity vector and then normalizing these two 
%   vectors to get the position and velocity error. 
%     For each case that is compared the maximum pos. and vel. error is 
%   displayed on the screen, sent to a mat file, and Latex file. This script 
%   should be run from the GMAT_scripts folder.
%
%      File folders used
%   [Main GMAT_results Directory]/GMAT_scripts
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports/[Tool1]_[Tool2]
%   [Main GMAT_results Directory]/LaTex_Docs
%
%   INPUTS 
%   The report files/input data must be formatted the same way. The
%   first column is time, second-fourth columns are the pos. vector (x,y,z), 
%   and fifth-seventh columns are the vel. vector (x,y,z). The data must be
%   seperated by spaces. The report files must also follow the FDAB GMAT 
%   comparison naming convention: 
%   [Tool]_[Trajectory]_[PointMasses]_[Non-SphericalGrav]_[Drag]_[OtherForces].report
%
%   OUTPUTS
%   LaTex documents containing the comparison results in table format.
%   Excel document w/ the current date in the filename and comparison results.
%   Matlab .mat file with the data for both tools, the comparison, and 
%       a header variable to know what each column is.
%
%   REFERENCES
%   Steve Hughes' Latex table generation script [called to from script]  
%   and Kevin Berry's Excel data exportation script [embedded into script].
%
%   REVISION HISTORY
%   $Id: Comparison_Tool1_Tool2_Libr.m,v 1.3 2007/05/04 20:51:43 edove Exp $
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      03/06/2006      Original
%   E.Dove      05/04/2007      Last Modified
%   E.Dove      05/06/2009      Changed folder to save Latex files to
% 
%   See also BUILDRUN_SCRIPT_GMAT, AUTOMATION_GMAT, COMPARISON_TOOL1_TOOL2_CS, COMPARISON_TOOL1_TOOL2_CB

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
    
    disp('Welcome to the Tool1-Tool2 Libration Point Report Comparison Program.');
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
    nameFile        = 'LibrationTest';
    countReport     = 0;
    matchFound      = 0;
    mat_header      = {'time', 'X-Pos', 'Y-Pos', 'Z-Pos', 'X-Vel', 'Y-Vel', 'Z-Vel'};
    columnSize      = size(mat_header,2);
    groupTraj       = {0};
    countTraj       = 0;
    countTrajRun    = 0;
    countMatches    = 1;
    matchChoicesT1  = [];
    matchChoicesT2  = [];
    maxNormAll      = [0 0 0];
    maxNorm2XL      = {'Test Case', 'Position Difference (m)', 'Velocity Difference (m/s)'};
    ColumnLabels    = {'Libration Point' 'Position Difference(m)' 'Velocity Difference(m/s)'}.';
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
    fprintf([Tool1Folder,' and ',Tool2Folder,' Comparison Results \t\t\t Date: ', date ,' \n\n']);
    fprintf('Filename with comparison data                                  Largest Position Diff. (m)   Largest Velocity Diff. (m/s)\n');
    fprintf('(matrix_tool1_tool2_traj_pmgrav_nsgrav_drag_altforces)\n');
    fprintf('------------------------------------------------------------   ---------------------------  ----------------------------\n');
        
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
                    [normMat_Tool1_Tool2,maxNorm,storePosError,storeVelError,currCase] = normNmaxData(Tool21_rows,diffMat_Tool1_Tool2,columnSize,allLoopChoice,reportChoiceTool2,charLoc1Tool2,reportFilesTool2,currTool1choice); % *** Function Call ***
                    
                    maxNorm2XL = [maxNorm2XL; {currCase, maxNorm(1,2) * 1000, maxNorm(1,3) * 1000}];
                    
                    [storeFilename] = saveData_4(currCase,Tool1Folder,Tool2Folder,DataDir,mat_Tool11,mat_Tool21,mat_header,normMat_Tool1_Tool2,allLoopChoice,nameFile);  % *** Function Call ***
                    fprintf('%60s %20.7f  %28.9f\n',storeFilename{allLoopChoice},storePosError{allLoopChoice},storeVelError{allLoopChoice});
                end
                % ======== Format data for exporting into a LaTeX document =========
                % Create a variable which stores the trajectory name
                % and number of test cases for that trajectory.
                
                if allFlag == 1
                    if matchFound == 1
                        findTraj = findstr(reportFilesTool2{reportChoiceTool2},'_');
                        findDot =  findstr(reportFilesTool2{reportChoiceTool2},'.');
                        currTraj = reportFilesTool2{reportChoiceTool2}((findTraj(2)+1):(findDot(size(findDot,2))-1)); % Obtain the name of the current trajectory
                    end
                    
                    if groupTraj{1,1} == 0
                        countTraj               = 1;
                        groupTraj{countTraj,1}  = currTraj;
                    elseif (groupTraj{countTraj,1} ~= 0) & (strcmp(groupTraj{countTraj,1},currTraj) == 0)
                        saveFile     = [Tool1Folder,'-',Tool2Folder,'-',groupTraj{countTraj,1},'.tex'];
                        [countMatches,groupTraj] = saveLaTex_5(ColumnLabels,countTrajRun,groupTraj,countTraj,maxNorm2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,countMatches,saveFile,saveDir,diffMat_Tool1_Tool2); % *** Function Call ***

                        countTraj               = countTraj + 1;
                        groupTraj{countTraj,1}  = currTraj;
                        countTrajRun            = 0;
                    elseif (groupTraj{countTraj,1} ~= 0) & (allLoopChoice == (menuSize-1))
                        countTrajRun = countTrajRun + 1;
                        saveFile     = [Tool1Folder,'-',Tool2Folder,'-',groupTraj{countTraj,1},'.tex'];
                        if matchFound == 1
                            [countMatches,groupTraj] = saveLaTex_5(ColumnLabels,countTrajRun,groupTraj,countTraj,maxNorm2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,countMatches,saveFile,saveDir,diffMat_Tool1_Tool2); % *** Function Call ***
                            allLoopChoice = allLoopChoice + 1;
                        else
                            countTrajRun = countTrajRun - 1;
                            [countMatches,groupTraj] = saveLaTex_5(ColumnLabels,countTrajRun,groupTraj,countTraj,maxNorm2XL,nameFile,mainDir,Tool1Folder,Tool2Folder,outputTestDir,Tool2Dir,countMatches,saveFile,saveDir,diffMat_Tool1_Tool2); % *** Function Call ***
                            allLoopChoice = allLoopChoice + 1;
                        end
                        if runExcelFlag == 1
                            saveExcel_4(runExcelFlag,e,eSheets,sheetExists,eWorkbooks,maxNorm2XL,DataDir,Tool1,Tool2,Tool1Folder,Tool2Folder,nameFile); % *** Function Call ***
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