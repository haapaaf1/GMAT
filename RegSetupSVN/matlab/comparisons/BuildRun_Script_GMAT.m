% BuildRun_Script_GMAT Builds & Runs GMAT scripts from within Matlab
%   BuildRun_Script_GMAT was designed to run multiple GMAT scripts with
%   little effort. This script should be run from the GMAT_scripts folder.
%
%   GMATs Matlab Server MUST BE STARTED BEFORE RUNNING THIS SCRIPT.
%
%      File folders used
%   [Main GMAT_results Directory]/GMAT_scripts 
%   [Main GMAT_results Directory]/[INSERT tool name here]_reports
%
%   INPUTS
%   All .m files located in the GMAT_scripts folder that follow the 
%   FDAB GMAT comparison naming convention:
%   [Tool]_[Trajectory]_[PointMasses]_[Non-SphericalGrav]_[Drag]_[OtherForces].m
%
%   OUTPUTS
%   When choosing to build multiple scripts and the all command is used
%       a mat file is created which keeps a log of the time it takes
%       to run each case.
%   Displays the case name and the time it takes for GMAT to build & run 
%       the file in Matlabs command window.
%   Other output data is dependent on the GMAT script
%
%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      07/20/2005      Original
%   E.Dove      09/19/2007      Last Modified
%   $Id: BuildRun_Script_GMAT.m,v 1.5 2007/09/19 21:49:15 edove Exp $
%
%   See also BUILDRUN_SCRIPT_GMAT, AUTOMATION_GMAT, COMPARISON_TOOL1_TOOL2_PV, COMPARISON_TOOL1_TOOL2_CS, COMPARISON_TOOL1_TOOL2_CS

clc
error = 1;

% While loop allows this script to run as many times as the user wants
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
    mainDir      = cd;
    DataDir      = [mainDir,'/output/AcceptTest/CompareResults/Performance'];
    inputDir     = [mainDir,'/input'];
    reportDir = [mainDir,'/output/Performance'];
    addpath(compareDir);
    addpath(inputDir);
    cd(inputDir);
    
%     try,
        rerunScript = 1;
        countScript = 1;
        folderCount = 0;

        disp('Welcome to the GMAT Build & Run Scripts Program. Make sure GMAT is open and ');
        disp('the GMAT server has been started then press ENTER to continue.')
        pause

        OpenGMAT
        
        disp(' ');
        disp('Choose the GMAT script set to Build & Run');
        disp('=========================================');
        temp = dir(inputDir);
        loopFolder = size(temp,1);
        
        % Search for folders that have GMAT script files.
        for loop = 1:loopFolder
            if (temp(loop).isdir == 1) & (strcmp(temp(loop).name, '.') ~= 1) & (strcmp(temp(loop).name, '..') ~= 1)
                %if strcmp(lower(temp(loop).name), 'cb') | strcmp(lower(temp(loop).name), 'cs') | strcmp(lower(temp(loop).name), 'integrator') | strcmp(lower(temp(loop).name), 'propagator') | strcmp(lower(temp(loop).name), 'freespace') | strcmp(lower(temp(loop).name), 'librationtests') | strcmp(lower(temp(loop).name), 'stopcond') | strcmp(lower(temp(loop).name), 'performance') | strcmp(lower(temp(loop).name), 'deltav')
                if (strcmp(lower(temp(loop).name), 'InlineMath') ~= 1) & (strcmp(lower(temp(loop).name), 'testing') ~= 1) & (strcmp(lower(temp(loop).name), 'multiruns') ~= 1) & (strcmp(lower(temp(loop).name), 'cvs') ~= 1) & (strcmp(lower(temp(loop).name), 'truthdata') ~= 1) & (strcmp(lower(temp(loop).name), 'optimizers') ~= 1)& (strcmp(lower(temp(loop).name), 'statereps') ~= 1)
                    folderCount = folderCount + 1;
                    validFolder{folderCount,1} = temp(loop).name;
                    disp([num2str(folderCount),'. ',validFolder{folderCount,1}]);
                end
            end           
        end
        folderMenu = size(validFolder,1); % Total number of options user can choose from
        
        % User inputs value. Program warns the user of any incorrectly entered value
        error = 1;
        folderChoice = input('Choice: ');
        disp(' ');
        while error == 1
            error = 2;
            while error == 2
                if size(folderChoice,1) ~= 0
                    break
                else
                    error = 2;
                    disp('You have entered an incorrect value. Please try again.');
                    folderChoice = input('Choice: ');
                    disp(' ');
                end
            end
            if folderChoice ~= [1:folderMenu]
                error = 1;
                disp('You have entered an incorrect value. Please try again.');
                folderChoice = input('Choice: ');
                disp(' ');
            else
                break
            end
        end

        folder = validFolder{folderChoice};
        mFiles = dir([inputDir,'/',folder,'/*.m']);
        numMs = size(mFiles,1);

        % Check for GMAT script files to be Built in the current directory
        if numMs ~= 0
            for loop=1:numMs
                if isempty(findstr(mFiles(loop).name,'GMAT_')) == 0
                    scriptFiles{countScript,1} = mFiles(loop).name;
                    countScript = countScript + 1;
                end
            end
        else
            disp(' ');
            disp('There are no m files in the current directory. Check for m files');
            disp(['in the following folder: ',inputDir,'/',folder]);
            disp('Then try re-running this script');
        end
        
        if exist('scriptFiles')
        else
            disp(' ');
            disp('There are no relevent m files in the current directory. Check for m files');
            disp(['in the following folder: ',inputDir,'/',folder]);
            disp('Press any key to restart the script.');
            pause
            continue
        end
        
        % Display menu
        disp(' ');
        disp('Choose the GMAT script to be Built and Run');
        disp('(Enter the number for the desired script)');
        disp('=========================================');

        scriptNum = size(scriptFiles,1); % Total number of options user can choose from
        for scriptLoop = 1:scriptNum
            disp([num2str(scriptLoop), '. ', scriptFiles{scriptLoop}]);
        end
        disp([num2str(scriptNum + 1),'. Build & Run multiple cases']);        
        
        % User inputs value. Program warns the user of any incorrectly entered value
        error = 1;
        scriptChoice = input('Choice: ');
        disp(' ');
        while error == 1
            error = 2;
            while error == 2
                if size(scriptChoice,1) ~= 0
                    break
                else
                    error = 2;
                    disp('You have entered an incorrect value. Please try again.');
                    scriptChoice = input('Choice: ');
                    disp(' ');
                end
            end
            if scriptChoice ~= [1:(scriptNum+1)]
                error = 1;
                disp('You have entered an incorrect value. Please try again.');
                scriptChoice = input('Choice: ');
                disp(' ');
            else
                break
            end
        end

        % Check to see if user wants to run multiple cases or just one.
        if scriptChoice == (scriptNum + 1)
            error = 1;
            while error == 1
                disp('Enter the numbers of the cases you wish to Build & Run in GMAT');
                disp('(Seperate numbers by commas/colons or type ''all'' to run all the cases)');
                disp('(i.e. 1:5 or all or 1,4,6:9 or 2:5,8,23 are all valid entries)');
                disp('========================================================================');
                multiInput      = input('','s');
                disp(' ');
                mulitInputSize  = size(multiInput,2);
                if strncmpi(multiInput, 'all',3)
                    selections = 1:scriptNum;
                    multiRuns  = scriptNum;
                    break
                elseif (size(str2num(multiInput),2) ~= 0) & isreal(str2num(multiInput))
                    selections = str2num(multiInput);
                    multiRuns  = size(str2num(multiInput),2);
                    break
                else
                    disp(' ');
                    disp('You have entered an incorrect value or made an error typing in ');
                    disp('multiple case numebers.');
                    disp(' ');
                end
            end
            for loop = 1:multiRuns
                % Run the GMAT Script and send to GMAT to Build and Run
                scriptLength = size(scriptFiles{selections(loop)},2);
                scriptLength2 = scriptLength - size('.m',2);
                run([folder,'/',scriptFiles{selections(loop)}(1:scriptLength2)]);
                tic;
                BuildRunGMAT
                WaitForGMAT
                
                timeRun(loop,1) = toc;
                disp(['Case ',num2str(selections(loop)),' has been run!  Time to run: ', num2str(timeRun(loop,1)/60), ' minutes']);
                ClearGMAT
            end
            if strncmpi(multiInput, 'all',3)
                timeList=cat(2,scriptFiles,num2cell(timeRun/60));
                timeHeader = {'Test Case','Time (minutes)'};
                if strcmp(lower(folder), 'propagator')
                    savefile = [date,'_Time2RunAll.mat'];
                else 
                    savefile = [date,'_Time2RunAll_',folder,'.mat'];
                end
                warning off
                save([DataDir,'/',savefile],'timeList','timeHeader','-v6');
                warning on
                disp('');
                disp(['Total time to run all cases: ', num2str(sum(timeRun)/60),' minutes']);
                
                if strcmp(lower(folder), 'performance')
                    disp(' ');
                    disp(['Please wait. Performance test cases need to be built & ran multiple times.']);
                    disp(['The process will be complete approx. ',num2str((sum(timeRun)/60)*4),' minutes from now']);
                    for performanceLoop = 2:5
                        clear timeList timeHeader
                        for loop = 1:multiRuns
                            % Run the GMAT Script and send to GMAT to Build and Run
                            scriptLength = size(scriptFiles{selections(loop)},2);
                            scriptLength2 = scriptLength - size('.m',2);
                            run([folder,'/',scriptFiles{selections(loop)}(1:scriptLength2)]);
                            tic;
                            BuildRunGMAT
                            WaitForGMAT
                            timeRun(loop,1) = toc;
                            ClearGMAT
                            
%                             if performanceLoop == 5
                                % Save amount of integrator step sizes
                                % Report style must have ephem step size checked and the propagator must report
                                % out in integrator step size
                                if isempty(strfind(scriptFiles{selections(loop)}(1:scriptLength2),'_output')) == 0
                                    fid = fopen([reportDir,'/',scriptFiles{selections(loop)}(1:scriptLength2),'.report']);
                                    currentLine = fgetl(fid);
                                    countLine = 1;
                                    while 1
                                        currentLine = fgetl(fid);
                                        if currentLine == -1
                                            break
                                        else
                                            countLine = countLine + 1;
                                        end
                                    end
                                    countLine = countLine - 1; % Subtract amount of lines that make up header
                                    intStep(loop-1,1) = countLine; % For _noOutput report files
                                    intStep(loop,1) = countLine; % For _output report files
                                end
%                             end
                        end
                        disp(['The process will be complete approx. ',num2str((sum(timeRun)/60)*(5-performanceLoop)),' minutes from now']);
                        timeList=cat(2,scriptFiles,num2cell(timeRun/60),num2cell(intStep));
                        timeHeader = {'Test Case','Time (minutes)', 'Integrator Steps'};
                        savefile = [date,'_Time2RunAll_',folder,'_',num2str(performanceLoop),'.mat'];
                        warning off
                        save([DataDir,'/',savefile],'timeList','timeHeader','-v6');
                        warning on
                    end
                end
            end
            
            disp(' ');
            disp('Press any key and then ENTER to build and run another GMAT script or');
            disp('Press ENTER to exit.');
            rerunScript = input('');
        else
            % Run the GMAT Script and send to GMAT to Build and Run
            scriptLength = size(scriptFiles{scriptChoice},2);
            scriptLength2 = scriptLength - 2;
            run([folder,'/',scriptFiles{scriptChoice}(1:scriptLength2)]);
            tic;
            BuildRunGMAT
            WaitForGMAT
            timeRun = toc;
            ClearGMAT
            
            disp(['Total time to run selected case: ', num2str(timeRun/60),' minutes']);
            disp(' ');
            disp('Press any key and then ENTER to build and run another GMAT script or');
            disp('Press ENTER to exit.');
            rerunScript = input('');            
        end
        
%     catch,
%         if isempty(findstr(str2num(multiInput),(scriptNum+1))) ~= 1
%             disp(' ');
%             disp(['You have entered the build & run multiple cases choice (', num2str(scriptNum + 1),')']);
%             disp('when asked to enter the multiple cases.');
%             disp(' ');
%             disp('Press any key and then ENTER to build and run another GMAT script or');
%             disp('Press ENTER to exit.');
%             rerunScript = input('');
%         else
%             disp(' ');
%             disp('An error has occured in the script. Check to make sure the GMAT server');
%             disp('is started and there are GMAT compatible scripts in this directory.');
%             input('Press ENTER to restart script');
%         end
%     end
end