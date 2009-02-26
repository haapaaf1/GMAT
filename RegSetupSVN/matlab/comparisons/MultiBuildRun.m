%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      11/10/2005      Original
%   E.Dove      11/10/2005      Last Modified

clc
error = 1;

% While loop allows this script to run as many times as the user wants
rerunScript = 1;
while size(rerunScript,1) ~= 0
    clc
    clear all
    close all
    
    DataDir = ['C:\GMAT_results\CompareResults'];
    
%     try,
        rerunScript = 1;
        countScript = 1;

        disp('Welcome to the GMAT Multi-Build & Run a Single Script Program. Make sure GMAT is open and ');
        disp('the GMAT server has been started then press ENTER to continue.')
        pause

        OpenGMAT
        disp(' ');
        disp('Please enter the directory to search for scripts to run:');
        disp('(Example format - C:\example\directory )')
        scriptDir = input('','s');
        mFiles = dir([scriptDir,'\*.m']);
        numMs = size(mFiles,1);

        % Check for GMAT script files to be Built in the current directory
        if numMs ~= 0
            for loop=1:numMs
                scriptFiles{countScript,1} = mFiles(loop).name;
                countScript = countScript + 1;
            end
        else
            disp(' ');
            disp('There are no script files in the current directory. Check for script files');
            disp(['in the following folder: ',scriptDir]);
            disp('Then try re-running this script');
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
            if scriptChoice ~= [1:(scriptNum)]
                error = 1;
                disp('You have entered an incorrect value. Please try again.');
                scriptChoice = input('Choice: ');
                disp(' ');
            else
                break
            end
        end

        multiRuns = 5;

        for loop = 1:multiRuns
            % Run the GMAT Script and send to GMAT to Build and Run
            scriptLength = size(scriptFiles{scriptChoice},2);
            scriptLength2 = scriptLength - size('.m',2);
            run([scriptDir,'\',scriptFiles{scriptChoice}(1:scriptLength2)]);

            tic;
            BuildRunGMAT
            WaitForGMAT
            timeRun(loop,1) = toc;

            disp(['Run ',num2str(loop),' has completed!  Time to run: ', num2str(timeRun(loop,1)), ' seconds']);
            nameCase{loop,1} = scriptFiles{scriptChoice};
        end

        timeList=cat(2,nameCase,num2cell(timeRun));
        savefile = ['MultiRun',num2str(floor(now*100000)),'.mat'];
        warning off
        save([DataDir,'\',savefile],'timeList','-v6');
        warning on
        disp('');
        disp(['Total time to run all cases: ', num2str(sum(timeRun)),' seconds']);
        disp(['Average run time: ', num2str(sum(timeRun)/multiRuns),' seconds']);

        disp(' ');
        disp('Press any key and then ENTER to build and run another GMAT script or');
        disp('Press ENTER to exit.');
        rerunScript = input('');
        
%     catch,
%             disp(' ');
%             disp('An error has occured in the script. Check to make sure the GMAT server');
%             disp('is started and there are GMAT compatible scripts in this directory.');
%             input('Press ENTER to restart script');
%     end
end