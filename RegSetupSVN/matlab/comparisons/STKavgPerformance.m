%   STKavgPerformance averages all performance runs from the STK_Repropagate
%   function into one easy to manage data structure
%
%      File folders used
%   [Main CVS Repository Directory]/GMAT_RegSetup/output/CompareResults/Performance
%
%   INPUTS 
%   Matlab .mat file with the propagation time of each satellite.
%
%   OUTPUTS
%   Cell array of averaged time values for each case
%
%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      03/30/2006      Original
%   E.Dove      09/18/2007      Last Modified
%   $Id: STKavgPerformance.m,v 1.4 2007/12/14 22:16:55 edove Exp $

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
addpath(compareDir);

% ===================  Initialize variables ========================
cd(compareDir);
DataDir = [mainDir,'/output/AcceptTest/CompareResults/Performance'];
warning off
mkdir(DataDir); % Creates a directory to save excel and matlab comparison data
warning on

rePropNum = 5; % Amount of time the Performance test cases were repropagated
STKtimesRows = 0; % Counter to keep track of the amount of STK rows
trajCount = 0;
STKtimes = {};

% ########## Display the menu for times of test cases run ############
availTimes = dir([DataDir,'/*STK*Time4All_Performance_',num2str(rePropNum),'.mat']);
if size(availTimes,1) == 0
    disp('There are no files with a file suffix of:')
    disp(['*STK*Time4All_Performance_',num2str(rePropNum),'.mat'])
    return
end
timeMenu = size(availTimes,1); % Total number of options user can choose from
for timeLoop = 1:timeMenu
    chars(timeLoop) = findstr(availTimes(timeLoop).name,'_Time4All_Performance') - 1;
    disp([num2str(timeLoop), '. ', availTimes(timeLoop).name(1:chars(timeLoop))]);
end

% User inputs value. Program warns the user of any incorrectly entered value
error = 1;
disp('====================================================================================');
timeChoice = input('Choice: ');
disp(' ');
while error == 1
    error = 2;
    while error == 2
        if size(timeChoice,1) ~= 0
            break
        else
            error = 2;
            disp('You have entered an incorrect value. Please try again.');
            timeChoice = input('Choice: ');
            disp(' ');
        end
    end
    if timeChoice ~= [1:timeMenu]
        error = 1;
        disp('You have entered an incorrect value. Please try again.');
        timeChoice = input('Choice: ');
        disp(' ');
    else
        break
    end
end

Time1 = availTimes(timeChoice).name(1:chars(timeChoice));
dateLoc = findstr('_STK',Time1)-1;

availTimes2 = dir([DataDir,'/',Time1(1:dateLoc),'*Time4All_Performance*']);
totalTrajs = size(availTimes2,1)/rePropNum;
for loop = 1:totalTrajs
    currTraj = {};
    findMat = findstr('.mat',availTimes2((loop-1)*5+5).name);
    open([DataDir,'/',availTimes2((loop-1)*rePropNum+rePropNum).name(1:findMat-3),'_',num2str(rePropNum),'.mat']);
    timeList{loop} = ans.timeList;
    timeHeader{loop} = ans.timeHeader;

    testCaseNum = size(timeList{loop},1); % Total number of test cases, including repetitions
    for trajloop = 1:testCaseNum
        findTraj = findstr(timeList{loop}{trajloop,1},'_');
        findDot = findstr(timeList{loop}{trajloop,1},'.');
        currTraj{trajloop,1} = timeList{loop}{trajloop,1}((findTraj(2)+1):findDot); % Obtain the name of the current trajectory
    end

    % Group similar cases together
    groupTraj{1,1} = currTraj{1,1};
    groupTraj{1,2} = 0;
    newTrajCount = 1;
    compareLoop = 1;

    for outerLoop = 1:testCaseNum
        if compareLoop == outerLoop
            groupTraj{newTrajCount,2} = size(strmatch(groupTraj{newTrajCount,1},currTraj,'exact'),1);
            compareLoop = compareLoop + groupTraj{newTrajCount,2};
            if compareLoop > size(currTraj,1)
                break
            end
            newTrajCount = newTrajCount + 1;
            groupTraj{newTrajCount,1} = currTraj{compareLoop,1};
            groupTraj{newTrajCount,2} = 0;
        end
    end
    avgLoopEnd = sum(cell2mat(groupTraj(:,2))/rePropNum); % Total number of unrepeated test cases for current group

    % Store the row location of each test case from the timeList variable
    for loopTraj = 1:size(groupTraj,1)
        groupTraj{loopTraj,3} = strfind(timeList{loop}(:,1),groupTraj{loopTraj,1});
        groupTraj{loopTraj,4} = [];
        for repropLocLoop = 1:testCaseNum
            if isempty(groupTraj{loopTraj,3}{repropLocLoop,1}) == 0
                groupTraj{loopTraj,4} = [groupTraj{loopTraj,4} repropLocLoop];
            end
        end
    end

    groupTrajFin = sortrows(groupTraj,1);

    % Average the values of the repeated test cases and save times and filename
    for avgLoop = 1:avgLoopEnd
        STKtimes(STKtimesRows+avgLoop,1) = timeList{loop}(groupTrajFin{avgLoop,4}(1),1);
        temp = cell2mat(timeList{loop}(groupTrajFin{avgLoop,4},2)); % Store all values for current test case for mean
        STKtimes(STKtimesRows+avgLoop,2) = {mean(temp)*60};
        STKtimes(STKtimesRows+avgLoop,3) = timeList{loop}(groupTrajFin{avgLoop,4}(1),3);
    end
    STKtimesRows = STKtimesRows + avgLoopEnd;
    clear groupTrajFin groupTraj
end